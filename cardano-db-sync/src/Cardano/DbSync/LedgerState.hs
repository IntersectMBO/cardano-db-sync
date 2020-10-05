{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.LedgerState
  ( CardanoLedgerState (..)
  , LedgerStateSnapshot (..)
  , LedgerStateVar (..)
  , applyBlock
  , initLedgerStateVar
  , listLedgerStateSlotNos
  , loadLedgerState
  , readLedgerState
  , saveLedgerState
  ) where

import qualified Cardano.Binary as Serialize

import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import           Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), fromWithOrigin)

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import           Control.Exception (IOException, handle)
import qualified Control.Exception as Exception
import           Control.Monad.Extra (firstJustM)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Either (partitionEithers)
import qualified Data.List as List

import           Ouroboros.Consensus.Block (CodecConfig, WithOrigin (..), blockSlot, blockPrevHash)
import           Ouroboros.Consensus.Byron.Ledger (initByronLedgerState)
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Cardano.Block (LedgerState (LedgerStateByron, LedgerStateShelley))
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Ledger.Abstract (LedgerConfig, ledgerTipHash, ledgerTipSlot,
                    tickThenApply, tickThenReapply)
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra (initHardForkState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           System.Directory (listDirectory, removeFile)
import           System.FilePath ((</>), dropExtension, takeExtension)

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(LedgerState CardanoBlock)
  , clsConfig :: !(LedgerConfig CardanoBlock)
  , clsCodec :: !(CodecConfig CardanoBlock)
  }

newtype LedgerStateVar = LedgerStateVar
  { unLedgerStateVar :: TVar CardanoLedgerState
  }

data LedgerStateFile = LedgerStateFile -- Internal use only.
  { lsfSlotNo :: !Word64
  , lsfFilePath :: !FilePath
  }

data LedgerStateSnapshot = LedgerStateSnapshot
  { lssState :: !CardanoLedgerState
  , lssRewardUpdate :: !(Maybe (Shelley.RewardUpdate StandardShelley))
  , lssParamUpdate :: !(Maybe (Shelley.PParams StandardShelley))
  }

initLedgerStateVar :: GenesisConfig -> IO LedgerStateVar
initLedgerStateVar genesisConfig = do
  LedgerStateVar <$>
    case genesisConfig of
      GenesisCardano _ byronConfig _ -> do
        let topConfig = mkTopLevelConfig genesisConfig
        newTVarIO $
          CardanoLedgerState
            { clsState = HardForkLedgerState $ initHardForkState (initByronLedgerState byronConfig Nothing)
            , clsConfig = topLevelConfigLedger topConfig
            , clsCodec = topLevelConfigCodec topConfig
            }

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: LedgerStateVar -> CardanoBlock -> IO LedgerStateSnapshot
applyBlock (LedgerStateVar stateVar) blk =
    -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
    -- be any contention on this variable, so putting everything inside 'atomically'
    -- is fine.
    atomically $ do
      oldState <- readTVar stateVar
      if ledgerTipHash (clsState oldState) == blockPrevHash blk
        then do
          let !newState = oldState { clsState = applyBlk (clsConfig oldState) blk (clsState oldState) }
          writeTVar stateVar newState
          let mRewards =
                  case (ledgerRewardUpdate (clsState newState), ledgerRewardUpdate (clsState oldState)) of
                    (Nothing, Just r) -> Just r
                    _otherwise -> Nothing
              mParams =
                  if ledgerEpochNo newState == ledgerEpochNo oldState + 1
                    then ledgerEpochProtocolParams (clsState newState)
                    else Nothing

          pure $ LedgerStateSnapshot
                    { lssState = newState
                    , lssRewardUpdate = mRewards
                    , lssParamUpdate = mParams
                    }
        else panic $ mconcat
               [ "applyBlock: Hash mismatch when applying block with slot no ", textShow (blockSlot blk), "\n"
               , "applyBlock: ", textShow (ledgerTipHash $ clsState oldState), " /= ", textShow (blockPrevHash blk)
               ]
  where
    applyBlk :: LedgerConfig CardanoBlock -> CardanoBlock -> LedgerState CardanoBlock -> LedgerState CardanoBlock
    applyBlk cfg block lsb =
      -- Set to False to get better error messages from Consensus (but slower block application).
      if True
        then tickThenReapply cfg block lsb
        else case runExcept $ tickThenApply cfg block lsb of
                Left err -> panic $ textShow err
                Right result -> result

saveLedgerState :: LedgerStateDir -> LedgerStateVar -> CardanoLedgerState -> SyncState -> IO ()
saveLedgerState lsd@(LedgerStateDir stateDir) (LedgerStateVar stateVar) ledger synced = do
  atomically $ writeTVar stateVar ledger
  case synced of
    SyncFollowing -> saveState                      -- If following, save every state.
    SyncLagging
      | unSlotNo slot == 0 -> pure ()               -- Genesis and the first EBB are weird so do not store them.
      | unSlotNo slot `mod` 10000 == 0 -> saveState -- Only save state ocassionally.
      | otherwise -> pure ()
  where
    filename :: FilePath
    filename = stateDir </> show (unSlotNo slot) ++ ".lstate"

    slot :: SlotNo
    slot = fromWithOrigin (SlotNo 0) (ledgerTipSlot $ clsState ledger)

    saveState :: IO ()
    saveState = do
      -- Encode and write lazily.
      LBS.writeFile filename $
        Serialize.serializeEncoding (encodeDisk (clsCodec ledger) $ clsState ledger)
      cleanupLedgerStateFiles lsd slot

loadLedgerState :: LedgerStateDir -> LedgerStateVar -> SlotNo -> IO ()
loadLedgerState stateDir (LedgerStateVar stateVar) slotNo = do
  -- Read current state to get the LedgerConfig and CodecConfig.
  lstate <- readLedgerState (LedgerStateVar stateVar)
  -- Load the state
  mState <- loadState stateDir lstate slotNo
  case mState of
    Nothing -> pure ()
    Just st -> atomically $ writeTVar stateVar st

-- -------------------------------------------------------------------------------------------------

-- Find the ledger state files and keep the 4 most recent.
cleanupLedgerStateFiles :: LedgerStateDir -> SlotNo -> IO ()
cleanupLedgerStateFiles stateDir slotNo = do
    files <- listLedgerStateFilesOrdered stateDir
    let (invalid, valid) = partitionEithers $ map keepFile files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    mapM_ safeRemoveFile invalid
    -- Remove all but 8 most recent state files.
    mapM_ safeRemoveFile $ map lsfFilePath (List.drop 8 valid)
  where
    -- Left files are deleted, Right files are kept.
    keepFile :: LedgerStateFile ->  Either FilePath LedgerStateFile
    keepFile lsf@(LedgerStateFile w fp) =
      if SlotNo w <= slotNo
        then Right lsf
        else Left fp

loadState :: LedgerStateDir -> CardanoLedgerState -> SlotNo -> IO (Maybe CardanoLedgerState)
loadState stateDir ledger slotNo = do
    files <- listLedgerStateFilesOrdered stateDir
    let (invalid, valid) = partitionEithers $ map keepFile files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    mapM_ safeRemoveFile invalid
    -- Want the highest numbered snapshot.
    firstJustM loadFile valid
  where
    -- Left files are deleted, Right files are kept.
    keepFile :: LedgerStateFile ->  Either FilePath LedgerStateFile
    keepFile lsf@(LedgerStateFile w fp) =
      if SlotNo w <= slotNo
        then Right lsf
        else Left fp

    loadFile :: LedgerStateFile -> IO (Maybe CardanoLedgerState)
    loadFile lsf = do
      mst <- safeReadFile (lsfFilePath lsf)
      case mst of
        Nothing -> pure Nothing
        Just st -> pure . Just $ ledger { clsState = st }

    safeReadFile :: FilePath -> IO (Maybe (LedgerState CardanoBlock))
    safeReadFile fp = do
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (_ :: IOException) -> pure Nothing
        Right bs -> pure $ decode bs

    decode :: ByteString -> Maybe (LedgerState CardanoBlock)
    decode =
      either (const Nothing) Just
        . Serialize.decodeFullDecoder "Ledger state file" (decodeDisk (clsCodec ledger))
        . LBS.fromStrict

-- Get a list of the ledger state files order most recent
listLedgerStateFilesOrdered :: LedgerStateDir -> IO [LedgerStateFile]
listLedgerStateFilesOrdered (LedgerStateDir stateDir) = do
    files <- filter isLedgerStateFile <$> listDirectory stateDir
    pure . List.sortBy revSlotNoOrder $ map extractIndex files
  where
    isLedgerStateFile :: FilePath -> Bool
    isLedgerStateFile fp = takeExtension fp == ".lstate"

    extractIndex :: FilePath -> LedgerStateFile
    extractIndex fp =
      case readMaybe (dropExtension fp) of
        Nothing -> LedgerStateFile 0 fp -- Should never happen.
        Just w -> LedgerStateFile w (stateDir </> fp)

    revSlotNoOrder :: LedgerStateFile -> LedgerStateFile -> Ordering
    revSlotNoOrder a b = compare (lsfSlotNo b) (lsfSlotNo a)

listLedgerStateSlotNos :: LedgerStateDir -> IO [SlotNo]
listLedgerStateSlotNos = fmap3 (SlotNo . lsfSlotNo) listLedgerStateFilesOrdered

readLedgerState :: LedgerStateVar -> IO CardanoLedgerState
readLedgerState (LedgerStateVar stateVar) =
  atomically $ readTVar stateVar

-- | Remove given file path and ignore any IOEXceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

ledgerEpochNo :: CardanoLedgerState -> EpochNo
ledgerEpochNo cls =
    case ledgerTipSlot (clsState cls) of
      Origin -> 0 -- An empty chain is in epoch 0
      NotOrigin slot -> runIdentity $ epochInfoEpoch epochInfo slot
  where
    epochInfo :: EpochInfo Identity
    epochInfo = epochInfoLedger (clsConfig cls) $ hardForkLedgerStatePerEra (clsState cls)

ledgerEpochProtocolParams :: LedgerState CardanoBlock -> Maybe (Shelley.PParams StandardShelley)
ledgerEpochProtocolParams lsc =
  case lsc of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ Shelley.esPp (Shelley.nesEs $ Consensus.shelleyLedgerState sls)

ledgerRewardUpdate :: LedgerState CardanoBlock -> Maybe (Shelley.RewardUpdate StandardShelley)
ledgerRewardUpdate lsc =
  case lsc of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Shelley.strictMaybeToMaybe . Shelley.nesRu
                                $ Consensus.shelleyLedgerState sls
