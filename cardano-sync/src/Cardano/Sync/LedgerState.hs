{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync.LedgerState
  ( CardanoLedgerState (..)
  , LedgerEnv (..)
  , LedgerStateSnapshot (..)
  , LedgerStateFile (..)
  , applyBlock
  , deleteNewerLedgerStateFiles
  , hashToAnnotation
  , listLedgerStateFilesOrdered
  , loadLedgerStateAtPoint
  , loadLedgerStateFromFile
  , mkLedgerEnv
  , saveLedgerState
  ) where

import           Cardano.Binary (DecoderError)
import qualified Cardano.Binary as Serialize

import           Cardano.Sync.Config.Types
import qualified Cardano.Sync.Era.Cardano.Util as Cardano
import qualified Cardano.Sync.Era.Shelley.Generic.EpochUpdate as Generic
import qualified Cardano.Sync.Era.Shelley.Generic.Rewards as Generic
import           Cardano.Sync.Types hiding (CardanoBlock)
import           Cardano.Sync.Util

import           Cardano.Prelude
import           Cardano.Slotting.Block (BlockNo (..))

import           Cardano.Slotting.EpochInfo (EpochInfo, epochInfoEpoch)
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..), WithOrigin (..), fromWithOrigin)

import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Control.Exception as Exception
import           Control.Monad.Extra (firstJustM, fromMaybeM)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.List as List
import qualified Data.Text as Text

import           Ouroboros.Consensus.Block (CodecConfig, WithOrigin (..), blockHash, blockPrevHash,
                   withOrigin)
import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Consensus.Cardano.Block (HardForkState (..), LedgerState (..))
import           Ouroboros.Consensus.Cardano.CanHardFork ()
import           Ouroboros.Consensus.Config (TopLevelConfig (..), configCodec, configLedger)
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import           Ouroboros.Consensus.HardFork.Combinator.State (epochInfoLedger)
import qualified Ouroboros.Consensus.HeaderValidation as Consensus
import           Ouroboros.Consensus.Ledger.Abstract (ledgerTipHash, ledgerTipPoint, ledgerTipSlot,
                   tickThenReapply)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerCfg (..), ExtLedgerState (..))
import qualified Ouroboros.Consensus.Ledger.Extended as Consensus
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import qualified Ouroboros.Consensus.Shelley.Protocol as Consensus
import           Ouroboros.Consensus.Storage.Serialisation (DecodeDisk (..), EncodeDisk (..))

import           Ouroboros.Network.Block (HeaderHash, Point (..))
import qualified Ouroboros.Network.Point as Point

import qualified Shelley.Spec.Ledger.API.Protocol as Shelley
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.STS.Tickn as Shelley

import           System.Directory (listDirectory, removeFile)
import           System.FilePath (dropExtension, takeExtension, (</>))

-- Note: The decision on whether a ledger-state is written to disk is based on the block number
-- rather than the slot number because while the block number is fully populated (for every block
-- other then genesis with number N there exists a block with number N - 1) whereas in the Shelley
-- era, only about 1/20 slots are occupied with blocks.
-- However, rollbacks are specified using a Point (basically a tuple of SlotNo and hash) and
-- therefore ledger states are stored in files with the SlotNo and hash in the file name.

{- HLINT ignore "Reduce duplication" -}

data LedgerEnv = LedgerEnv
  { leProtocolInfo :: !(Consensus.ProtocolInfo IO CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Shelley.Network
  , leStateVar :: TVar CardanoLedgerState
  }

topLevelConfig :: LedgerEnv -> TopLevelConfig CardanoBlock
topLevelConfig = Consensus.pInfoConfig . leProtocolInfo

newtype CardanoLedgerState = CardanoLedgerState
  { clsState :: ExtLedgerState CardanoBlock
  }

data LedgerStateFile = LedgerStateFile
  { lsfSlotNo :: !SlotNo
  , lsfHash :: !ByteString
  , lsfFilePath :: !FilePath
  } deriving Show

data LedgerStateSnapshot = LedgerStateSnapshot
  { lssState :: !CardanoLedgerState
  , lssEpochUpdate :: !(Maybe Generic.EpochUpdate) -- Only Just for a single block at the epoch boundary
  }

mkLedgerEnv :: Consensus.ProtocolInfo IO CardanoBlock
            -> LedgerStateDir
            -> Shelley.Network
            -> SlotNo
            -> Bool
            -> IO LedgerEnv
mkLedgerEnv protocolInfo dir network slot deleteFiles = do
  when deleteFiles $
    deleteNewerLedgerStateFiles dir slot
  st <- findLatestLedgerState protocolInfo dir deleteFiles
  var <- newTVarIO st
  pure LedgerEnv
    { leProtocolInfo = protocolInfo
    , leDir = dir
    , leNetwork = network
    , leStateVar = var
    }


initCardanoLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> CardanoLedgerState
initCardanoLedgerState pInfo = CardanoLedgerState
      { clsState = Consensus.pInfoInitLedger pInfo
      }

-- The function 'tickThenReapply' does zero validation, so add minimal validation ('blockPrevHash'
-- matches the tip hash of the 'LedgerState'). This was originally for debugging but the check is
-- cheap enough to keep.
applyBlock :: LedgerEnv -> CardanoBlock -> IO LedgerStateSnapshot
applyBlock env blk =
    -- 'LedgerStateVar' is just being used as a mutable variable. There should not ever
    -- be any contention on this variable, so putting everything inside 'atomically'
    -- is fine.
    atomically $ do
      oldState <- readTVar (leStateVar env)
      let !newState = oldState { clsState = applyBlk (ExtLedgerCfg (topLevelConfig env)) blk (clsState oldState) }
      writeTVar (leStateVar env) newState
      pure $ LedgerStateSnapshot
                { lssState = newState
                , lssEpochUpdate =
                    if ledgerEpochNo env newState == ledgerEpochNo env oldState + 1
                      then ledgerEpochUpdate env (clsState newState)
                             (ledgerRewardUpdate env (ledgerState $ clsState oldState))
                      else Nothing
                }
  where
    applyBlk
        :: ExtLedgerCfg CardanoBlock -> CardanoBlock
        -> ExtLedgerState CardanoBlock
        -> ExtLedgerState CardanoBlock
    applyBlk cfg block lsb =
      case tickThenReapplyCheckHash cfg block lsb of
        Left err -> panic err
        Right result -> result

-- Delete ledger state files for slots later than the provided SlotNo.
deleteNewerLedgerStateFiles :: LedgerStateDir -> SlotNo -> IO ()
deleteNewerLedgerStateFiles stateDir slotNo = do
    delFiles <- filter isNewer <$> listLedgerStateFilesOrdered stateDir
    mapM_ (safeRemoveFile . lsfFilePath) delFiles
  where
    isNewer :: LedgerStateFile -> Bool
    isNewer lsf = lsfSlotNo lsf > slotNo

saveCurrentLedgerState :: LedgerEnv -> IO ()
saveCurrentLedgerState env = do
    ledger <- readTVarIO (leStateVar env)
    case mkLedgerStateFilename (leDir env) ledger of
      Origin -> pure () -- we don't store genesis
      At file -> LBS.writeFile file $
        Serialize.serializeEncoding $
          Consensus.encodeExtLedgerState
             (encodeDisk codecConfig)
             (encodeDisk codecConfig)
             (encodeDisk codecConfig)
             (clsState ledger)
  where
    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec (topLevelConfig env)

saveLedgerState :: LedgerEnv -> LedgerStateSnapshot -> SyncState -> IO ()
saveLedgerState env snapshot synced = do
  writeLedgerState env ledger
  case synced of
    SyncFollowing -> saveCleanupState                          -- If following, save every state.
    SyncLagging
      | block `mod` 2000 == 0 -> saveCleanupState              -- Only save state ocassionally.
      | isJust (lssEpochUpdate snapshot) -> saveCleanupState   -- Epoch boundaries cost a lot, so we better save them
      | otherwise -> pure ()
  where
    block :: Word64
    block = withOrigin 0 unBlockNo $ ledgerTipBlockNo ledger

    ledger :: ExtLedgerState CardanoBlock
    ledger = clsState $ lssState snapshot

    saveCleanupState :: IO ()
    saveCleanupState = do
      saveCurrentLedgerState env
      cleanupLedgerStateFiles env $
        fromWithOrigin (SlotNo 0) (ledgerTipSlot . ledgerState $ ledger)

findLatestLedgerState :: Consensus.ProtocolInfo IO CardanoBlock -> LedgerStateDir -> Bool -> IO CardanoLedgerState
findLatestLedgerState pInfo dir deleteFiles =
    fromMaybeM (pure $ initCardanoLedgerState pInfo)
               (findLatestLedgerStateDisk config dir deleteFiles)
  where
    config = Consensus.pInfoConfig pInfo

findLatestLedgerStateDisk :: TopLevelConfig CardanoBlock -> LedgerStateDir -> Bool -> IO (Maybe CardanoLedgerState)
findLatestLedgerStateDisk config dir deleteFiles = do
  files <- listLedgerStateFilesOrdered dir
  firstJustM (loadLedgerStateFromFile config deleteFiles) files

loadLedgerStateAtPoint :: LedgerEnv -> CardanoPoint -> IO ()
loadLedgerStateAtPoint env point = do
  -- Load the state
  mState <- loadState env point
  case mState of
    Right st -> writeLedgerState env (clsState st)
    Left file -> panic $ "loadLedgerStateAtPoint failed to find required state file: "
                        <> Text.pack (lsfFilePath file)

mkLedgerStateFilename :: LedgerStateDir -> CardanoLedgerState -> WithOrigin FilePath
mkLedgerStateFilename dir ledger = lsfFilePath . dbPointToFileName dir
    <$> getPoint (ledgerTipPoint (Proxy @CardanoBlock) (ledgerState $ clsState ledger))

hashToAnnotation :: ByteString -> ByteString
hashToAnnotation = Base16.encode . BS.take 5

dbPointToFileName :: LedgerStateDir -> Point.Block SlotNo (HeaderHash CardanoBlock) -> LedgerStateFile
dbPointToFileName (LedgerStateDir stateDir) (Point.Block slot hash) =
    LedgerStateFile
      { lsfSlotNo = slot
      , lsfHash = shortHash
      , lsfFilePath = stateDir </> show (unSlotNo slot) ++ "-" ++ BS.unpack shortHash ++ ".lstate"
      }
  where
    shortHash :: ByteString
    shortHash = hashToAnnotation $ toRawHash (Proxy @CardanoBlock) hash

parseLedgerStateFileName :: LedgerStateDir -> FilePath -> Maybe LedgerStateFile
parseLedgerStateFileName (LedgerStateDir stateDir) fp =
    case break (== '-') (dropExtension fp) of
      (slot, '-':hash) -> build (BS.pack hash) <$> readMaybe slot
      _otherwise -> Nothing
  where
    build :: ByteString -> Word64 -> LedgerStateFile
    build hash slot =
      LedgerStateFile
        { lsfSlotNo = SlotNo slot
        , lsfHash = hash
        , lsfFilePath = stateDir </> fp
        }

-- | This should be exposed by 'consensus'.
ledgerTipBlockNo :: ExtLedgerState blk -> WithOrigin BlockNo
ledgerTipBlockNo = fmap Consensus.annTipBlockNo . Consensus.headerStateTip . Consensus.headerState

-- -------------------------------------------------------------------------------------------------

cleanupLedgerStateFiles :: LedgerEnv -> SlotNo -> IO ()
cleanupLedgerStateFiles env slotNo = do
    files <- listLedgerStateFilesOrdered (leDir env)
    let (invalid, valid) = partitionEithers $ map keepFile files
    -- Remove invalid (ie SlotNo >= current) ledger state files (occurs on rollback).
    mapM_ safeRemoveFile invalid
    -- Remove all but 8 most recent state files.
    mapM_ (safeRemoveFile . lsfFilePath) (List.drop 8 valid)
  where
    -- Left files are deleted, Right files are kept.
    keepFile :: LedgerStateFile ->  Either FilePath LedgerStateFile
    keepFile lsf@(LedgerStateFile s _ fp) =
      if s <= slotNo
        then Right lsf
        else Left fp

extractEpochNonce :: ExtLedgerState CardanoBlock -> Maybe Shelley.Nonce
extractEpochNonce extLedgerState =
    case Consensus.headerStateChainDep (headerState extLedgerState) of
      ChainDepStateByron _ -> Nothing
      ChainDepStateShelley st -> Just $ extractNonce st
      ChainDepStateAllegra st -> Just $ extractNonce st
      ChainDepStateMary st -> Just $ extractNonce st
  where
    extractNonce :: Consensus.TPraosState crypto -> Shelley.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . Consensus.tpraosStateChainDepState

-- loadLedgerStateFromFile :: LedgerEnv -> LedgerStateFile -> IO (Maybe CardanoLedgerState)
-- loadLedgerStateFromFile env = loadLedgerStateFromFile (topLevelConfig env) False

loadState :: LedgerEnv -> CardanoPoint -> IO (Either LedgerStateFile CardanoLedgerState)
loadState env point = case dbPointToFileName (leDir env) <$> getPoint point of
  At file -> maybe (Left file) Right <$> loadLedgerStateFromFile (topLevelConfig env) False file
  Origin -> pure $ Right $ initCardanoLedgerState (leProtocolInfo env)

loadLedgerStateFromFile :: TopLevelConfig CardanoBlock -> Bool -> LedgerStateFile -> IO (Maybe CardanoLedgerState)
loadLedgerStateFromFile config delete lsf = do
    mst <- safeReadFile (lsfFilePath lsf)
    case mst of
      Nothing -> when delete (safeRemoveFile $ lsfFilePath lsf) >> pure Nothing
      Just st -> pure . Just $ CardanoLedgerState { clsState = st }
  where
    safeReadFile :: FilePath -> IO (Maybe (ExtLedgerState CardanoBlock))
    safeReadFile fp = do
      mbs <- Exception.try $ BS.readFile fp
      case mbs of
        Left (_ :: IOException) -> pure Nothing
        Right bs ->
          case decode bs of
            Left _err -> do
              safeRemoveFile fp
              pure Nothing
            Right ls -> pure $ Just ls

    codecConfig :: CodecConfig CardanoBlock
    codecConfig = configCodec config

    decode :: ByteString -> Either DecoderError (ExtLedgerState CardanoBlock)
    decode =
      Serialize.decodeFullDecoder
          "Ledger state file"
          (Consensus.decodeExtLedgerState
            (decodeDisk codecConfig)
            (decodeDisk codecConfig)
            (decodeDisk codecConfig))
        . LBS.fromStrict

-- Get a list of the ledger state files order most recent
listLedgerStateFilesOrdered :: LedgerStateDir -> IO [LedgerStateFile]
listLedgerStateFilesOrdered dir = do
    files <- filter isLedgerStateFile <$> listDirectory (unLedgerStateDir dir)
    pure . List.sortBy revSlotNoOrder $ mapMaybe (parseLedgerStateFileName dir) files
  where
    isLedgerStateFile :: FilePath -> Bool
    isLedgerStateFile fp = takeExtension fp == ".lstate"

    revSlotNoOrder :: LedgerStateFile -> LedgerStateFile -> Ordering
    revSlotNoOrder a b = compare (lsfSlotNo b) (lsfSlotNo a)

writeLedgerState :: LedgerEnv -> ExtLedgerState CardanoBlock -> IO ()
writeLedgerState env st = atomically $ writeTVar (leStateVar env) (CardanoLedgerState st)

-- | Remove given file path and ignore any IOEXceptions.
safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fp = handle (\(_ :: IOException) -> pure ()) $ removeFile fp

ledgerEpochNo :: LedgerEnv -> CardanoLedgerState -> EpochNo
ledgerEpochNo env cls =
    case ledgerTipSlot (ledgerState (clsState cls)) of
      Origin -> 0 -- An empty chain is in epoch 0
      NotOrigin slot -> runIdentity $ epochInfoEpoch epochInfo slot
  where
    epochInfo :: EpochInfo Identity
    epochInfo = epochInfoLedger (configLedger $ topLevelConfig env) (hardForkLedgerStatePerEra . ledgerState $ clsState cls)

-- Create an EpochUpdate from the current epoch state and the rewards from the last epoch.
ledgerEpochUpdate :: LedgerEnv -> ExtLedgerState CardanoBlock -> Maybe Generic.Rewards -> Maybe Generic.EpochUpdate
ledgerEpochUpdate env els mRewards =
  case ledgerState els of
    LedgerStateByron _ -> Nothing
    LedgerStateShelley sls -> Just $ Generic.shelleyEpochUpdate (leNetwork env) sls mRewards mNonce
    LedgerStateAllegra als -> Just $ Generic.allegraEpochUpdate (leNetwork env) als mRewards mNonce
    LedgerStateMary mls -> Just $ Generic.maryEpochUpdate (leNetwork env) mls mRewards mNonce
  where
    mNonce :: Maybe Shelley.Nonce
    mNonce = extractEpochNonce els

-- This will return a 'Just' from the time the rewards are updated until the end of the
-- epoch. It is 'Nothing' for the first block of a new epoch (which is slightly inconvenient).
ledgerRewardUpdate :: LedgerEnv -> LedgerState CardanoBlock -> Maybe Generic.Rewards
ledgerRewardUpdate env lsc =
    case lsc of
      LedgerStateByron _ -> Nothing -- This actually happens during the Byron era.
      LedgerStateShelley sls -> Generic.shelleyRewards (leNetwork env) sls
      LedgerStateAllegra als -> Generic.allegraRewards (leNetwork env) als
      LedgerStateMary mls -> Generic.maryRewards (leNetwork env) mls

-- Like 'Consensus.tickThenReapply' but also checks that the previous hash from the block matches
-- the head hash of the ledger state.
tickThenReapplyCheckHash
    :: ExtLedgerCfg CardanoBlock -> CardanoBlock
    -> ExtLedgerState CardanoBlock
    -> Either Text (ExtLedgerState CardanoBlock)
tickThenReapplyCheckHash cfg block lsb =
  if blockPrevHash block == ledgerTipHash (ledgerState lsb)
    then Right $ tickThenReapply cfg block lsb
    else Left $ mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , textShow (unSlotNo $ fromWithOrigin (SlotNo 0) (ledgerTipSlot $ ledgerState lsb))
                  , " hash ", renderByteArray (Cardano.unChainHash (ledgerTipHash $ ledgerState lsb))
                  , " but block previous hash is "
                  , renderByteArray (Cardano.unChainHash $ blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray (BSS.fromShort . Consensus.getOneEraHash $ blockHash block), "."
                  ]
