{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Interpreter
  ( Interpreter (..)
  , CardanoBlock
  , MockBlock (..)
  , NodeId (..)
  , initInterpreter
  , withInterpreter
  , finalizeFingerprint
  , forgeNextFindLeader
  , forgeNext
  , forgeNextAfter
  , getCurrentState
  , getCurrentEpoch
  , getCurrentSlot
  , forgeWithStakeCreds
  , withAlonzoLedgerState
  , withShelleyLedgerState
  , mkTxId
  ) where

import           Cardano.Prelude (bimap, getField)

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.Aeson
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (NS (S, Z))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word (Word64)
import           GHC.Generics
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           System.Directory

import           Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Cardano.Block (AlonzoEra,
                   LedgerState (LedgerStateAllegra, LedgerStateAlonzo, LedgerStateMary, LedgerStateShelley),
                   ShelleyEra, StandardCrypto)
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Mempool as Consensus
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock, shelleyLedgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Consensus
import qualified Ouroboros.Consensus.TypeFamilyWrappers as Consensus
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()

import           Cardano.Ledger.Alonzo.Tx
import qualified Cardano.Ledger.Shelley.API.Mempool as SL
import           Cardano.Ledger.Shelley.LedgerState (NewEpochState (..))
import qualified Cardano.Ledger.TxIn as SL

import           Cardano.Mock.ChainDB
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import qualified Cardano.Mock.Forging.Tx.Shelley as Shelley
import           Cardano.Mock.Forging.Types

data Interpreter = Interpreter
  { iForging :: Map Int (BlockForging IO CardanoBlock)
  , iState :: StrictMVar IO InterpreterState
  , iTracerForge :: Tracer IO (ForgeStateInfo CardanoBlock)
  , iTopLeverConfig :: TopLevelConfig CardanoBlock
  , iFingerMode :: FingerprintMode
  , iFingerFile :: FilePath
  }

data InterpreterState = InterpreterState
  { isChain :: !(ChainDB CardanoBlock)
  , isForecast :: !(Forecast (LedgerView (BlockProtocol CardanoBlock)))
  , isSlot :: !SlotNo
  -- ^ The first slot to try the next block
  , isNextBlockNo :: !BlockNo
  -- ^ the block number of the block to be forged
  , isFingerprint :: Fingerprint
  -- ^ newest first list of slots where blocks were succesfully produced.
  } deriving Generic
    deriving NoThunks via OnlyCheckWhnfNamed "InterpreterState" InterpreterState

-- | Vrf and leader election is pseudo-random.
-- Running a simulation multiple times, will always give the same result.
-- So after the first simulation, we can reuse the slots, stored in some file
-- to avoid costly executions.
-- 'SearchSlots' mode starts from an empty list of slots and progressively
-- fills the list. Eventually the list is dumbed to the 'FilePath'.
-- 'ValidateSlots' mode starts from the list of slots and validates that the given
-- leader can indeed forge the next slot every time.
data FingerprintMode = SearchSlots FilePath | ValidateSlots

newtype Fingerprint = Fingerprint [Word64]
  deriving (Generic, FromJSON, ToJSON)

mkFingerprint :: FilePath -> IO (FingerprintMode, Fingerprint)
mkFingerprint path = do
  thereIsFile <- doesPathExist path
  if thereIsFile then do
    mfingerPrint <- eitherDecodeFileStrict path
    fingerPrint <- either (throwIO . FingerprintDecodeError) pure mfingerPrint
    -- we need to reverse since this is newest first
    pure (ValidateSlots, fingerPrint)
  else
    pure (SearchSlots path, emptyFingerprint)

isSearchingMode :: FingerprintMode -> Bool
isSearchingMode (SearchSlots _) = True
isSearchingMode _ = False

-- | Given the current slot, return the slot to test and the next 'Fingerprint'
getFingerTipSlot :: Interpreter -> Fingerprint -> SlotNo -> Either ForgingError SlotNo
getFingerTipSlot interpreter fingerprint currentSlotNo = case iFingerMode interpreter of
  SearchSlots _
    -> Right currentSlotNo
  ValidateSlots | Just slotNo <- fst <$> unconsFingerprint fingerprint
    -> Right slotNo
  ValidateSlots -> Left $ EmptyFingerprint currentSlotNo (iFingerFile interpreter)

addSlot :: Fingerprint -> SlotNo -> Fingerprint
addSlot (Fingerprint slots) slot = Fingerprint (unSlotNo slot : slots)

unconsFingerprint :: Fingerprint -> Maybe (SlotNo, Fingerprint)
unconsFingerprint (Fingerprint slots) =
  bimap SlotNo Fingerprint <$> List.uncons slots

lengthSlots :: Fingerprint -> Int
lengthSlots (Fingerprint slots) = length slots

emptyFingerprint :: Fingerprint
emptyFingerprint = Fingerprint []

reverseFingerprint :: Fingerprint -> Fingerprint
reverseFingerprint (Fingerprint slots) = Fingerprint $ reverse slots

finalizeFingerprint :: Interpreter -> IO ()
finalizeFingerprint inter = do
    interState <- readMVar $ iState inter
    case iFingerMode inter of
      SearchSlots fp -> do
        encodeFile fp $ reverseFingerprint $ isFingerprint interState
      ValidateSlots -> pure ()

deriving instance Generic (ChainDB CardanoBlock)

deriving instance NoThunks (Forecast a)

deriving instance Generic (Forecast a)

initInterpreter :: ProtocolInfo IO CardanoBlock
                -> Tracer IO (ForgeStateInfo CardanoBlock)
                -> FilePath
                -> IO Interpreter
initInterpreter pinfo traceForge fingerprintFile = do
  forging <- pInfoBlockForging pinfo
  let topLeverCfg = pInfoConfig pinfo
  let initSt = pInfoInitLedger pinfo
  let ledgerView = mkForecast topLeverCfg initSt
  (mode, fingerprint) <- mkFingerprint fingerprintFile
  let initState = InterpreterState
        { isChain = initChainDB topLeverCfg initSt
        , isForecast = ledgerView
        , isSlot = SlotNo 0
        , isNextBlockNo = BlockNo 0
        , isFingerprint = fingerprint
        }
  stvar <- newMVar initState
  pure $ Interpreter
    { iForging = Map.fromList $ zip [0..] forging
    , iState = stvar
    , iTracerForge = traceForge
    , iTopLeverConfig = topLeverCfg
    , iFingerMode = mode
    , iFingerFile = fingerprintFile
    }

withInterpreter :: ProtocolInfo IO CardanoBlock
                -> Tracer IO (ForgeStateInfo CardanoBlock)
                -> FilePath
                -> (Interpreter -> IO a)
                -> IO a
withInterpreter p t f action = do
  interpreter <- initInterpreter p t f
  a <- action interpreter
  finalizeFingerprint interpreter
  pure a

addOrValidateSlot :: Interpreter
                  -> Fingerprint
                  -> CardanoBlock
                  -> Either ForgingError Fingerprint
addOrValidateSlot inter fingerprint blk =
  case iFingerMode inter of
    SearchSlots _ -> Right $ addSlot fingerprint (blockSlot blk)
    ValidateSlots -> case unconsFingerprint fingerprint of
      Nothing -> Left $ EmptyFingerprint (blockSlot blk) (iFingerFile inter)
      Just (slotNo, fingerPrint') | slotNo == blockSlot blk
        -> Right fingerPrint'
      Just (slotNo, fingerPrint')
        -- The validation here is unecessary, since we have used the slot to
        -- forge the block. But we do it nontheless as a sanity check.
        -> Left $ NotExpectedSlotNo (blockSlot blk) slotNo (lengthSlots fingerPrint')

forgeWithStakeCreds :: Interpreter -> IO CardanoBlock
forgeWithStakeCreds inter = do
    st <- getCurrentState inter
    tx <- case ledgerState st of
      LedgerStateShelley sts -> either throwIO (pure . TxShelley) $ Shelley.mkDCertTxPools sts
      LedgerStateAlonzo sta -> either throwIO (pure . TxAlonzo) $ Alonzo.mkDCertTxPools sta
      _ -> throwIO UnexpectedEra
    forgeNextFindLeader inter [tx]

forgeNextAfter :: Interpreter -> Word64 -> [TxEra] -> IO CardanoBlock
forgeNextAfter interpreter skipSlots txs' = do
    modifyMVar (iState interpreter) $ \st ->
      pure (st {isSlot = isSlot st + SlotNo skipSlots}, ())
    forgeNextFindLeader interpreter txs'

forgeNextFindLeader :: Interpreter -> [TxEra] -> IO CardanoBlock
forgeNextFindLeader interpreter txs' =
    forgeNextLeaders interpreter txs' $ Map.elems (iForging interpreter)

forgeNext :: Interpreter -> MockBlock -> IO CardanoBlock
forgeNext interpreter testBlock = do
    case Map.lookup (unNodeId $ node testBlock) (iForging interpreter) of
      Nothing -> throwIO $ NonExistantNode (node testBlock)
      Just forging -> forgeNextLeaders interpreter (txs testBlock) [forging]

forgeNextLeaders :: Interpreter -> [TxEra] -> [BlockForging IO CardanoBlock] -> IO CardanoBlock
forgeNextLeaders interpreter txs' possibleLeaders = do
        interState <- readMVar $ iState interpreter
        (blk, fingerprint) <- tryOrValidateSlot interState possibleLeaders
        let !chain' = extendChainDB (isChain interState) blk
        let !newSt = currentState chain'
        let newInterState = InterpreterState
              { isChain = chain'
              , isForecast = mkForecast cfg newSt
              , isSlot = blockSlot blk + 1
              , isNextBlockNo = blockNo blk + 1
              , isFingerprint = fingerprint
              }
        _ <- swapMVar (iState interpreter) newInterState
        pure blk
  where
    cfg = iTopLeverConfig interpreter

    tryOrValidateSlot :: InterpreterState
                      -> [BlockForging IO CardanoBlock]
                      -> IO (CardanoBlock, Fingerprint)
    tryOrValidateSlot interState blockForgings = do
      currentSlot <- throwLeft $
        getFingerTipSlot interpreter (isFingerprint interState) (isSlot interState)
      trySlots interState blockForgings 0 currentSlot (isSearchingMode (iFingerMode interpreter))

    trySlots :: InterpreterState
             -> [BlockForging IO CardanoBlock]
             -> Int
             -> SlotNo
             -> Bool
             -> IO (CardanoBlock, Fingerprint)
    trySlots interState blockForgings numberOfTries currentSlot searching = do
      let callFailed = if searching
            then trySlots interState blockForgings (numberOfTries + 1) (currentSlot + 1) searching
            else throwIO $ FailedToValidateSlot currentSlot (lengthSlots (isFingerprint interState)) (iFingerFile interpreter)

          callSuccedded blk = do
            fingerprint' <- throwLeft $
                addOrValidateSlot interpreter (isFingerprint interState) blk
            pure (blk, fingerprint')

      when (numberOfTries > 140) (throwIO WentTooFar)

      -- We require the ticked ledger view in order to construct the ticked
      -- 'ChainDepState'.
      ledgerView :: Ticked (LedgerView (BlockProtocol CardanoBlock)) <-
        case runExcept (forecastFor (isForecast interState) currentSlot) of
          Left err ->
            -- This can only happen if we cross an epoch boundary
            throwIO $ ForecastError currentSlot err
          Right lv -> pure lv

          -- Tick the 'ChainDepState' for the 'SlotNo' we're producing a block
          -- for. We only need the ticked 'ChainDepState' to check the whether
          -- we're a leader. This is much cheaper than ticking the entire
          -- 'ExtLedgerState'.

      let tickedChainDepState :: Ticked (ChainDepState (BlockProtocol CardanoBlock))
          !tickedChainDepState =
              tickChainDepState
                (configConsensus cfg)
                ledgerView
                currentSlot
                (headerStateChainDep (headerState $ currentState $ isChain interState))

      let tryAllForging [] = throwIO $ userError "found empty forgers"
          tryAllForging (forging : rest) = do
            !shouldForge <- checkShouldForge forging
                              (iTracerForge interpreter)
                              cfg
                              currentSlot
                              tickedChainDepState
            case shouldForge of
              ShouldForge proof -> pure $ Just (proof, forging)
              _ | not (null rest) -> tryAllForging rest
              ForgeStateUpdateError err -> do
                Text.putStrLn $ Text.unwords
                  ["TraceForgeStateUpdateError", textShow currentSlot, textShow err]
                pure Nothing
              CannotForge cannotForge -> do
                Text.putStrLn $ Text.unwords
                  ["TraceNodeCannotForge", textShow currentSlot, textShow cannotForge]
                pure Nothing
              NotLeader ->
                pure Nothing

      mproof <- tryAllForging blockForgings
      case mproof of
        Nothing -> callFailed
        Just (proof, blockForging) -> do
          -- Tick the ledger state for the 'SlotNo' we're producing a block for
          let tickedLedgerSt :: Ticked (LedgerState CardanoBlock)
              !tickedLedgerSt =
                applyChainTick
                  (configLedger cfg)
                  currentSlot
                  (ledgerState $ currentState $ isChain interState)
          let txs'' = mkValidated <$> txs'
          !blk <- Block.forgeBlock blockForging
            cfg
            (isNextBlockNo interState)
            currentSlot
            tickedLedgerSt
            txs''
            proof
          callSuccedded blk

    -- We will probably not use it and wait for ledger to provide a way to construct
    -- Validated Tx in an unsafe way.
    _applyTxs :: [Consensus.GenTx CardanoBlock]
             -> SlotNo
             -> TickedLedgerState CardanoBlock
             -> Either (ApplyTxErr CardanoBlock) [Validated (GenTx CardanoBlock)]
    _applyTxs genTxs slotNo st =
      runExcept $ forM genTxs $ \tx ->
        snd <$> applyTx
                  (topLevelConfigLedger cfg)
                  Intervene
                  slotNo
                  tx
                  st

getCurrentState :: Interpreter -> IO (ExtLedgerState CardanoBlock)
getCurrentState inter = do
    interState <- readMVar (iState inter)
    pure $ currentState $ isChain interState

getCurrentEpoch :: Interpreter -> IO EpochNo
getCurrentEpoch inter = do
    est <- getCurrentState inter
    case ledgerState est of
      LedgerStateShelley st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateAllegra st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateMary st -> pure $ nesEL $ shelleyLedgerState st
      LedgerStateAlonzo st -> pure $ nesEL $ shelleyLedgerState st
      _ -> throwIO UnexpectedEra

getCurrentSlot :: Interpreter -> IO SlotNo
getCurrentSlot interpreter =
    isSlot <$> readMVar (iState interpreter)

withAlonzoLedgerState :: Interpreter
                      -> (LedgerState (ShelleyBlock (AlonzoEra StandardCrypto)) -> Either ForgingError a)
                      -> IO a
withAlonzoLedgerState inter mk = do
    st <- getCurrentState inter
    case ledgerState st of
      LedgerStateAlonzo sta -> case mk sta of
        Right a -> pure a
        Left err -> throwIO err
      _ -> throwIO ExpectedAlonzoState

withShelleyLedgerState :: Interpreter
                       -> (LedgerState (ShelleyBlock (ShelleyEra StandardCrypto)) -> Either ForgingError a)
                       -> IO a
withShelleyLedgerState inter mk = do
    st <- getCurrentState inter
    case ledgerState st of
      LedgerStateShelley sts -> case mk sts of
        Right a -> pure a
        Left err -> throwIO err
      _ -> throwIO ExpectedShelleyState

mkTxId :: TxEra -> SL.TxId StandardCrypto
mkTxId (TxAlonzo tx) =
  SL.txid @(AlonzoEra StandardCrypto) (getField @"body" tx)
mkTxId (TxShelley tx) =
  SL.txid @(ShelleyEra StandardCrypto) (getField @"body" tx)

mkValidated :: TxEra -> Validated (Consensus.GenTx CardanoBlock)
mkValidated (TxAlonzo tx) =
    Consensus.HardForkValidatedGenTx
      (Consensus.OneEraValidatedGenTx
        (S (S (S (S (Z (Consensus.WrapValidatedGenTx tx')))))))
  where
    tx' = Consensus.mkShelleyValidatedTx $
            SL.unsafeMakeValidated tx

mkValidated (TxShelley tx) =
    Consensus.HardForkValidatedGenTx
      (Consensus.OneEraValidatedGenTx
        (S (Z (Consensus.WrapValidatedGenTx tx'))))
  where
    tx' = Consensus.mkShelleyValidatedTx $
            SL.unsafeMakeValidated tx

mkForecast :: TopLevelConfig CardanoBlock
           -> ExtLedgerState CardanoBlock
           -> Forecast (LedgerView (BlockProtocol CardanoBlock))
mkForecast cfg st =
    ledgerViewForecastAt
       (configLedger cfg)
       (ledgerState st)

textShow :: Show a => a -> Text
textShow = Text.pack . show

throwLeft :: (MonadThrow f, Exception e) => Either e a -> f a
throwLeft = either throwIO pure
