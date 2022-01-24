{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Mock.Forging.Interpreter
  ( Interpreter (..)
  , CardanoBlock
  , MockBlock (..)
  , NodeId (..)
  , InterpreterError (..)
  , initInterpreter
  , forgeNext
  , withAlonzoLedgerState
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Tracer
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict (NS (S, Z))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Generics
import           NoThunks.Class (OnlyCheckWhnfNamed (..))

import           Ouroboros.Consensus.Block hiding (blockMatchesHeader)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, HardForkBlock, StandardCrypto, AlonzoEra, LedgerState(LedgerStateAlonzo))
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
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Consensus
import qualified Ouroboros.Consensus.TypeFamilyWrappers as Consensus
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Consensus.Cardano.Node ()

import           Cardano.Ledger.Alonzo.Tx
import qualified Cardano.Ledger.Shelley.API.Mempool as SL

import           Cardano.Mock.ChainDB

type CardanoBlock = HardForkBlock (CardanoEras StandardCrypto)

-- | This module translated 'MockBlock' into CardanoBlock
data MockBlock = MockBlock
  { txs :: [ValidatedTx (AlonzoEra StandardCrypto)]
  , node :: NodeId
  }

newtype NodeId = NodeId { unNodeId :: Int }
  deriving Show

data Interpreter = Interpreter
  { iForging :: Map Int (BlockForging IO CardanoBlock)
  , iState :: StrictMVar IO InterpreterState
  , iTracerForge :: Tracer IO (ForgeStateInfo CardanoBlock)
  , iTopLeverConfig :: TopLevelConfig CardanoBlock
  }

data InterpreterState = InterpreterState
  { isChain :: !(ChainDB CardanoBlock)
  , isForecast :: !(Forecast (LedgerView (BlockProtocol CardanoBlock)))
  , isSlot :: !SlotNo
  -- ^ The first slot to try the next block
  , isNextBlockNo :: !BlockNo
  -- ^ the block number of the block to be forged
  } deriving Generic
    deriving NoThunks via OnlyCheckWhnfNamed "InterpreterState" InterpreterState

deriving instance Generic (ChainDB CardanoBlock)

deriving instance NoThunks (Forecast a)

deriving instance Generic (Forecast a)

initInterpreter :: ProtocolInfo IO CardanoBlock
                -> Tracer IO (ForgeStateInfo CardanoBlock)
                -> IO Interpreter
initInterpreter pinfo traceForge = do
  forging <- pInfoBlockForging pinfo
  let topLeverCfg = pInfoConfig pinfo
  let initSt = pInfoInitLedger pinfo
  let ledgerView = mkForecast topLeverCfg initSt
  let initState = InterpreterState
        { isChain = initChainDB topLeverCfg initSt
        , isForecast = ledgerView
        , isSlot = SlotNo 0
        , isNextBlockNo = BlockNo 0
        }
  stvar <- newMVar initState
  pure $ Interpreter
    { iForging = Map.fromList $ zip [0..] forging
    , iState = stvar
    , iTracerForge = traceForge
    , iTopLeverConfig = topLeverCfg
    }

forgeNext :: Interpreter -> MockBlock -> IO CardanoBlock
forgeNext interpreter testBlock = do
    interState <- readMVar $ iState interpreter
    case Map.lookup (unNodeId $ node testBlock) (iForging interpreter) of
      Nothing -> throwIO $ NonExistantNode (node testBlock)
      Just forging -> do
        blk <- tryConsecutiveSlots interState forging 0 (isSlot interState)
        let !chain' = extendChainDB (isChain interState) blk
        let !newSt = currentState chain'
        print newSt
        let newInterState = InterpreterState
              { isChain = chain'
              , isForecast = mkForecast cfg newSt
              , isSlot = blockSlot blk + 1
              , isNextBlockNo = blockNo blk + 1
              }
        _ <- swapMVar (iState interpreter) newInterState
        pure blk
  where
    cfg = iTopLeverConfig interpreter

    tryConsecutiveSlots :: InterpreterState
                        -> BlockForging IO CardanoBlock
                        -> Int
                        -> SlotNo
                        -> IO CardanoBlock
    tryConsecutiveSlots interState blockForging numberOfTries currentSlot = do
      let tryNext :: IO CardanoBlock = tryConsecutiveSlots interState blockForging (numberOfTries + 1) (currentSlot + 1)
      when (numberOfTries > 1000) (throwIO WentTooFar)

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

      !shouldForge <- checkShouldForge blockForging
                        (iTracerForge interpreter)
                        -- (contramap (TraceLabelCreds (forgeLabel blockForging))
                        --   (iTracerForge interpreter))
                        cfg
                        currentSlot
                        tickedChainDepState
      -- Check if we are the leader
      case shouldForge of
        ForgeStateUpdateError err -> do
          Text.putStrLn $ Text.unwords
            ["TraceForgeStateUpdateError", textShow currentSlot, textShow err]
          tryNext
        CannotForge cannotForge -> do
          Text.putStrLn $ Text.unwords
            ["TraceNodeCannotForge", textShow currentSlot, textShow cannotForge]
          tryNext
        NotLeader ->
          tryNext
        ShouldForge proof -> do
          -- Tick the ledger state for the 'SlotNo' we're producing a block for
          let tickedLedgerSt :: Ticked (LedgerState CardanoBlock)
              !tickedLedgerSt =
                applyChainTick
                  (configLedger cfg)
                  currentSlot
                  (ledgerState $ currentState $ isChain interState)

          let txs' = mkValidated <$> txs testBlock

          !blk <- Block.forgeBlock blockForging
            cfg
            (isNextBlockNo interState)
            currentSlot
            tickedLedgerSt
            txs'
            proof
          Text.putStrLn $ "Forged block at " <> textShow currentSlot <> " by " <> textShow (node testBlock)
          pure blk

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

getState :: Interpreter -> IO (ExtLedgerState CardanoBlock)
getState inter = do
    interState <- readMVar (iState inter)
    pure $ currentState $ isChain interState

withAlonzoLedgerState :: Interpreter
                      -> (LedgerState (ShelleyBlock (AlonzoEra StandardCrypto)) -> a)
                      -> IO a
withAlonzoLedgerState inter mk = do
    st <- getState inter
    case ledgerState st of
      LedgerStateAlonzo sta -> pure $ mk sta
      _ -> error "Expected Alonzo era state"

mkValidated :: ValidatedTx (AlonzoEra StandardCrypto)
            -> Validated (Consensus.GenTx CardanoBlock)
mkValidated tx =
    Consensus.HardForkValidatedGenTx
      (Consensus.OneEraValidatedGenTx
        (S (S (S (S (Z (Consensus.WrapValidatedGenTx tx')))))))
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

data InterpreterError =
    WentTooFar
  | ForecastError SlotNo OutsideForecastRange
  | NonExistantNode NodeId
  deriving (Show, Exception)

textShow :: Show a => a -> Text
textShow = Text.pack . show
