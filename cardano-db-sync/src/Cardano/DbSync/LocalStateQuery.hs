{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.LocalStateQuery (
  NoLedgerEnv (..),
  mkNoLedgerEnv,
  getSlotDetailsNode,
  localStateQueryHandler,
  newStateQueryTMVar,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Prelude hiding (atomically, (.))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTMVar,
  StrictTVar,
  atomically,
  newEmptyTMVarIO,
  newTVarIO,
  putTMVar,
  readTVar,
  takeTMVar,
  writeTVar,
 )
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (getCurrentTime)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block (BlockQuery (QueryHardFork), CardanoEras)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (
  QueryHardFork (GetInterpreter),
 )
import Ouroboros.Consensus.HardFork.History.Qry (
  Interpreter,
  PastHorizonException,
  interpretQuery,
 )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Network.Block (Point (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client (
  ClientStAcquired (..),
  ClientStAcquiring (..),
  ClientStIdle (..),
  ClientStQuerying (..),
  LocalStateQueryClient (..),
 )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

data NoLedgerEnv = NoLedgerEnv
  { nleTracer :: Trace IO Text
  , nleSystemStart :: !SystemStart
  , nleQueryVar :: StateQueryTMVar CardanoBlock CardanoInterpreter
  , nleHistoryInterpreterVar :: StrictTVar IO (Strict.Maybe CardanoInterpreter)
  , nleNetwork :: !Ledger.Network
  , nleProtocolInfo :: !(Consensus.ProtocolInfo CardanoBlock)
  }

newtype StateQueryTMVar blk result = StateQueryTMVar
  { unStateQueryTMVar ::
      StrictTMVar
        IO
        ( Query blk result
        , StrictTMVar IO (Either AcquireFailure result)
        )
  }

mkNoLedgerEnv :: Trace IO Text -> Consensus.ProtocolInfo CardanoBlock -> Ledger.Network -> SystemStart -> IO NoLedgerEnv
mkNoLedgerEnv trce protoInfo network systemStart = do
  qVar <- newStateQueryTMVar
  interVar <- newTVarIO Strict.Nothing
  pure $ NoLedgerEnv trce systemStart qVar interVar network protoInfo

newStateQueryTMVar :: IO (StateQueryTMVar blk result)
newStateQueryTMVar = StateQueryTMVar <$> newEmptyTMVarIO

-- Get the requested slot details using a history interpreter stashed in an IORef.
-- If the history interpreter does not exist, get one.
-- If the existing history interpreter returns an error, get a new one and try again.
getSlotDetailsNode ::
  NoLedgerEnv ->
  SlotNo ->
  IO SlotDetails
getSlotDetailsNode nlEnv slot = do
  einterp1 <- maybe (getHistoryInterpreter nlEnv) pure =<< atomically (fromStrictMaybe <$> readTVar interVar)
  case evalSlotDetails einterp1 of
    Right sd -> insertCurrentTime sd
    Left _ -> do
      einterp2 <- getHistoryInterpreter nlEnv
      case evalSlotDetails einterp2 of
        Left err -> throwIO $ SNErrLocalStateQuery $ "getSlotDetailsNode: " <> Prelude.show err
        Right sd -> insertCurrentTime sd
  where
    interVar = nleHistoryInterpreterVar nlEnv

    evalSlotDetails :: Interpreter (CardanoEras StandardCrypto) -> Either PastHorizonException SlotDetails
    evalSlotDetails interp =
      interpretQuery interp (querySlotDetails (nleSystemStart nlEnv) slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      time <- getCurrentTime
      pure $ sd {sdCurrentTime = time}

    fromStrictMaybe :: Strict.Maybe a -> Maybe a
    fromStrictMaybe (Strict.Just a) = Just a
    fromStrictMaybe Strict.Nothing = Nothing

getHistoryInterpreter ::
  NoLedgerEnv ->
  IO CardanoInterpreter
getHistoryInterpreter nlEnv = do
  respVar <- newEmptyTMVarIO
  atomically $ putTMVar reqVar (BlockQuery $ QueryHardFork GetInterpreter, respVar)
  res <- atomically $ takeTMVar respVar
  case res of
    Left err ->
      throwIO $ SNErrLocalStateQuery $ "getHistoryInterpreter: " <> Prelude.show err
    Right interp -> do
      logInfo tracer "getHistoryInterpreter: acquired"
      atomically $ writeTVar interVar $ Strict.Just interp
      pure interp
  where
    reqVar = unStateQueryTMVar $ nleQueryVar nlEnv
    interVar = nleHistoryInterpreterVar nlEnv
    tracer = nleTracer nlEnv

-- This is called during the ChainSync setup and loops forever. Queries can be posted to
-- it and responses retrieved via a TVar.
localStateQueryHandler ::
  forall a.
  NoLedgerEnv ->
  LocalStateQueryClient CardanoBlock (Point CardanoBlock) (Query CardanoBlock) IO a
localStateQueryHandler nlEnv =
  LocalStateQueryClient idleState
  where
    idleState :: IO (StateQuery.ClientStIdle CardanoBlock (Point CardanoBlock) (Query CardanoBlock) IO a)
    idleState = do
      (query, respVar) <- atomically $ takeTMVar reqVar
      pure
        . SendMsgAcquire Nothing
        $ ClientStAcquiring
          { recvMsgAcquired =
              pure . SendMsgQuery query $
                ClientStQuerying
                  { recvMsgResult = \result -> do
                      atomically $ putTMVar respVar (Right result)
                      pure $ SendMsgRelease idleState
                  }
          , recvMsgFailure = \failure -> do
              atomically $ putTMVar respVar (Left failure)
              idleState
          }

    reqVar = unStateQueryTMVar $ nleQueryVar nlEnv
