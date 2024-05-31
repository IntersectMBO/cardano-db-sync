{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.LocalStateQuery (
  getSlotDetailsNode,
  localStateQueryHandler,
) where

import Cardano.BM.Trace (logInfo)
import Cardano.DbSync.AppT (App, NoLedgerEnv (..), StateQueryTMVar (..))
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Prelude hiding (atomically, (.))
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (
  atomically,
  newEmptyTMVarIO,
  putTMVar,
  readTVar,
  takeTMVar,
  writeTVar,
 )
import qualified Data.Strict.Maybe as Strict
import Data.Time.Clock (getCurrentTime)
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
import Ouroboros.Network.Block (Point (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Client (
  ClientStAcquired (..),
  ClientStAcquiring (..),
  ClientStIdle (..),
  ClientStQuerying (..),
  LocalStateQueryClient (..),
 )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (..))

-- Get the requested slot details using a history interpreter stashed in an IORef.
-- If the history interpreter does not exist, get one.
-- If the existing history interpreter returns an error, get a new one and try again.
getSlotDetailsNode ::
  NoLedgerEnv ->
  SlotNo ->
  App SlotDetails
getSlotDetailsNode nlEnv slot = do
  einterp1 <- maybe (getHistoryInterpreter nlEnv) pure =<< liftIO (atomically (fromStrictMaybe <$> readTVar interVar))
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

    insertCurrentTime :: SlotDetails -> App SlotDetails
    insertCurrentTime sd = do
      time <- liftIO getCurrentTime
      pure $ sd {sdCurrentTime = time}

    fromStrictMaybe :: Strict.Maybe a -> Maybe a
    fromStrictMaybe (Strict.Just a) = Just a
    fromStrictMaybe Strict.Nothing = Nothing

getHistoryInterpreter :: NoLedgerEnv -> App CardanoInterpreter
getHistoryInterpreter nlEnv = do
  respVar <- liftIO newEmptyTMVarIO
  liftIO $ atomically $ putTMVar reqVar (BlockQuery $ QueryHardFork GetInterpreter, respVar)
  res <- liftIO $ atomically $ takeTMVar respVar
  case res of
    Left err ->
      throwIO $ SNErrLocalStateQuery $ "getHistoryInterpreter: " <> Prelude.show err
    Right interp -> do
      liftIO $ logInfo tracer "getHistoryInterpreter: acquired"
      liftIO $ atomically $ writeTVar interVar $ Strict.Just interp
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
        . SendMsgAcquire VolatileTip
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
