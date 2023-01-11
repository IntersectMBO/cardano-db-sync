{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.LocalStateQuery (
  NoLedgerStateEnv (..),
  mkNoLedgerStateEnv,
  getSlotDetailsNode,
  localStateQueryHandler,
  newStateQueryTMVar,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Db (textShow)
import Cardano.DbSync.StateQuery
import Cardano.DbSync.Types
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

data NoLedgerStateEnv = NoLedgerStateEnv
  { nlsTracer :: Trace IO Text
  , nlsSystemStart :: !SystemStart
  , nlsQueryVar :: StateQueryTMVar CardanoBlock CardanoInterpreter
  , nlsHistoryInterpreterVar :: StrictTVar IO (Strict.Maybe CardanoInterpreter)
  }

newtype StateQueryTMVar blk result = StateQueryTMVar
  { unStateQueryTMVar ::
      StrictTMVar
        IO
        ( Query blk result
        , StrictTMVar IO (Either AcquireFailure result)
        )
  }

mkNoLedgerStateEnv :: Trace IO Text -> SystemStart -> IO NoLedgerStateEnv
mkNoLedgerStateEnv trce systemStart = do
  qVar <- newStateQueryTMVar
  interVar <- newTVarIO Strict.Nothing
  pure $ NoLedgerStateEnv trce systemStart qVar interVar

newStateQueryTMVar :: IO (StateQueryTMVar blk result)
newStateQueryTMVar = StateQueryTMVar <$> newEmptyTMVarIO

-- Get the requested slot details using a history interpreter stashed in an IORef.
-- If the history interpreter does not exist, get one.
-- If the existing history interpreter returns an error, get a new one and try again.
getSlotDetailsNode ::
  NoLedgerStateEnv ->
  SlotNo ->
  IO SlotDetails
getSlotDetailsNode env slot = do
  einterp1 <- maybe (getHistoryInterpreter env) pure =<< atomically (fromStrictMaybe <$> readTVar interVar)
  case evalSlotDetails einterp1 of
    Right sd -> insertCurrentTime sd
    Left _ -> do
      einterp2 <- getHistoryInterpreter env
      case evalSlotDetails einterp2 of
        Left err -> panic $ "getSlotDetailsNode: " <> textShow err
        Right sd -> insertCurrentTime sd
  where
    interVar = nlsHistoryInterpreterVar env

    evalSlotDetails :: Interpreter (CardanoEras StandardCrypto) -> Either PastHorizonException SlotDetails
    evalSlotDetails interp =
      interpretQuery interp (querySlotDetails (nlsSystemStart env) slot)

    insertCurrentTime :: SlotDetails -> IO SlotDetails
    insertCurrentTime sd = do
      time <- getCurrentTime
      pure $ sd {sdCurrentTime = time}

    fromStrictMaybe :: Strict.Maybe a -> Maybe a
    fromStrictMaybe (Strict.Just a) = Just a
    fromStrictMaybe Strict.Nothing = Nothing

getHistoryInterpreter ::
  NoLedgerStateEnv ->
  IO CardanoInterpreter
getHistoryInterpreter env = do
  respVar <- newEmptyTMVarIO
  atomically $ putTMVar reqVar (BlockQuery $ QueryHardFork GetInterpreter, respVar)
  res <- atomically $ takeTMVar respVar
  case res of
    Left err ->
      panic $ "getHistoryInterpreter: " <> textShow err
    Right interp -> do
      logInfo tracer "getHistoryInterpreter: acquired"
      atomically $ writeTVar interVar $ Strict.Just interp
      pure interp
  where
    reqVar = unStateQueryTMVar $ nlsQueryVar env
    interVar = nlsHistoryInterpreterVar env
    tracer = nlsTracer env

-- This is called during the ChainSync setup and loops forever. Queries can be posted to
-- it and responses retrieved via a TVar.
localStateQueryHandler ::
  forall a.
  NoLedgerStateEnv ->
  LocalStateQueryClient CardanoBlock (Point CardanoBlock) (Query CardanoBlock) IO a
localStateQueryHandler env =
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

    reqVar = unStateQueryTMVar $ nlsQueryVar env
