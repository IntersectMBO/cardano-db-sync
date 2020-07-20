{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.StateQuery
  ( StateQueryTMVar -- Opaque, so it cannot be misused.
  , getSlotDetails
  , localStateQueryHandler
  , newStateQueryTMVar
  ) where

import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Time.Clock (UTCTime, addUTCTime)

import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), SystemStart (..))
import           Ouroboros.Consensus.Cardano.Block (CardanoEras, Query (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock (..))
import           Ouroboros.Consensus.HardFork.Combinator.Ledger.Query (QueryHardFork (GetInterpreter))
import           Ouroboros.Consensus.HardFork.History.Qry (Qry (..), Interpreter, interpretQuery, slotToEpoch')
import           Ouroboros.Consensus.HardFork.History.Summary (PastHorizonException)
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (ClientStQuerying (..),
                    ClientStAcquired (..), ClientStAcquiring (..), ClientStIdle (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Client (LocalStateQueryClient (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as StateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)

import           System.IO.Unsafe (unsafePerformIO)

newtype StateQueryTMVar blk result = StateQueryTMVar
  { unStateQueryTMVar ::
      TMVar
        ( Point blk
        , Query blk result
        , TMVar (Either AcquireFailure result)
        )
  }

newStateQueryTMVar :: IO (StateQueryTMVar blk result)
newStateQueryTMVar = StateQueryTMVar <$> newEmptyTMVarIO


-- Incredibly naive. Runs a query to get a new history interpreter each time it is called.
getSlotDetails
    :: Trace IO Text -> DbSyncEnv
    -> StateQueryTMVar (HardForkBlock (CardanoEras TPraosStandardCrypto)) (Interpreter (CardanoEras TPraosStandardCrypto))
    -> Point (HardForkBlock (CardanoEras TPraosStandardCrypto)) -> SlotNo
    -> IO SlotDetails
getSlotDetails tracer env queryVar point slot = do
    einterp1 <- maybe (getHistoryInterpreter tracer queryVar point) pure =<< readIORef historyInterpVar
    case evalSlotDetails einterp1 of
      Right sd -> pure sd
      Left _ -> do
        einterp2 <- getHistoryInterpreter tracer queryVar point
        case evalSlotDetails einterp2 of
          Right sd -> pure sd
          Left err -> panic $ "getSlotDetails: " <> textShow err
  where
    evalSlotDetails :: Interpreter (CardanoEras TPraosStandardCrypto) -> Either PastHorizonException SlotDetails
    evalSlotDetails interp =
      interpretQuery interp (querySlotDetails (envSystemStart env) slot)

-- -------------------------------------------------------------------------------------------------

{-# NOINLINE historyInterpVar #-}
historyInterpVar :: IORef (Maybe (Interpreter (CardanoEras TPraosStandardCrypto)))
historyInterpVar = unsafePerformIO $ newIORef Nothing

getHistoryInterpreter
    :: Trace IO Text
    -> StateQueryTMVar (HardForkBlock (CardanoEras TPraosStandardCrypto)) (Interpreter (CardanoEras TPraosStandardCrypto))
    -> Point (HardForkBlock (CardanoEras TPraosStandardCrypto))
    -> IO (Interpreter (CardanoEras TPraosStandardCrypto))
getHistoryInterpreter tracer queryVar point = do
  respVar <- newEmptyTMVarIO
  atomically $ putTMVar (unStateQueryTMVar queryVar) (point, QueryHardFork GetInterpreter, respVar)
  res <- atomically $ takeTMVar respVar
  case res of
    Left err ->
      panic $ "getHistoryInterpreter: " <> textShow err
    Right interp -> do
      logInfo tracer $ "getHistoryInterpreter: acquired"
      writeIORef historyInterpVar $ Just interp
      pure interp


-- This is called during the ChainSync setup and loops forever. Queries can be posted to
-- it and responses retrieved via a TVar.
localStateQueryHandler
    :: forall block result a
    . StateQueryTMVar block result
    -> LocalStateQueryClient block (Query block) IO a
localStateQueryHandler (StateQueryTMVar reqVar) =
    LocalStateQueryClient idleState
  where
    idleState :: IO (StateQuery.ClientStIdle block (Query block) IO a)
    idleState = do
      (point, query, respVar) <- atomically $ takeTMVar reqVar
      pure $
        SendMsgAcquire point $
          ClientStAcquiring
            { recvMsgAcquired =
                SendMsgQuery query $
                  ClientStQuerying
                    { recvMsgResult = \result -> do
                        atomically $ putTMVar respVar (Right result)
                        pure $ SendMsgRelease idleState
                    }
            , recvMsgFailure = \failure -> do
                atomically $ putTMVar respVar (Left failure)
                idleState
            }

-- -------------------------------------------------------------------------------------------------

querySlotDetails :: SystemStart -> SlotNo -> Qry SlotDetails
querySlotDetails start absSlot = do
  relSlot <- QAbsToRelSlot absSlot
  relTime <- QRelSlotToTime relSlot
  utcTime <- relToUTCTime start <$> QRelToAbsTime relTime
  (absEpoch, slotInEpoch)  <- slotToEpoch' absSlot
  epochSize <- QEpochSize absEpoch
  pure $ SlotDetails utcTime absEpoch (SlotInEpoch slotInEpoch) epochSize

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start

