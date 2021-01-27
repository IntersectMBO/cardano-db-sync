{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.StateQuery
  ( InterpreterVar (..)
  , getSlotDetails
  , newInterpreterVar
  , getInterpreter
  ) where

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Cardano.Prelude

import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, readTMVar)

import           Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import           Ouroboros.Consensus.Cardano.Block (CardanoEras)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..),
                   SystemStart (..))
import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History.Follower as Follower
import           Ouroboros.Consensus.HardFork.History.Qry (Expr (..), Interpreter,
                   PastHorizonException, Qry, interpretQuery, qryFromExpr, slotToEpoch')
import           Ouroboros.Consensus.Network.NodeToClient
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)



newtype InterpreterVar blk = InterpreterVar {
    interpreter :: TMVar (Maybe (Interpreter (HardForkIndices blk)))
  }

newInterpreterVar :: IO (InterpreterVar blk)
newInterpreterVar = InterpreterVar <$> newEmptyTMVarIO

getInterpreter :: WithInterClient blk
               -> InterpreterVar blk
               -> IO (Interpreter (HardForkIndices blk))
getInterpreter withInter interVar = case Follower.interpreter withInter of
    Just inter -> return inter
    Nothing -> do
      mInter <- atomically $ readTMVar $ interpreter interVar
      case mInter of
        Just inter -> return inter
        Nothing -> panic "could not find interpreter"

getSlotDetails :: SlotNo -> DbSyncEnv -> Interpreter (CardanoEras StandardCrypto) -> IO SlotDetails
getSlotDetails slot env inter = case evalSlotDetails inter of
    Left err -> panic $ "getSlotDetails: " <> textShow err
    Right sd -> insertCurrentTime sd
  where
    evalSlotDetails :: Interpreter (CardanoEras StandardCrypto) -> Either PastHorizonException SlotDetails
    evalSlotDetails interp =
      interpretQuery interp (querySlotDetails (envSystemStart env) slot)

insertCurrentTime :: SlotDetails -> IO SlotDetails
insertCurrentTime sd = do
  time <- getCurrentTime
  pure $ sd { sdCurrentTime = time }

-- -------------------------------------------------------------------------------------------------

-- TODO: Switch back to the old version of this when this is fixed:
-- https://github.com/input-output-hk/cardano-db-sync/issues/276
querySlotDetails :: SystemStart -> SlotNo -> Qry SlotDetails
querySlotDetails start absSlot = do
  absTime <- qryFromExpr $
                ELet (EAbsToRelSlot (ELit absSlot)) $ \ relSlot ->
                ELet (ERelSlotToTime (EVar relSlot)) $ \ relTime ->
                ELet (ERelToAbsTime (EVar relTime)) $ \ absTime ->
                EVar absTime
  (absEpoch, slotInEpoch) <- slotToEpoch' absSlot
  epochSize <- qryFromExpr $ EEpochSize (ELit absEpoch)
  let time = relToUTCTime start absTime
  -- Set sdCurrentTime below and over write that above.
  pure $ SlotDetails time time absEpoch (EpochSlot slotInEpoch) epochSize

relToUTCTime :: SystemStart -> RelativeTime -> UTCTime
relToUTCTime (SystemStart start) (RelativeTime rel) = addUTCTime rel start
