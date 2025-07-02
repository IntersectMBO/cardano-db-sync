{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cardano.DbSync.Default (
  insertListBlocks,
) where

import Control.Monad.Logger (LoggingT)
import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict

import Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.Shelley.AdaPots as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (blockHash, blockNo, getHeaderFields, headerFieldBlockNo, unBlockNo)

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Ledger
import Cardano.DbSync.Api.Types (ConsistentLevel (..), InsertOptions (..), LedgerEnv (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Epoch (epochHandler)
import Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Block (insertBlockUniversal)
import Cardano.DbSync.Era.Universal.Epoch (hasEpochStartEvent, hasNewEpochEvent)
import Cardano.DbSync.Era.Universal.Insert.Certificate (mkAdaPots)
import Cardano.DbSync.Era.Universal.Insert.LedgerEvent (insertNewEpochLedgerEvents)
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.DbSync.Ledger.State (applyBlockAndSnapshot, defaultApplyResult)
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Rollback
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)

insertListBlocks ::
  SyncEnv ->
  [CardanoBlock] ->
  IO (Either SyncNodeError ())
insertListBlocks syncEnv blocks = do
  result <- DB.runDbIohkLoggingEither tracer (envDbEnv syncEnv) $ do
    runExceptT $ traverse_ (applyAndInsertBlockMaybe syncEnv tracer) blocks
  case result of
    Left dbErr -> pure $ Left $ SNErrDatabase dbErr
    Right (Left syncErr) -> pure $ Left syncErr
    Right (Right _) -> pure $ Right ()
  where
    tracer = getTrace syncEnv

applyAndInsertBlockMaybe ::
  SyncEnv ->
  Trace IO Text ->
  CardanoBlock ->
  ExceptT SyncNodeError (DB.DbAction (LoggingT IO)) ()
applyAndInsertBlockMaybe syncEnv tracer cblk = do
  bl <- liftIO $ isConsistent syncEnv
  (!applyRes, !tookSnapshot) <- liftIO (mkApplyResult bl)
  if bl
    then -- In the usual case it will be consistent so we don't need to do any queries. Just insert the block
      lift $ insertBlock syncEnv cblk applyRes False tookSnapshot
    else do
      eiBlockInDbAlreadyId <- lift $ DB.queryBlockIdEither (SBS.fromShort . Consensus.getOneEraHash $ blockHash cblk) ""
      -- If the block is already in db, do nothing. If not, delete all blocks with greater 'BlockNo' or
      -- equal, insert the block and restore consistency between ledger and db.
      case eiBlockInDbAlreadyId of
        Left _ -> do
          liftIO . logInfo tracer $
            mconcat
              [ "Received block which is not in the db with "
              , textShow (getHeaderFields cblk)
              , ". Time to restore consistency."
              ]
          lift $ rollbackFromBlockNo syncEnv (blockNo cblk)
          lift $ insertBlock syncEnv cblk applyRes True tookSnapshot
          liftIO $ setConsistentLevel syncEnv Consistent
        Right blockId | Just (adaPots, slotNo, epochNo) <- getAdaPots applyRes -> do
          replaced <- lift $ DB.replaceAdaPots blockId $ mkAdaPots blockId slotNo epochNo adaPots
          if replaced
            then liftIO $ logInfo tracer $ "Fixed AdaPots for " <> textShow epochNo
            else liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
        Right _
          | Just epochNo <- getNewEpoch applyRes ->
              liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
        _otherwise -> pure ()
  where
    mkApplyResult :: Bool -> IO (ApplyResult, Bool)
    mkApplyResult isCons = do
      case envLedgerEnv syncEnv of
        HasLedger hle -> applyBlockAndSnapshot hle cblk isCons
        NoLedger nle -> do
          slotDetails <- getSlotDetailsNode nle (cardanoBlockSlotNo cblk)
          pure (defaultApplyResult slotDetails, False)

    getAdaPots :: ApplyResult -> Maybe (Shelley.AdaPots, SlotNo, EpochNo)
    getAdaPots appRes = do
      newEpoch <- maybeFromStrict $ apNewEpoch appRes
      adaPots <- maybeFromStrict $ Generic.neAdaPots newEpoch
      pure (adaPots, sdSlotNo $ apSlotDetails appRes, sdEpochNo $ apSlotDetails appRes)

    getNewEpoch :: ApplyResult -> Maybe EpochNo
    getNewEpoch appRes =
      Generic.neEpoch <$> maybeFromStrict (apNewEpoch appRes)

insertBlock ::
  SyncEnv ->
  CardanoBlock ->
  ApplyResult ->
  -- is first Block after rollback
  Bool ->
  -- has snapshot been taken
  Bool ->
  DB.DbAction (LoggingT IO) ()
insertBlock syncEnv cblk applyRes firstAfterRollback tookSnapshot = do
  !epochEvents <- liftIO $ atomically $ generateNewEpochEvents syncEnv (apSlotDetails applyRes)
  let !applyResult = applyRes {apEvents = sort $ epochEvents <> apEvents applyRes}
  let !details = apSlotDetails applyResult
  let !withinTwoMin = isWithinTwoMin details
  let !withinHalfHour = isWithinHalfHour details
  insertNewEpochLedgerEvents syncEnv (sdEpochNo details) (apEvents applyResult)
  let isNewEpochEvent = hasNewEpochEvent (apEvents applyResult)
  let isStartEventOrRollback = hasEpochStartEvent (apEvents applyResult) || firstAfterRollback
  let isMember poolId = Set.member poolId (apPoolsRegistered applyResult)
  let insertBlockUniversal' blk =
        insertBlockUniversal
          syncEnv
          isStartEventOrRollback
          withinTwoMin
          withinHalfHour
          blk
          details
          isMember
          applyResult

  -- Here we insert the block and it's txs, but in addition we also cache some values which we later
  -- use when updating the Epoch, thus saving us having to recalculating them later.
  -- Any TxOut lookup failures will propagate via throwError
  case cblk of
    BlockByron blk ->
      insertByronBlock syncEnv isStartEventOrRollback blk details
    BlockShelley blk ->
      insertBlockUniversal' $
        Generic.fromShelleyBlock blk
    BlockAllegra blk ->
      insertBlockUniversal' $
        Generic.fromAllegraBlock blk
    BlockMary blk ->
      insertBlockUniversal' $
        Generic.fromMaryBlock blk
    BlockAlonzo blk ->
      insertBlockUniversal' $
        Generic.fromAlonzoBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
    BlockBabbage blk ->
      insertBlockUniversal' $
        Generic.fromBabbageBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
    BlockConway blk ->
      insertBlockUniversal' $
        Generic.fromConwayBlock (ioPlutusExtra iopts) (getPrices applyResult) blk
  -- update the epoch
  updateEpoch details isNewEpochEvent
  whenPruneTxOut syncEnv $
    when (unBlockNo blkNo `mod` getPruneInterval syncEnv == 0) $
      do
        DB.deleteConsumedTxOut tracer txOutVariantType (getSafeBlockNoDiff syncEnv)
  commitOrIndexes withinTwoMin withinHalfHour
  where
    tracer = getTrace syncEnv
    txOutVariantType = getTxOutVariantType syncEnv
    iopts = getInsertOptions syncEnv

    updateEpoch details isNewEpochEvent =
      -- if have --dissable-epoch && --dissable-cache then no need to run this function
      when (soptEpochAndCacheEnabled $ envOptions syncEnv) $
        epochHandler
          syncEnv
          tracer
          (envCache syncEnv)
          isNewEpochEvent
          (BlockDetails cblk details)

    getPrices :: ApplyResult -> Maybe Ledger.Prices
    getPrices applyResult = case apPrices applyResult of
      Strict.Just pr -> Just pr
      Strict.Nothing | hasLedgerState syncEnv -> Just $ Ledger.Prices minBound minBound
      Strict.Nothing -> Nothing

    commitOrIndexes :: Bool -> Bool -> DB.DbAction (LoggingT IO) ()
    commitOrIndexes withinTwoMin withinHalfHour = do
      commited <-
        if withinTwoMin || tookSnapshot
          then pure True
          else pure False
      when withinHalfHour $ do
        bootStrapMaybe syncEnv
        ranIndexes <- liftIO $ getRanIndexes syncEnv
        addConstraintsIfNotExist syncEnv tracer
        unless ranIndexes $
          liftIO $
            runIndexMigrations syncEnv

    blkNo = headerFieldBlockNo $ getHeaderFields cblk

isWithinTwoMin :: SlotDetails -> Bool
isWithinTwoMin sd = isSyncedWithinSeconds sd 120 == SyncFollowing

isWithinHalfHour :: SlotDetails -> Bool
isWithinHalfHour sd = isSyncedWithinSeconds sd 1800 == SyncFollowing
