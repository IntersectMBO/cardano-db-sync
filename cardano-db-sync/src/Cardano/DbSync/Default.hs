{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Default (
  insertListBlocks,
) where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api (generateNewEpochEvents, getPruneInterval, getRanIndexes, getSafeBlockNoDiff, hasLedgerState, isConsistent, runIndexMigrations, setConsistentLevel, whenPruneTxOut)
import Cardano.DbSync.Api.Ledger (bootStrapMaybe)
import Cardano.DbSync.AppT (App, ConsistentLevel (..), InsertOptions (..), LedgerEnv (..), MonadAppDB (..), SyncEnv (..), SyncOptions (..), askInsertOptions, askTrace)
import Cardano.DbSync.Epoch (epochHandler)
import Cardano.DbSync.Era.Byron.Insert (insertByronBlock)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Block (insertBlockUniversal)
import Cardano.DbSync.Era.Universal.Epoch (hasEpochStartEvent, hasNewEpochEvent)
import Cardano.DbSync.Era.Universal.Insert.Certificate (mkAdaPots)
import Cardano.DbSync.Era.Universal.Insert.LedgerEvent (insertNewEpochLedgerEvents)
import Cardano.DbSync.Error (runOrThrowApp)
import Cardano.DbSync.Ledger.State (applyBlockAndSnapshot, defaultApplyResult)
import Cardano.DbSync.Ledger.Types (ApplyResult (..))
import Cardano.DbSync.LocalStateQuery
import Cardano.DbSync.Rollback (rollbackFromBlockNo)
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.DbSync.Util.Constraint (addConstraintsIfNotExist)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Cardano.Ledger.Shelley.AdaPots as Shelley
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Ouroboros.Consensus.Block (BlockNo)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import Ouroboros.Network.Block (blockHash, blockNo, getHeaderFields, headerFieldBlockNo, unBlockNo)

insertListBlocks :: [CardanoBlock] -> App ()
insertListBlocks blocks = do
  traverse_ applyAndInsertBlockMaybe blocks

applyAndInsertBlockMaybe :: CardanoBlock -> App ()
applyAndInsertBlockMaybe cblk = do
  tracer <- askTrace
  isCon <- isConsistent
  (!applyRes, !tookSnapshot) <- mkApplyResult isCon cblk

  if isCon
    then insertBlock cblk applyRes False tookSnapshot
    else do
      eiBlockInDbAlreadyId <- dbQueryToApp $ DB.queryBlockId (SBS.fromShort . Consensus.getOneEraHash $ blockHash cblk)
      case eiBlockInDbAlreadyId of
        Left _ -> do
          liftIO . logInfo tracer $
            mconcat
              [ "Received block which is not in the db with "
              , textShow (getHeaderFields cblk)
              , ". Time to restore consistency."
              ]
          rollbackFromBlockNo (blockNo cblk)
          -- void $ migrateStakeDistr (apOldLedger applyRes)
          insertBlock cblk applyRes True tookSnapshot
          setConsistentLevel Consistent
        Right blockId
          | Just (adaPots, slotNo, epochNo) <- getAdaPots applyRes -> do
              replaced <- dbQueryToApp $ DB.replaceAdaPots blockId $ mkAdaPots blockId slotNo epochNo adaPots
              liftIO $
                logInfo tracer $
                  if replaced
                    then "Fixed AdaPots for " <> textShow epochNo
                    else "Reached " <> textShow epochNo
        Right _
          | Just epochNo <- getNewEpoch applyRes ->
              liftIO $ logInfo tracer $ "Reached " <> textShow epochNo
        _other -> pure ()

mkApplyResult :: Bool -> CardanoBlock -> App (ApplyResult, Bool)
mkApplyResult isCons cblk = do
  ledgerEnv <- asks envLedgerEnv
  case ledgerEnv of
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
  CardanoBlock ->
  ApplyResult ->
  -- is first Block after rollback
  Bool ->
  -- has snapshot been taken
  Bool ->
  App ()
-- ExceptT SyncNodeError (ReaderT SqlBackend (LoggingT IO)) ()
insertBlock cblk applyRes firstAfterRollback tookSnapshot = do
  tracer <- askTrace
  iopts <- askInsertOptions
  !epochEvents <- generateNewEpochEvents (apSlotDetails applyRes)
  let !applyResult = applyRes {apEvents = sort $ epochEvents <> apEvents applyRes}
  let !details = apSlotDetails applyResult
  let !withinTwoMin = isWithinTwoMin details
  let !withinHalfHour = isWithinHalfHour details
  insertNewEpochLedgerEvents (sdEpochNo details) (apEvents applyResult)
  let isNewEpochEvent = hasNewEpochEvent (apEvents applyResult)
  let isStartEventOrRollback = hasEpochStartEvent (apEvents applyResult) || firstAfterRollback
  let isMember poolId = Set.member poolId (apPoolsRegistered applyResult)
  let insertBlockUniversal' blk =
        insertBlockUniversal
          isStartEventOrRollback
          withinTwoMin
          withinHalfHour
          blk
          details
          isMember
          applyResult

  -- Here we insert the block and it's txs, but in adition we also cache some values which we later
  -- use when updating the Epoch, thus saving us having to recalulating them later.
  case cblk of
    BlockByron blk ->
      runOrThrowApp tracer $ insertByronBlock isStartEventOrRollback blk details
    BlockShelley blk ->
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromShelleyBlock blk
    BlockAllegra blk ->
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromAllegraBlock blk
    BlockMary blk ->
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromMaryBlock blk
    BlockAlonzo blk -> do
      prices <- getPrices applyResult
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromAlonzoBlock (ioPlutusExtra iopts) prices blk
    BlockBabbage blk -> do
      prices <- getPrices applyResult
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromBabbageBlock (ioPlutusExtra iopts) prices blk
    BlockConway blk -> do
      prices <- getPrices applyResult
      runOrThrowApp tracer $
        insertBlockUniversal' $
          Generic.fromConwayBlock (ioPlutusExtra iopts) prices blk
  -- update the epoch
  void $ updateEpoch cblk details isNewEpochEvent
  whenPruneTxOut $ do
    pruneInterval <- getPruneInterval
    when (unBlockNo (blkNo cblk) `mod` pruneInterval == 0) $
      do
        blockNoDiff <- getSafeBlockNoDiff
        dbQueryToApp $ DB.deleteConsumedTxOut tracer blockNoDiff
  commitOrIndexes tookSnapshot withinTwoMin withinHalfHour

updateEpoch :: CardanoBlock -> SlotDetails -> Bool -> App ()
updateEpoch cblk details isNewEpochEvent = do
  syncEnv <- ask
  tracer <- askTrace
  -- if have --dissable-epoch && --dissable-cache then no need to run this function
  when (soptEpochAndCacheEnabled $ envOptions syncEnv) $
    runOrThrowApp tracer $
      epochHandler
        (envCache syncEnv)
        isNewEpochEvent
        (BlockDetails cblk details)

getPrices :: ApplyResult -> App (Maybe Ledger.Prices)
getPrices applyResult = do
  syncEnv <- ask
  case apPrices applyResult of
    Strict.Just pr -> pure $ Just pr
    Strict.Nothing | hasLedgerState syncEnv -> pure $ Just $ Ledger.Prices minBound minBound
    Strict.Nothing -> pure Nothing

commitOrIndexes ::
  -- | took snapshot
  Bool ->
  -- | within two minutes
  Bool ->
  -- | within half hour
  Bool ->
  App ()
commitOrIndexes tookSnapshot withinTwoMin withinHalfHour = do
  commited <-
    if withinTwoMin || tookSnapshot
      then do
        dbQueryToApp DB.transactionCommit
        pure True
      else pure False
  when withinHalfHour $ do
    bootStrapMaybe
    ranIndexes <- getRanIndexes
    addConstraintsIfNotExist
    unless ranIndexes $ do
      unless commited $ dbQueryToApp DB.transactionCommit
      runIndexMigrations

isWithinTwoMin :: SlotDetails -> Bool
isWithinTwoMin sd = isSyncedWithinSeconds sd 120 == SyncFollowing

isWithinHalfHour :: SlotDetails -> Bool
isWithinHalfHour sd = isSyncedWithinSeconds sd 1800 == SyncFollowing

blkNo :: CardanoBlock -> BlockNo
blkNo cblk = headerFieldBlockNo $ getHeaderFields cblk
