{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Universal.Block (
  insertBlockUniversal,
)
where

import Data.Bits (testBit)
import Data.Either.Extra (eitherToMaybe)
import Data.List (sortOn)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import Cardano.Binary (decodeFull', serialize')
import Cardano.Crypto.Leios (BitField, LeiosCert (..), encodeBitField, leiosSignatureToBytes)
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Keys
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.State as LState
import Cardano.Prelude
import LeiosDemoTypes (EbAnnouncement (..), EbHash (..))
import Lens.Micro ((^.))
import Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import qualified Ouroboros.Consensus.Shelley.Ledger.Ledger as Consensus

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..))
import Cardano.DbSync.Cache (
  cleanCachesForTip,
  insertBlockAndCache,
  optimiseCaches,
  queryPoolKeyWithCache,
  queryPrevBlockWithCache,
 )
import Cardano.DbSync.Cache.Types (CacheAction (..), CacheStatus (..))
import Cardano.DbSync.DbEvent (liftDbLookup)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Era.Universal.Epoch
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Era.Universal.Insert.Pool (IsPoolMember)
import Cardano.DbSync.Era.Universal.Insert.Tx (insertTx)
import Cardano.DbSync.Error (SyncNodeError, mkSyncNodeCallStack)
import Cardano.DbSync.Ledger.Types (ApplyResult (..), CardanoLedgerState (..))
import Cardano.DbSync.OffChain
import Cardano.DbSync.Types
import Cardano.DbSync.Util

--------------------------------------------------------------------------------------------
-- Insert a universal Block.
-- This is the entry point for inserting a block into the database, used for all eras appart from Byron.
--------------------------------------------------------------------------------------------
insertBlockUniversal ::
  SyncEnv ->
  -- | Should log
  Bool ->
  -- | Within two minutes
  Bool ->
  -- | Within half hour
  Bool ->
  Generic.Block ->
  SlotDetails ->
  IsPoolMember ->
  ApplyResult ->
  ExceptT SyncNodeError DB.DbM ()
insertBlockUniversal syncEnv shouldLog withinTwoMins withinHalfHour blk details isMember applyResult = do
  -- if we're syncing within 2 mins of the tip, we clean certain caches for tip following.
  when (isSyncedWithintwoMinutes details) $ cleanCachesForTip cache
  -- Optimise caches every 100k blocks to prevent unbounded growth
  when (unBlockNo (Generic.blkBlockNo blk) `mod` 100000 == 0) $ optimiseCaches cache
  do
    pbid <- case Generic.blkPreviousHash blk of
      Nothing -> liftDbLookup mkSyncNodeCallStack $ DB.queryGenesis $ renderErrorMessage (Generic.blkEra blk) -- this is for networks that fork from Byron on epoch 0.
      Just pHash -> queryPrevBlockWithCache syncEnv pHash (renderErrorMessage (Generic.blkEra blk))
    mPhid <- queryPoolKeyWithCache syncEnv UpdateCache $ coerceKeyRole $ Generic.blkSlotLeader blk
    let epochNo = sdEpochNo details

    slid <- lift $ DB.insertSlotLeader $ Generic.mkSlotLeader (ioShelley iopts) (Generic.unKeyHashRaw $ Generic.blkSlotLeader blk) (eitherToMaybe mPhid)
    blkId <-
      insertBlockAndCache syncEnv $
        DB.Block
          { DB.blockHash = Generic.blkHash blk
          , DB.blockEpochNo = Just $ unEpochNo epochNo
          , DB.blockSlotNo = Just $ unSlotNo (Generic.blkSlotNo blk)
          , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
          , DB.blockBlockNo = Just $ unBlockNo (Generic.blkBlockNo blk)
          , DB.blockPreviousId = Just pbid
          , DB.blockSlotLeaderId = slid
          , DB.blockSize = Generic.blkSize blk
          , DB.blockTime = sdSlotTime details
          , DB.blockTxCount = fromIntegral $ length (Generic.blkTxs blk)
          , DB.blockProtoMajor = getVersion $ Ledger.pvMajor (Generic.blkProto blk)
          , DB.blockProtoMinor = fromIntegral $ Ledger.pvMinor (Generic.blkProto blk)
          , -- Shelley specific
            DB.blockVrfKey = Just $ Generic.blkVrfKey blk
          , DB.blockOpCert = Just $ Generic.blkOpCert blk
          , DB.blockOpCertCounter = Just $ Generic.blkOpCertCounter blk
          , -- Leios (Dijkstra)
            DB.blockHasLeiosCert = Generic.blkHasLeiosCert blk
          , DB.blockEbAnnouncementHash = ebHashBytes . ebAnnouncementHash <$> Generic.blkLeiosEbAnnouncement blk
          , DB.blockEbAnnouncementSize = ebAnnouncementSize <$> Generic.blkLeiosEbAnnouncement blk
          , DB.blockLeiosCertSigners = serialize' . encodeBitField . leiosCertSigners <$> Generic.blkLeiosCert blk
          , DB.blockLeiosCertSignature = leiosSignatureToBytes . leiosCertSignature <$> Generic.blkLeiosCert blk
          }

    -- Leios: resolve the cert's signer bitfield against the committee (the stake-ordered pool
    -- distribution of the parent ledger state — apOldLedger, which is exactly the committee
    -- consensus verified the cert against) and record one row per signing pool.
    forM_ (Generic.blkLeiosCert blk) $ \cert ->
      whenStrictJust (apOldLedger applyResult) $
        insertLeiosCertSigners syncEnv blkId cert

    let zippedTx = zip [0 ..] (Generic.blkTxs blk)
    let txInserter = insertTx syncEnv isMember blkId (sdEpochNo details) (Generic.blkSlotNo blk) applyResult
    blockGroupedData <- foldM (\gp (idx, tx) -> txInserter idx tx gp (Generic.blkEra blk)) mempty zippedTx

    minIds <- insertBlockGroupedData syncEnv blockGroupedData

    when withinHalfHour $
      insertReverseIndex blkId minIds

    liftIO $ do
      let epoch = unEpochNo epochNo
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)

      when (withinTwoMins && slotWithinEpoch /= 0 && unBlockNo (Generic.blkBlockNo blk) `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ renderInsertName (Generic.blkEra blk)
            , ": continuing epoch "
            , textShow epoch
            , " (slot "
            , textShow slotWithinEpoch
            , "/"
            , textShow (unEpochSize $ sdEpochSize details)
            , ")"
            ]
      logger tracer $
        mconcat
          [ renderInsertName (Generic.blkEra blk)
          , ": epoch "
          , textShow (unEpochNo epochNo)
          , ", slot "
          , textShow (unSlotNo $ Generic.blkSlotNo blk)
          , ", block "
          , textShow (unBlockNo $ Generic.blkBlockNo blk)
          , ", hash "
          , renderByteArray (Generic.blkHash blk)
          ]

    whenStrictJust (apNewEpoch applyResult) $ \newEpoch -> do
      insertOnNewEpoch syncEnv blkId (Generic.blkSlotNo blk) epochNo newEpoch

    insertStakeSlice syncEnv $ apStakeSlice applyResult

    when (ioGov iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo blk) `mod` 10000 == 0)) $
      lift $
        insertOffChainVoteResults tracer (envOffChainVoteResultQueue syncEnv)

    when (ioOffChainPoolData iopts && (withinHalfHour || unBlockNo (Generic.blkBlockNo blk) `mod` 10000 == 0)) $
      lift $
        insertOffChainPoolResults tracer (envOffChainPoolResultQueue syncEnv)
  where
    iopts = getInsertOptions syncEnv

    logger :: Trace IO a -> a -> IO ()
    logger
      | shouldLog = logInfo
      | withinTwoMins = logInfo
      | unBlockNo (Generic.blkBlockNo blk) `mod` 5000 == 0 = logInfo
      | otherwise = logDebug

    renderInsertName :: Generic.BlockEra -> Text
    renderInsertName eraText =
      mconcat ["Insert ", textShow eraText, " Block"]

    renderErrorMessage :: Generic.BlockEra -> Text
    renderErrorMessage eraText =
      case eraText of
        Generic.Shelley -> "insertBlockForEra"
        other -> mconcat ["insertBlockForEra(", textShow other, ")"]

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    cache :: CacheStatus
    cache = envCache syncEnv

-- | Resolve a block's LeiosCert signer bitfield to the signing pools and insert one
-- 'DB.LeiosCertSigner' row per signer. The committee is the "everyone votes" stake-ordered
-- pool distribution of the parent ledger state ('apOldLedger'): seat @i@ (bit @i@, MSB-first)
-- is the @i@-th pool when pools are sorted by ascending active stake (ties by pool-id) — exactly
-- how the ledger's @mkCommitteeEveryoneVotes@ orders the committee.
insertLeiosCertSigners ::
  SyncEnv ->
  DB.BlockId ->
  LeiosCert ->
  CardanoLedgerState ->
  ExceptT SyncNodeError DB.DbM ()
insertLeiosCertSigners syncEnv blkId cert oldLedger =
  case committeeOrder oldLedger of
    Nothing ->
      liftIO $ logInfo trce "leios-resolve: no Dijkstra committee in parent ledger state"
    Just ordered -> do
      let seats = bitFieldSetBits (leiosCertSigners cert) (length ordered)
      liftIO $
        logInfo trce $
          "leios-resolve: committee=" <> textShow (length ordered) <> " signer-seats=" <> textShow seats
      forM_ seats $ \seat ->
        case drop seat ordered of
          [] -> liftIO $ logInfo trce $ "leios-resolve: seat " <> textShow seat <> " beyond committee"
          ((poolId, _) : _) -> do
            ePhid <- queryPoolKeyWithCache syncEnv UpdateCache poolId
            case ePhid of
              Left _ ->
                liftIO $ logInfo trce $ "leios-resolve: seat " <> textShow seat <> " pool not in db"
              Right phid ->
                void $
                  lift $
                    DB.insertLeiosCertSigner $
                      DB.LeiosCertSigner
                        { DB.leiosCertSignerBlockId = blkId
                        , DB.leiosCertSignerPoolHashId = phid
                        , DB.leiosCertSignerSeatIndex = fromIntegral seat
                        }
  where
    trce = getTrace syncEnv

-- | The Leios committee ordering from the parent ledger state's active pool distribution
-- ('nesPd'): pools sorted by ascending normalised stake, ties broken by pool-id (Map order).
-- 'Nothing' for non-Dijkstra parent states (which never carry a cert).
committeeOrder cls =
  case ledgerState (clsState cls) of
    LedgerStateDijkstra dls ->
      let nes = Consensus.shelleyLedgerState dls
          pd = Shelley.nesPd nes ^. LState.poolDistrDistrL
       in Just $ sortOn (LState.individualPoolStake . snd) (Map.toList pd)
    _ -> Nothing

-- | Set-bit indices of a LeiosCert signer bitfield, MSB-first, over @n@ committee seats.
-- Mirrors the ledger's private @bitFieldMembers@: seat @i@ is byte @i \`div\` 8@, bit @7 - (i \`mod\` 8)@.
bitFieldSetBits :: BitField -> Int -> [Int]
bitFieldSetBits bf n =
  [ i
  | i <- [0 .. n - 1]
  , let byteIx = i `div` 8
  , byteIx < BS.length raw
  , testBit (BS.index raw byteIx) (7 - (i `mod` 8))
  ]
  where
    raw = either (const BS.empty) identity (decodeFull' (serialize' (encodeBitField bf)))
