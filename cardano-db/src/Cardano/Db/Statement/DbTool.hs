{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.DbTool where

import Cardano.Prelude (ByteString, MonadIO (..), Proxy (..), Word64)
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Time (UTCTime)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core as SC
import qualified Cardano.Db.Schema.Core as SVC
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Schema.Types (utcTimeAsTimestampDecoder, utcTimeAsTimestampEncoder)
import Cardano.Db.Schema.Variants (TxOutVariantType (..), TxOutW (..), UtxoQueryResult (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC
import Cardano.Db.Statement.Function.Core (mkDbCallStack, runDbSessionMain)
import Cardano.Db.Statement.Function.Query (adaDecoder)
import Cardano.Db.Statement.Types (tableName)
import Cardano.Db.Types (Ada (..), DbAction, DbLovelace, dbLovelaceDecoder, lovelaceToAda)
import Data.Fixed (Fixed (..))

------------------------------------------------------------------------------------------------------------
-- DbTool Epcoh
------------------------------------------------------------------------------------------------------------

-- | Query delegation for specific address and epoch
queryDelegationForEpochStmt :: HsqlStmt.Statement (Text.Text, Word64) (Maybe (Id.StakeAddressId, UTCTime, DbLovelace, Id.PoolHashId))
queryDelegationForEpochStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.text)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]
    decoder = HsqlD.rowMaybe $ do
      addrId <- Id.idDecoder Id.StakeAddressId
      endTime <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      amount <- dbLovelaceDecoder
      poolId <- Id.idDecoder Id.PoolHashId
      pure (addrId, endTime, amount, poolId)
    epochTable = tableName (Proxy @SC.Epoch)
    epochStakeTable = tableName (Proxy @SC.EpochState)
    stakeAddressTable = tableName (Proxy @SC.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT es.addr_id, ep.end_time, es.amount, es.pool_id"
          , " FROM " <> epochTable <> " ep"
          , " INNER JOIN " <> epochStakeTable <> " es ON ep.no = es.epoch_no"
          , " INNER JOIN " <> stakeAddressTable <> " saddr ON saddr.id = es.addr_id"
          , " WHERE saddr.view = $1"
          , " AND es.epoch_no <= $2"
          , " ORDER BY es.epoch_no DESC"
          , " LIMIT 1"
          ]

queryDelegationForEpoch :: MonadIO m => Text.Text -> Word64 -> DbAction m (Maybe (Id.StakeAddressId, UTCTime, DbLovelace, Id.PoolHashId))
queryDelegationForEpoch address epochNum =
  runDbSessionMain (mkDbCallStack "queryDelegationForEpoch") $
    HsqlSes.statement (address, epochNum) queryDelegationForEpochStmt

------------------------------------------------------------------------------------------------------------

queryBlockNoListStmt :: HsqlStmt.Statement (Word64, Word64) [Word64]
queryBlockNoListStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTableN = tableName (Proxy @SCB.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT block_no"
          , " FROM " <> blockTableN
          , " WHERE block_no IS NOT NULL"
          , " AND block_no > $1"
          , " ORDER BY block_no ASC"
          , " LIMIT $2"
          ]
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]
    decoder = HsqlD.rowList (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryBlockNoList :: MonadIO m => Word64 -> Word64 -> DbAction m [Word64]
queryBlockNoList start count =
  runDbSessionMain (mkDbCallStack "queryBlockNoList") $
    HsqlSes.statement (start, count) queryBlockNoListStmt

------------------------------------------------------------------------------------------------------------
queryBlockTimestampsStmt :: HsqlStmt.Statement (Word64, Word64) [UTCTime]
queryBlockTimestampsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTableN = tableName (Proxy @SCB.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT time"
          , " FROM " <> blockTableN
          , " WHERE block_no IS NOT NULL"
          , " AND block_no > $1"
          , " ORDER BY block_no ASC"
          , " LIMIT $2"
          ]
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]
    decoder = HsqlD.rowList (HsqlD.column $ HsqlD.nonNullable utcTimeAsTimestampDecoder)

queryBlockTimestamps :: MonadIO m => Word64 -> Word64 -> DbAction m [UTCTime]
queryBlockTimestamps start count =
  runDbSessionMain (mkDbCallStack "queryBlockTimestamps") $
    HsqlSes.statement (start, count) queryBlockTimestampsStmt

------------------------------------------------------------------------------------------------------------
queryBlocksTimeAftersStmt :: HsqlStmt.Statement UTCTime [(Maybe Word64, Maybe Word64, UTCTime)]
queryBlocksTimeAftersStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTableN = tableName (Proxy @SCB.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT epoch_no, block_no, time"
          , " FROM " <> blockTableN
          , " WHERE time > $1"
          , " ORDER BY time DESC"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable utcTimeAsTimestampEncoder)
    decoder = HsqlD.rowList $ do
      epochNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      blockNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      pure (epochNo, blockNo, time)

queryBlocksTimeAfters :: MonadIO m => UTCTime -> DbAction m [(Maybe Word64, Maybe Word64, UTCTime)]
queryBlocksTimeAfters now =
  runDbSessionMain (mkDbCallStack "queryBlocksTimeAfters") $
    HsqlSes.statement now queryBlocksTimeAftersStmt

------------------------------------------------------------------------------------------------------------
queryLatestMemberRewardEpochNoStmt :: HsqlStmt.Statement () (Maybe Word64)
queryLatestMemberRewardEpochNoStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
    blockTable = tableName (Proxy @SCB.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT MAX(" <> blockTable <> ".epoch_no)"
          , " FROM " <> blockTable
          , " WHERE " <> blockTable <> ".epoch_no IS NOT NULL"
          ]

queryLatestMemberRewardEpochNo :: MonadIO m => DbAction m Word64
queryLatestMemberRewardEpochNo = do
  result <-
    runDbSessionMain (mkDbCallStack "queryLatestMemberRewardEpochNo") $
      HsqlSes.statement () queryLatestMemberRewardEpochNoStmt
  pure $ maybe 0 (\x -> if x >= 2 then x - 2 else 0) result

--------------------------------------------------------------------------------

-- | Query reward amount for epoch and stake address
queryRewardAmountStmt :: HsqlStmt.Statement (Word64, Id.StakeAddressId) (Maybe DbLovelace)
queryRewardAmountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        , snd >$< Id.idEncoder Id.getStakeAddressId
        ]
    decoder = HsqlD.rowMaybe dbLovelaceDecoder
    epochTable = tableName (Proxy @SC.Epoch)
    rewardTable = tableName (Proxy @SC.Reward)
    stakeAddressTable = tableName (Proxy @SC.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT reward.amount"
          , " FROM " <> epochTable <> " ep"
          , " INNER JOIN " <> rewardTable <> " reward ON ep.no = reward.earned_epoch"
          , " INNER JOIN " <> stakeAddressTable <> " saddr ON saddr.id = reward.addr_id"
          , " WHERE ep.no = $1"
          , " AND saddr.id = $2"
          , " ORDER BY ep.no ASC"
          ]

queryRewardAmount :: MonadIO m => Word64 -> Id.StakeAddressId -> DbAction m (Maybe DbLovelace)
queryRewardAmount epochNo saId =
  runDbSessionMain (mkDbCallStack "queryRewardAmount") $
    HsqlSes.statement (epochNo, saId) queryRewardAmountStmt

------------------------------------------------------------------------------------------------------------

-- | Query delegation history for stake address
queryDelegationHistoryStmt :: HsqlStmt.Statement (Text.Text, Word64) [(Id.StakeAddressId, Word64, UTCTime, DbLovelace, Id.PoolHashId)]
queryDelegationHistoryStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable HsqlE.text)
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]
    decoder = HsqlD.rowList $ do
      addrId <- Id.idDecoder Id.StakeAddressId
      epochNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      endTime <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      amount <- dbLovelaceDecoder
      poolId <- Id.idDecoder Id.PoolHashId
      pure (addrId, epochNo, endTime, amount, poolId)
    epochTable = tableName (Proxy @SC.Epoch)
    epochStakeTable = tableName (Proxy @SC.EpochStake)
    stakeAddressTable = tableName (Proxy @SC.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT es.addr_id, es.epoch_no, ep.end_time, es.amount, es.pool_id"
          , " FROM " <> epochTable <> " ep"
          , " INNER JOIN " <> epochStakeTable <> " es ON ep.no = es.epoch_no"
          , " INNER JOIN " <> stakeAddressTable <> " saddr ON saddr.id = es.addr_id"
          , " WHERE saddr.view = $1"
          , " AND es.epoch_no <= $2"
          ]

queryDelegationHistory :: MonadIO m => Text.Text -> Word64 -> DbAction m [(Id.StakeAddressId, Word64, UTCTime, DbLovelace, Id.PoolHashId)]
queryDelegationHistory address maxEpoch =
  runDbSessionMain (mkDbCallStack "queryDelegationHistory") $
    HsqlSes.statement (address, maxEpoch) queryDelegationHistoryStmt

------------------------------------------------------------------------------------------------------------
-- DbTool AdaPots
------------------------------------------------------------------------------------------------------------

-- | Query for the sum of AdaPots across all pots in an epoch used in DBTool
data AdaPotsSum = AdaPotsSum
  { apsEpochNo :: Word64
  , apsSum :: Word64
  }

queryAdaPotsSumStmt :: HsqlStmt.Statement () [AdaPotsSum]
queryAdaPotsSumStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    adaPotsTableN = tableName (Proxy @SC.AdaPots)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT epoch_no, "
          , "(treasury + reserves + rewards + utxo + deposits_stake + deposits_drep + deposits_proposal + fees) as total_sum"
          , " FROM " <> adaPotsTableN
          ]

    decoder = HsqlD.rowList $ do
      epochNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      totalSum <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure $ AdaPotsSum epochNo totalSum

queryAdaPotsSum :: MonadIO m => DbAction m [AdaPotsSum]
queryAdaPotsSum =
  runDbSessionMain (mkDbCallStack "queryAdaPotsSum") $
    HsqlSes.statement () queryAdaPotsSumStmt

------------------------------------------------------------------------------------------------------------
-- DbTool Pool
------------------------------------------------------------------------------------------------------------

queryPoolsWithoutOwnersStmt :: HsqlStmt.Statement () Int
queryPoolsWithoutOwnersStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    poolUpdateTableN = tableName (Proxy @SCP.PoolUpdate)
    poolOwnerTableN = tableName (Proxy @SCP.PoolOwner)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::int"
          , " FROM " <> poolUpdateTableN <> " pupd"
          , " WHERE NOT EXISTS ("
          , "   SELECT 1 FROM " <> poolOwnerTableN <> " powner"
          , "   WHERE pupd.id = powner.pool_update_id"
          , " )"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)

queryPoolsWithoutOwners :: MonadIO m => DbAction m Int
queryPoolsWithoutOwners =
  runDbSessionMain (mkDbCallStack "queryPoolsWithoutOwners") $
    HsqlSes.statement () queryPoolsWithoutOwnersStmt

------------------------------------------------------------------------------------------------------------
-- DbTool TxOut
------------------------------------------------------------------------------------------------------------

queryUtxoAtSlotNoStmt :: HsqlStmt.Statement Word64 (Maybe Id.BlockId)
queryUtxoAtSlotNoStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM block WHERE slot_no = $1 LIMIT 1"
          ]
    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.BlockId)

queryUtxoAtSlotNo :: MonadIO m => TxOutVariantType -> Word64 -> DbAction m [UtxoQueryResult]
queryUtxoAtSlotNo txOutTableType slotNo = do
  runDbSessionMain (mkDbCallStack "queryUtxoAtSlotNo") $ do
    mBlockId <- HsqlSes.statement slotNo queryUtxoAtSlotNoStmt
    case mBlockId of
      Nothing -> pure []
      Just blockId -> HsqlSes.statement blockId $ case txOutTableType of
        TxOutVariantCore -> queryUtxoAtBlockIdCoreStmt
        TxOutVariantAddress -> queryUtxoAtBlockIdVariantStmt

utxoAtBlockIdWhereClause :: Text.Text
utxoAtBlockIdWhereClause =
  Text.concat
    [ " WHERE txout.tx_id IN ("
    , "   SELECT tx.id FROM tx WHERE tx.block_id <= $1"
    , " )"
    , " AND (blk.block_no IS NULL OR blk.id > $1)"
    , " AND tx2.hash IS NOT NULL"
    ]

queryUtxoAtBlockIdCoreStmt :: HsqlStmt.Statement Id.BlockId [UtxoQueryResult]
queryUtxoAtBlockIdCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*, txout.address, tx2.hash"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , utxoAtBlockIdWhereClause
          ]

    encoder = Id.idEncoder Id.getBlockId

    decoder = HsqlD.rowList $ do
      txOut <- SVC.txOutCoreDecoder
      address <- HsqlD.column (HsqlD.nonNullable HsqlD.text)
      txHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure $
        UtxoQueryResult
          { utxoTxOutW = VCTxOutW txOut
          , utxoAddress = address
          , utxoTxHash = txHash
          }

queryUtxoAtBlockIdVariantStmt :: HsqlStmt.Statement Id.BlockId [UtxoQueryResult]
queryUtxoAtBlockIdVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txout.*, addr.*, tx2.hash"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , " INNER JOIN address addr ON txout.address_id = addr.id"
          , utxoAtBlockIdWhereClause
          ]

    encoder = Id.idEncoder Id.getBlockId

    decoder = HsqlD.rowList $ do
      txOut <- SVA.txOutAddressDecoder
      addr <- SVA.addressDecoder
      txHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      pure $
        UtxoQueryResult
          { utxoTxOutW = VATxOutW txOut (Just addr)
          , utxoAddress = SVA.addressAddress addr
          , utxoTxHash = txHash
          }

-- Individual functions for backward compatibility
queryUtxoAtBlockId :: MonadIO m => TxOutVariantType -> Id.BlockId -> DbAction m [UtxoQueryResult]
queryUtxoAtBlockId txOutTableType blockId =
  runDbSessionMain (mkDbCallStack "queryUtxoAtBlockId") $
    HsqlSes.statement blockId $ case txOutTableType of
      TxOutVariantCore -> queryUtxoAtBlockIdCoreStmt
      TxOutVariantAddress -> queryUtxoAtBlockIdVariantStmt

------------------------------------------------------------------------------------------------------------

-- Query to get block ID at a specific slot
queryBlockIdAtSlotStmt :: HsqlStmt.Statement Word64 (Maybe Id.BlockId)
queryBlockIdAtSlotStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM block"
          , " WHERE slot_no = $1"
          ]

    encoder = HsqlE.param $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8
    decoder = HsqlD.rowMaybe $ Id.idDecoder Id.BlockId

-- Shared WHERE clause for address balance queries
addressBalanceWhereClause :: Text.Text
addressBalanceWhereClause =
  Text.concat
    [ " WHERE txout.tx_id IN ("
    , "   SELECT tx.id FROM tx"
    , "   WHERE tx.block_id IN ("
    , "     SELECT block.id FROM block"
    , "     WHERE block.id <= $1"
    , "   )"
    , " )"
    , " AND (blk.block_no IS NULL OR blk.id > $1)"
    ]

-- Query to get address balance for Core variant
queryAddressBalanceAtBlockIdCoreStmt :: HsqlStmt.Statement (Id.BlockId, Text.Text) Ada
queryAddressBalanceAtBlockIdCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(txout.value), 0)::bigint"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , addressBalanceWhereClause
          , " AND txout.address = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getBlockId)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable HsqlE.text)

    decoder =
      HsqlD.singleRow $
        fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))

-- Query to get address balance for Variant variant
queryAddressBalanceAtBlockIdVariantStmt :: HsqlStmt.Statement (Id.BlockId, Text.Text) Ada
queryAddressBalanceAtBlockIdVariantStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(txout.value), 0)::bigint"
          , " FROM tx_out txout"
          , " LEFT JOIN tx_in txin ON txout.tx_id = txin.tx_out_id AND txout.index = txin.tx_out_index"
          , " LEFT JOIN tx tx1 ON txin.tx_in_id = tx1.id"
          , " LEFT JOIN block blk ON tx1.block_id = blk.id"
          , " LEFT JOIN tx tx2 ON txout.tx_id = tx2.id"
          , " INNER JOIN address addr ON txout.address_id = addr.id"
          , addressBalanceWhereClause
          , " AND addr.address = $2"
          ]

    encoder =
      contramap fst (Id.idEncoder Id.getBlockId)
        <> contramap snd (HsqlE.param $ HsqlE.nonNullable HsqlE.text)

    decoder =
      HsqlD.singleRow $
        fromMaybe (Ada 0) <$> HsqlD.column (HsqlD.nullable (Ada . fromIntegral <$> HsqlD.int8))

-- Main query function
queryAddressBalanceAtSlot :: MonadIO m => TxOutVariantType -> Text.Text -> Word64 -> DbAction m Ada
queryAddressBalanceAtSlot txOutVariantType addr slotNo = do
  let dbCallStack = mkDbCallStack "queryAddressBalanceAtSlot"

  -- First get the block ID for the slot
  mBlockId <-
    runDbSessionMain dbCallStack $
      HsqlSes.statement slotNo queryBlockIdAtSlotStmt

  -- If no block at that slot, return 0
  case mBlockId of
    Nothing -> pure $ Ada 0
    Just blockId ->
      case txOutVariantType of
        TxOutVariantCore ->
          runDbSessionMain (mkDbCallStack "queryAddressBalanceAtBlockIdCore") $
            HsqlSes.statement (blockId, addr) queryAddressBalanceAtBlockIdCoreStmt
        TxOutVariantAddress ->
          runDbSessionMain (mkDbCallStack "queryAddressBalanceAtBlockIdVariant") $
            HsqlSes.statement (blockId, addr) queryAddressBalanceAtBlockIdVariantStmt

--------------------------------------------------------------------------------
-- Cardano DbTool - Transactions
--------------------------------------------------------------------------------

-- | Query stake address ID by view/address text
queryStakeAddressIdStmt :: HsqlStmt.Statement Text.Text (Maybe Id.StakeAddressId)
queryStakeAddressIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.text)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.StakeAddressId)
    stakeAddressTable = tableName (Proxy @SVC.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM " <> stakeAddressTable
          , " WHERE view = $1"
          ]

queryStakeAddressId :: MonadIO m => Text.Text -> DbAction m (Maybe Id.StakeAddressId)
queryStakeAddressId address =
  runDbSessionMain (mkDbCallStack "queryStakeAddressId") $
    HsqlSes.statement address queryStakeAddressIdStmt

--------------------------------------------------------------------------------

-- | Query input transactions for Core variant
queryInputTransactionsCoreStmt :: HsqlStmt.Statement Id.StakeAddressId [(ByteString, UTCTime, DbLovelace)]
queryInputTransactionsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowList $ do
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      value <- dbLovelaceDecoder
      pure (hash, time, value)
    txTable = tableName (Proxy @SVC.Tx)
    txOutCoreTable = tableName (Proxy @SVC.TxOutCore)
    blockTable = tableName (Proxy @SVC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT " <> txTable <> ".hash, " <> blockTable <> ".time, " <> txOutCoreTable <> ".value"
          , " FROM " <> txTable
          , " INNER JOIN " <> txOutCoreTable <> " ON " <> txOutCoreTable <> ".tx_id = " <> txTable <> ".id"
          , " INNER JOIN " <> blockTable <> " ON " <> txTable <> ".block_id = " <> blockTable <> ".id"
          , " WHERE " <> txOutCoreTable <> ".stake_address_id = $1"
          ]

queryInputTransactionsCore :: MonadIO m => Id.StakeAddressId -> DbAction m [(ByteString, UTCTime, DbLovelace)]
queryInputTransactionsCore saId =
  runDbSessionMain (mkDbCallStack "queryInputTransactionsCore") $
    HsqlSes.statement saId queryInputTransactionsCoreStmt

--------------------------------------------------------------------------------

-- | Query input transactions for Address variant
queryInputTransactionsAddressStmt :: HsqlStmt.Statement Id.StakeAddressId [(ByteString, UTCTime, DbLovelace)]
queryInputTransactionsAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowList $ do
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      value <- dbLovelaceDecoder
      pure (hash, time, value)
    txTable = tableName (Proxy @SVC.Tx)
    txOutAddressTable = tableName (Proxy @SVA.TxOutAddress)
    addressTable = tableName (Proxy @SVA.Address)
    blockTable = tableName (Proxy @SVC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT " <> txTable <> ".hash, " <> blockTable <> ".time, " <> txOutAddressTable <> ".value"
          , " FROM " <> txTable
          , " INNER JOIN " <> txOutAddressTable <> " ON " <> txOutAddressTable <> ".tx_id = " <> txTable <> ".id"
          , " INNER JOIN " <> addressTable <> " ON " <> txOutAddressTable <> ".address_id = " <> addressTable <> ".id"
          , " INNER JOIN " <> blockTable <> " ON " <> txTable <> ".block_id = " <> blockTable <> ".id"
          , " WHERE " <> addressTable <> ".stake_address_id = $1"
          ]

queryInputTransactionsAddress :: MonadIO m => Id.StakeAddressId -> DbAction m [(ByteString, UTCTime, DbLovelace)]
queryInputTransactionsAddress saId =
  runDbSessionMain (mkDbCallStack "queryInputTransactionsAddress") $
    HsqlSes.statement saId queryInputTransactionsAddressStmt

--------------------------------------------------------------------------------

-- | Query withdrawal transactions
queryWithdrawalTransactionsStmt :: HsqlStmt.Statement Id.StakeAddressId [(ByteString, UTCTime, DbLovelace)]
queryWithdrawalTransactionsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowList $ do
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      amount <- dbLovelaceDecoder
      pure (hash, time, amount)
    txTable = tableName (Proxy @SVC.Tx)
    blockTable = tableName (Proxy @SVC.Block)
    withdrawalTable = tableName (Proxy @SVC.Withdrawal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT " <> txTable <> ".hash, " <> blockTable <> ".time, " <> withdrawalTable <> ".amount"
          , " FROM " <> txTable
          , " INNER JOIN " <> blockTable <> " ON " <> txTable <> ".block_id = " <> blockTable <> ".id"
          , " INNER JOIN " <> withdrawalTable <> " ON " <> withdrawalTable <> ".tx_id = " <> txTable <> ".id"
          , " WHERE " <> withdrawalTable <> ".addr_id = $1"
          ]

queryWithdrawalTransactions :: MonadIO m => Id.StakeAddressId -> DbAction m [(ByteString, UTCTime, DbLovelace)]
queryWithdrawalTransactions saId =
  runDbSessionMain (mkDbCallStack "queryWithdrawalTransactions") $
    HsqlSes.statement saId queryWithdrawalTransactionsStmt

--------------------------------------------------------------------------------

-- | Query output transactions for Core variant
queryOutputTransactionsCoreStmt :: HsqlStmt.Statement Id.StakeAddressId [(ByteString, UTCTime, DbLovelace)]
queryOutputTransactionsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowList $ do
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      value <- dbLovelaceDecoder
      pure (hash, time, value)
    txOutCoreTable = tableName (Proxy @SVC.TxOutCore)
    txTable = tableName (Proxy @SVC.Tx)
    txInTable = tableName (Proxy @SVC.TxIn)
    blockTable = tableName (Proxy @SVC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txOutTx.hash, " <> blockTable <> ".time, " <> txOutCoreTable <> ".value"
          , " FROM " <> txOutCoreTable
          , " INNER JOIN " <> txTable <> " txInTx ON " <> txOutCoreTable <> ".tx_id = txInTx.id"
          , " INNER JOIN " <> txInTable <> " ON " <> txInTable <> ".tx_out_id = txInTx.id AND " <> txInTable <> ".tx_out_index = " <> txOutCoreTable <> ".index"
          , " INNER JOIN " <> txTable <> " txOutTx ON txOutTx.id = " <> txInTable <> ".tx_in_id"
          , " INNER JOIN " <> blockTable <> " ON txOutTx.block_id = " <> blockTable <> ".id"
          , " WHERE " <> txOutCoreTable <> ".stake_address_id = $1"
          ]

queryOutputTransactionsCore :: MonadIO m => Id.StakeAddressId -> DbAction m [(ByteString, UTCTime, DbLovelace)]
queryOutputTransactionsCore saId =
  runDbSessionMain (mkDbCallStack "queryOutputTransactionsCore") $
    HsqlSes.statement saId queryOutputTransactionsCoreStmt

--------------------------------------------------------------------------------

-- | Query output transactions for Address variant
queryOutputTransactionsAddressStmt :: HsqlStmt.Statement Id.StakeAddressId [(ByteString, UTCTime, DbLovelace)]
queryOutputTransactionsAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowList $ do
      hash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      time <- HsqlD.column (HsqlD.nonNullable utcTimeAsTimestampDecoder)
      value <- dbLovelaceDecoder
      pure (hash, time, value)
    txOutAddressTable = tableName (Proxy @SVA.TxOutAddress)
    addressTable = tableName (Proxy @SVA.Address)
    txTable = tableName (Proxy @SVC.Tx)
    txInTable = tableName (Proxy @SVC.TxIn)
    blockTable = tableName (Proxy @SVC.Block)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT txOutTx.hash, " <> blockTable <> ".time, " <> txOutAddressTable <> ".value"
          , " FROM " <> txOutAddressTable
          , " INNER JOIN " <> addressTable <> " ON " <> txOutAddressTable <> ".address_id = " <> addressTable <> ".id"
          , " INNER JOIN " <> txTable <> " txInTx ON " <> txOutAddressTable <> ".tx_id = txInTx.id"
          , " INNER JOIN " <> txInTable <> " ON " <> txInTable <> ".tx_out_id = txInTx.id AND " <> txInTable <> ".tx_out_index = " <> txOutAddressTable <> ".index"
          , " INNER JOIN " <> txTable <> " txOutTx ON txOutTx.id = " <> txInTable <> ".tx_in_id"
          , " INNER JOIN " <> blockTable <> " ON txOutTx.block_id = " <> blockTable <> ".id"
          , " WHERE " <> addressTable <> ".stake_address_id = $1"
          ]

queryOutputTransactionsAddress :: MonadIO m => Id.StakeAddressId -> DbAction m [(ByteString, UTCTime, DbLovelace)]
queryOutputTransactionsAddress saId =
  runDbSessionMain (mkDbCallStack "queryOutputTransactionsAddress") $
    HsqlSes.statement saId queryOutputTransactionsAddressStmt

--------------------------------------------------------------------------------
-- Cardano DbTool - Balance
--------------------------------------------------------------------------------

-- | Query input sum for Core variant
queryInputsSumCoreStmt :: HsqlStmt.Statement Id.StakeAddressId Ada
queryInputsSumCoreStmt =
  HsqlStmt.Statement sql encoder (HsqlD.singleRow adaDecoder) True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    txOutCoreTable = tableName (Proxy @SVC.TxOutCore)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> txOutCoreTable <> ".value), 0)::bigint"
          , " FROM " <> txOutCoreTable
          , " WHERE " <> txOutCoreTable <> ".stake_address_id = $1"
          ]

queryInputsSumCore :: MonadIO m => Id.StakeAddressId -> DbAction m Ada
queryInputsSumCore saId =
  runDbSessionMain (mkDbCallStack "queryInputsSumCore") $
    HsqlSes.statement saId queryInputsSumCoreStmt

--------------------------------------------------------------------------------

-- | Query input sum for Address variant
queryInputsSumAddressStmt :: HsqlStmt.Statement Id.StakeAddressId Ada
queryInputsSumAddressStmt =
  HsqlStmt.Statement sql encoder (HsqlD.singleRow adaDecoder) True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    txOutAddressTable = tableName (Proxy @SVA.TxOutAddress)
    addressTable = tableName (Proxy @SVA.Address)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> txOutAddressTable <> ".value), 0)::bigint"
          , " FROM " <> txOutAddressTable
          , " INNER JOIN " <> addressTable <> " ON " <> txOutAddressTable <> ".address_id = " <> addressTable <> ".id"
          , " WHERE " <> addressTable <> ".stake_address_id = $1"
          ]

queryInputsSumAddress :: MonadIO m => Id.StakeAddressId -> DbAction m Ada
queryInputsSumAddress saId =
  runDbSessionMain (mkDbCallStack "queryInputsSumAddress") $
    HsqlSes.statement saId queryInputsSumAddressStmt

--------------------------------------------------------------------------------

-- | Query rewards sum
queryRewardsSumStmt :: HsqlStmt.Statement (Id.StakeAddressId, Word64) Ada
queryRewardsSumStmt =
  HsqlStmt.Statement sql encoder (HsqlD.singleRow adaDecoder) True
  where
    encoder =
      mconcat
        [ fst >$< Id.idEncoder Id.getStakeAddressId
        , snd >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        ]
    rewardTable = tableName (Proxy @SVC.Reward)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> rewardTable <> ".amount), 0)::bigint"
          , " FROM " <> rewardTable
          , " WHERE " <> rewardTable <> ".addr_id = $1"
          , " AND " <> rewardTable <> ".spendable_epoch <= $2"
          ]

queryRewardsSum :: MonadIO m => Id.StakeAddressId -> Word64 -> DbAction m Ada
queryRewardsSum saId currentEpoch =
  runDbSessionMain (mkDbCallStack "queryRewardsSum") $
    HsqlSes.statement (saId, currentEpoch) queryRewardsSumStmt

--------------------------------------------------------------------------------

-- | Query withdrawals sum
queryWithdrawalsSumStmt :: HsqlStmt.Statement Id.StakeAddressId Ada
queryWithdrawalsSumStmt =
  HsqlStmt.Statement sql encoder (HsqlD.singleRow adaDecoder) True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    withdrawalTable = tableName (Proxy @SVC.Withdrawal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(" <> withdrawalTable <> ".amount), 0)::bigint"
          , " FROM " <> withdrawalTable
          , " WHERE " <> withdrawalTable <> ".addr_id = $1"
          ]

queryWithdrawalsSum :: MonadIO m => Id.StakeAddressId -> DbAction m Ada
queryWithdrawalsSum saId =
  runDbSessionMain (mkDbCallStack "queryWithdrawalsSum") $
    HsqlSes.statement saId queryWithdrawalsSumStmt

--------------------------------------------------------------------------------

-- | Query outputs, fees, and deposit for Core variant
queryOutputsCoreStmt :: HsqlStmt.Statement Id.StakeAddressId (Ada, Ada, Ada)
queryOutputsCoreStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.singleRow $ do
      outputs <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      fees <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      deposit <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure
        ( lovelaceToAda (MkFixed $ fromIntegral outputs)
        , lovelaceToAda (MkFixed $ fromIntegral fees)
        , lovelaceToAda (MkFixed $ fromIntegral deposit)
        )
    txOutCoreTable = tableName (Proxy @SVC.TxOutCore)
    txTable = tableName (Proxy @SVC.Tx)
    txInTable = tableName (Proxy @SVC.TxIn)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT"
          , "  COALESCE(SUM(" <> txOutCoreTable <> ".value), 0)::bigint,"
          , "  COALESCE(SUM(" <> txTable <> ".fee), 0)::bigint,"
          , "  COALESCE(SUM(" <> txTable <> ".deposit), 0)::bigint"
          , " FROM " <> txOutCoreTable
          , " INNER JOIN " <> txTable <> " ON " <> txOutCoreTable <> ".tx_id = " <> txTable <> ".id"
          , " INNER JOIN " <> txInTable <> " ON " <> txInTable <> ".tx_out_id = " <> txTable <> ".id"
          , "   AND " <> txInTable <> ".tx_out_index = " <> txOutCoreTable <> ".index"
          , " WHERE " <> txOutCoreTable <> ".stake_address_id = $1"
          ]

queryOutputsCore :: MonadIO m => Id.StakeAddressId -> DbAction m (Ada, Ada, Ada)
queryOutputsCore saId =
  runDbSessionMain (mkDbCallStack "queryOutputsCore") $
    HsqlSes.statement saId queryOutputsCoreStmt

--------------------------------------------------------------------------------

-- | Query outputs, fees, and deposit for Address variant
queryOutputsAddressStmt :: HsqlStmt.Statement Id.StakeAddressId (Ada, Ada, Ada)
queryOutputsAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.singleRow $ do
      outputs <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      fees <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      deposit <- HsqlD.column (HsqlD.nonNullable HsqlD.int8)
      pure
        ( lovelaceToAda (MkFixed $ fromIntegral outputs)
        , lovelaceToAda (MkFixed $ fromIntegral fees)
        , lovelaceToAda (MkFixed $ fromIntegral deposit)
        )
    txOutAddressTable = tableName (Proxy @SVA.TxOutAddress)
    addressTable = tableName (Proxy @SVA.Address)
    txTable = tableName (Proxy @SVC.Tx)
    txInTable = tableName (Proxy @SVC.TxIn)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT"
          , "  COALESCE(SUM(" <> txOutAddressTable <> ".value), 0)::bigint,"
          , "  COALESCE(SUM(" <> txTable <> ".fee), 0)::bigint,"
          , "  COALESCE(SUM(" <> txTable <> ".deposit), 0)::bigint"
          , " FROM " <> txOutAddressTable
          , " INNER JOIN " <> addressTable <> " ON " <> txOutAddressTable <> ".address_id = " <> addressTable <> ".id"
          , " INNER JOIN " <> txTable <> " ON " <> txOutAddressTable <> ".tx_id = " <> txTable <> ".id"
          , " INNER JOIN " <> txInTable <> " ON " <> txInTable <> ".tx_out_id = " <> txTable <> ".id"
          , "   AND " <> txInTable <> ".tx_out_index = " <> txOutAddressTable <> ".index"
          , " WHERE " <> addressTable <> ".stake_address_id = $1"
          ]

queryOutputsAddress :: MonadIO m => Id.StakeAddressId -> DbAction m (Ada, Ada, Ada)
queryOutputsAddress saId =
  runDbSessionMain (mkDbCallStack "queryOutputsAddress") $
    HsqlSes.statement saId queryOutputsAddressStmt

--------------------------------------------------------------------------------

queryEpochBlockNumbersStmt :: HsqlStmt.Statement Word64 [(Word64, Word64)]
queryEpochBlockNumbersStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTableN = tableName (Proxy @SCB.Block)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(block_no, 0), tx_count"
          , " FROM " <> blockTableN
          , " WHERE epoch_no = $1"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)

    decoder = HsqlD.rowList $ do
      blockNo <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      txCount <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure (blockNo, txCount)

queryEpochBlockNumbers :: MonadIO m => Word64 -> DbAction m [(Word64, Word64)]
queryEpochBlockNumbers epoch =
  runDbSessionMain (mkDbCallStack "queryEpochBlockNumbers") $
    HsqlSes.statement epoch queryEpochBlockNumbersStmt
