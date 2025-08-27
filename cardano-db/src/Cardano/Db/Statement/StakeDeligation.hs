{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.StakeDeligation where

import Cardano.Prelude (ByteString, MonadIO, Proxy (..))
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Word (Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.EpochAndProtocol as SEP
import qualified Cardano.Db.Schema.Core.StakeDeligation as SS
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), bulkEncoder, mkDbCallStack, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk, insertBulkMaybeIgnore, insertBulkMaybeIgnoreWithConstraint)
import Cardano.Db.Statement.Function.Query (adaSumDecoder, countAll)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (Ada, DbAction, DbLovelace, RewardSource, dbLovelaceDecoder, rewardSourceDecoder, rewardSourceEncoder)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential (Ptr (..), SlotNo32 (..))
import Contravariant.Extras (contrazip2, contrazip4)
import qualified Hasql.Pipeline as HsqlP

--------------------------------------------------------------------------------
-- Deligation
--------------------------------------------------------------------------------
insertDelegationStmt :: HsqlStmt.Statement SS.Delegation Id.DelegationId
insertDelegationStmt =
  insert
    SS.delegationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DelegationId)

insertDelegation :: MonadIO m => SS.Delegation -> DbAction m Id.DelegationId
insertDelegation delegation =
  runDbSession (mkDbCallStack "insertDelegation") $ HsqlSes.statement delegation insertDelegationStmt

--------------------------------------------------------------------------------
-- Statement for querying delegations with non-null redeemer_id
queryDelegationScriptStmt :: HsqlStmt.Statement () [SS.Delegation]
queryDelegationScriptStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SS.Delegation)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> tableN
          , " WHERE redeemer_id IS NOT NULL"
          ]
    decoder = HsqlD.rowList SS.delegationDecoder

queryDelegationScript :: MonadIO m => DbAction m [SS.Delegation]
queryDelegationScript =
  runDbSession (mkDbCallStack "queryDelegationScript") $
    HsqlSes.statement () queryDelegationScriptStmt

--------------------------------------------------------------------------------
-- EpochStake
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertBulkEpochStakeStmt :: Bool -> HsqlStmt.Statement [SS.EpochStake] ()
insertBulkEpochStakeStmt dbConstraintEpochStake =
  insertBulkMaybeIgnore
    dbConstraintEpochStake
    extractEpochStake
    SS.epochStakeBulkEncoder
    NoResultBulk
  where
    extractEpochStake :: [SS.EpochStake] -> ([Id.StakeAddressId], [Id.PoolHashId], [DbLovelace], [Word64])
    extractEpochStake xs =
      ( map SS.epochStakeAddrId xs
      , map SS.epochStakePoolId xs
      , map SS.epochStakeAmount xs
      , map SS.epochStakeEpochNo xs
      )

insertBulkEpochStake :: MonadIO m => Bool -> [SS.EpochStake] -> DbAction m ()
insertBulkEpochStake dbConstraintEpochStake epochStakes =
  runDbSession (mkDbCallStack "insertBulkEpochStake") $
    HsqlSes.statement epochStakes $
      insertBulkEpochStakeStmt dbConstraintEpochStake

-- | QUERIES -------------------------------------------------------------------
queryEpochStakeCountStmt :: HsqlStmt.Statement Word64 Word64
queryEpochStakeCountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM epoch_stake"
          , " WHERE epoch_no = $1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryEpochStakeCount :: MonadIO m => Word64 -> DbAction m Word64
queryEpochStakeCount epoch =
  runDbSession (mkDbCallStack "queryEpochStakeCount") $
    HsqlSes.statement epoch queryEpochStakeCountStmt

--------------------------------------------------------------------------------
-- EpochProgress
--------------------------------------------------------------------------------

updateStakeProgressCompletedStmt :: HsqlStmt.Statement Word64 ()
updateStakeProgressCompletedStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    tableN = tableName (Proxy @SS.EpochStakeProgress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "INSERT INTO " <> tableN <> " (epoch_no, completed)"
          , " VALUES ($1, TRUE)"
          , " ON CONFLICT (epoch_no)"
          , " DO UPDATE SET completed = TRUE"
          ]

    encoder = fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8)
    decoder = HsqlD.noResult

updateStakeProgressCompleted :: MonadIO m => Word64 -> DbAction m ()
updateStakeProgressCompleted epoch =
  runDbSession (mkDbCallStack "updateStakeProgressCompleted") $
    HsqlSes.statement epoch updateStakeProgressCompletedStmt

--------------------------------------------------------------------------------
-- Reward
--------------------------------------------------------------------------------

-- | INSERT ---------------------------------------------------------------------
insertBulkRewardsStmt :: Bool -> HsqlStmt.Statement [SS.Reward] ()
insertBulkRewardsStmt dbConstraintRewards =
  if dbConstraintRewards
    then
      insertBulkMaybeIgnoreWithConstraint
        True
        "unique_reward"
        extractReward
        SS.rewardBulkEncoder
        NoResultBulk
    else
      insertBulk
        extractReward
        SS.rewardBulkEncoder
        NoResultBulk
  where
    extractReward :: [SS.Reward] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Id.PoolHashId])
    extractReward xs =
      ( map SS.rewardAddrId xs
      , map SS.rewardType xs
      , map SS.rewardAmount xs
      , map SS.rewardSpendableEpoch xs
      , map SS.rewardPoolId xs
      )

insertBulkRewards :: MonadIO m => Bool -> [SS.Reward] -> DbAction m ()
insertBulkRewards dbConstraintRewards rewards =
  runDbSession (mkDbCallStack "insertBulkRewards") $
    HsqlSes.statement rewards $
      insertBulkRewardsStmt dbConstraintRewards

-- | QUERY ---------------------------------------------------------------------
queryNormalEpochRewardCountStmt :: HsqlStmt.Statement Word64 Word64
queryNormalEpochRewardCountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint"
          , " FROM reward"
          , " WHERE spendable_epoch = $1"
          , " AND type IN ('member', 'leader')"
          ]

    encoder = HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
    decoder =
      HsqlD.singleRow $
        fromIntegral <$> HsqlD.column (HsqlD.nonNullable HsqlD.int8)

queryNormalEpochRewardCount :: MonadIO m => Word64 -> DbAction m Word64
queryNormalEpochRewardCount epochNum =
  runDbSession (mkDbCallStack "queryNormalEpochRewardCount") $
    HsqlSes.statement epochNum queryNormalEpochRewardCountStmt

--------------------------------------------------------------------------------
queryRewardCount :: MonadIO m => DbAction m Word64
queryRewardCount =
  runDbSession (mkDbCallStack "queryRewardCount") $
    HsqlSes.statement () (countAll @SS.Reward)

--------------------------------------------------------------------------------
queryRewardMapDataStmt :: HsqlStmt.Statement Word64 [(ByteString, RewardSource, DbLovelace)]
queryRewardMapDataStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    rewardTableN = tableName (Proxy @SS.Reward)
    stakeAddressTableN = tableName (Proxy @SS.StakeAddress)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT sa.hash_raw, r.type, r.amount"
          , " FROM " <> rewardTableN <> " r"
          , " INNER JOIN " <> stakeAddressTableN <> " sa ON r.addr_id = sa.id"
          , " WHERE r.spendable_epoch = $1"
          , " AND r.type != 'refund'"
          , " AND r.type != 'treasury'"
          , " AND r.type != 'reserves'"
          , " ORDER BY sa.hash_raw DESC"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int8))

    decoder = HsqlD.rowList $ do
      hashRaw <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      rewardType <- HsqlD.column (HsqlD.nonNullable rewardSourceDecoder)
      amount <- dbLovelaceDecoder
      pure (hashRaw, rewardType, amount)

queryRewardMapData :: MonadIO m => Word64 -> DbAction m [(ByteString, RewardSource, DbLovelace)]
queryRewardMapData epochNo =
  runDbSession (mkDbCallStack "queryRewardMapData") $
    HsqlSes.statement epochNo queryRewardMapDataStmt

-- Bulk delete statement
deleteRewardsBulkStmt :: HsqlStmt.Statement ([Id.StakeAddressId], [RewardSource], [Word64], [Id.PoolHashId]) ()
deleteRewardsBulkStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    rewardTableN = tableName (Proxy @SS.Reward)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "DELETE FROM " <> rewardTableN
          , " WHERE (addr_id, type, spendable_epoch, pool_id) IN ("
          , "  SELECT addr_id, reward_type::rewardtype, epoch, pool_id"
          , "  FROM UNNEST($1::bigint[], $2::text[], $3::bigint[], $4::bigint[]) AS t(addr_id, reward_type, epoch, pool_id)"
          , ")"
          ]

    encoder =
      contrazip4
        (bulkEncoder $ Id.idBulkEncoder Id.getStakeAddressId) -- addr_id
        (bulkEncoder $ HsqlE.nonNullable rewardSourceEncoder) -- type
        (bulkEncoder $ HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8) -- spendable_epoch
        (bulkEncoder $ Id.idBulkEncoder Id.getPoolHashId) -- pool_id

-- Public API function
deleteRewardsBulk ::
  MonadIO m =>
  ([Id.StakeAddressId], [RewardSource], [Word64], [Id.PoolHashId]) ->
  DbAction m ()
deleteRewardsBulk params =
  runDbSession (mkDbCallStack "deleteRewardsBulk") $
    HsqlSes.statement params deleteRewardsBulkStmt

--------------------------------------------------------------------------------
deleteOrphanedRewardsBulkStmt :: HsqlStmt.Statement (Word64, [Id.StakeAddressId]) ()
deleteOrphanedRewardsBulkStmt =
  HsqlStmt.Statement sql encoder HsqlD.noResult True
  where
    rewardTableN = tableName (Proxy @SS.Reward)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "DELETE FROM " <> rewardTableN
          , " WHERE spendable_epoch = $1"
          , " AND addr_id = ANY($2)"
          ]
    encoder =
      contrazip2
        (fromIntegral >$< HsqlE.param (HsqlE.nonNullable HsqlE.int8))
        (HsqlE.param $ HsqlE.nonNullable $ HsqlE.foldableArray (Id.idBulkEncoder Id.getStakeAddressId))

-- | Delete orphaned rewards in bulk
deleteOrphanedRewardsBulk ::
  MonadIO m =>
  Word64 ->
  [Id.StakeAddressId] ->
  DbAction m ()
deleteOrphanedRewardsBulk epochNo addrIds =
  runDbSession (mkDbCallStack "deleteOrphanedRewardsBulk") $
    HsqlSes.statement (epochNo, addrIds) deleteOrphanedRewardsBulkStmt

--------------------------------------------------------------------------------
-- RewardRest
--------------------------------------------------------------------------------
insertBulkRewardRestsStmt :: HsqlStmt.Statement [SS.RewardRest] ()
insertBulkRewardRestsStmt =
  insertBulk
    extractRewardRest
    SS.rewardRestBulkEncoder
    NoResultBulk
  where
    extractRewardRest :: [SS.RewardRest] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64])
    extractRewardRest xs =
      ( map SS.rewardRestAddrId xs
      , map SS.rewardRestType xs
      , map SS.rewardRestAmount xs
      , map SS.rewardRestSpendableEpoch xs
      )

insertBulkRewardRests :: MonadIO m => [SS.RewardRest] -> DbAction m ()
insertBulkRewardRests rewardRests =
  runDbSession (mkDbCallStack "insertBulkRewardRests") $
    HsqlSes.statement rewardRests insertBulkRewardRestsStmt

--------------------------------------------------------------------------------
queryRewardRestCount :: MonadIO m => DbAction m Word64
queryRewardRestCount =
  runDbSession (mkDbCallStack "queryRewardRestCount") $
    HsqlSes.statement () (countAll @SS.RewardRest)

--------------------------------------------------------------------------------
-- StakeAddress
--------------------------------------------------------------------------------
insertStakeAddressStmt :: HsqlStmt.Statement SS.StakeAddress Id.StakeAddressId
insertStakeAddressStmt =
  insertCheckUnique
    SS.stakeAddressEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.StakeAddressId)

insertStakeAddress :: MonadIO m => SS.StakeAddress -> DbAction m Id.StakeAddressId
insertStakeAddress stakeAddress =
  runDbSession (mkDbCallStack "insertStakeAddress") $
    HsqlSes.statement stakeAddress insertStakeAddressStmt

--------------------------------------------------------------------------------
insertStakeDeregistrationStmt :: HsqlStmt.Statement SS.StakeDeregistration Id.StakeDeregistrationId
insertStakeDeregistrationStmt =
  insert
    SS.stakeDeregistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.StakeDeregistrationId)

insertStakeDeregistration :: MonadIO m => SS.StakeDeregistration -> DbAction m Id.StakeDeregistrationId
insertStakeDeregistration stakeDeregistration =
  runDbSession (mkDbCallStack "insertStakeDeregistration") $
    HsqlSes.statement stakeDeregistration insertStakeDeregistrationStmt

--------------------------------------------------------------------------------
insertStakeRegistrationStmt :: HsqlStmt.Statement SS.StakeRegistration Id.StakeRegistrationId
insertStakeRegistrationStmt =
  insert
    SS.stakeRegistrationEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.StakeRegistrationId)

insertStakeRegistration :: MonadIO m => SS.StakeRegistration -> DbAction m Id.StakeRegistrationId
insertStakeRegistration stakeRegistration =
  runDbSession (mkDbCallStack "insertStakeRegistration") $
    HsqlSes.statement stakeRegistration insertStakeRegistrationStmt

-- | Queries

--------------------------------------------------------------------------------
queryStakeAddressStmt :: HsqlStmt.Statement ByteString (Maybe Id.StakeAddressId)
queryStakeAddressStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.rowMaybe (Id.idDecoder Id.StakeAddressId)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id"
          , " FROM stake_address"
          , " WHERE hash_raw = $1"
          ]

queryStakeAddress :: MonadIO m => ByteString -> DbAction m (Maybe Id.StakeAddressId)
queryStakeAddress addr = do
  runDbSession dbCallStack $ HsqlSes.statement addr queryStakeAddressStmt
  where
    dbCallStack = mkDbCallStack "queryStakeAddress"

-----------------------------------------------------------------------------------
queryStakeRefPtrStmt :: HsqlStmt.Statement Ptr (Maybe Id.StakeAddressId)
queryStakeRefPtrStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTable = tableName (Proxy @SCB.Block)
    txTable = tableName (Proxy @SCB.Tx)
    srTable = tableName (Proxy @SS.StakeRegistration)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT sr.addr_id FROM "
          , blockTable
          , " blk"
          , " INNER JOIN "
          , txTable
          , " tx ON blk.id = tx.block_id"
          , " INNER JOIN "
          , srTable
          , " sr ON sr.tx_id = tx.id"
          , " WHERE blk.slot_no = $1"
          , " AND tx.block_index = $2"
          , " AND sr.cert_index = $3"
          , " ORDER BY blk.slot_no DESC"
          , " LIMIT 1"
          ]

    encoder =
      mconcat
        [ (\(Ptr (SlotNo32 s) _ _) -> s) >$< HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int8))
        , (\(Ptr _ (TxIx t) _) -> t) >$< HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int8))
        , (\(Ptr _ _ (CertIx c)) -> c) >$< HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int8))
        ]

    decoder =
      HsqlD.rowMaybe
        ( HsqlD.column $
            HsqlD.nonNullable $
              Id.StakeAddressId <$> HsqlD.int8
        )

queryStakeRefPtr :: MonadIO m => Ptr -> DbAction m (Maybe Id.StakeAddressId)
queryStakeRefPtr ptr =
  runDbSession (mkDbCallStack "queryStakeRefPtr") $
    HsqlSes.statement ptr queryStakeRefPtrStmt

-----------------------------------------------------------------------------------
-- Statement for querying stake addresses with non-null script_hash
queryStakeAddressScriptStmt :: HsqlStmt.Statement () [SS.StakeAddress]
queryStakeAddressScriptStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SS.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT *"
          , " FROM " <> tableN
          , " WHERE script_hash IS NOT NULL"
          ]
    decoder = HsqlD.rowList SS.stakeAddressDecoder

queryStakeAddressScript :: MonadIO m => DbAction m [SS.StakeAddress]
queryStakeAddressScript =
  runDbSession (mkDbCallStack "queryStakeAddressScript") $
    HsqlSes.statement () queryStakeAddressScriptStmt

-----------------------------------------------------------------------------------
queryAddressInfoRewardsStmt :: HsqlStmt.Statement Id.StakeAddressId Ada
queryAddressInfoRewardsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    rewardTableN = tableName (Proxy @SS.Reward)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(amount), 0)"
          , " FROM " <> rewardTableN
          , " WHERE addr_id = $1"
          ]
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.singleRow adaSumDecoder

queryAddressInfoWithdrawalsStmt :: HsqlStmt.Statement Id.StakeAddressId Ada
queryAddressInfoWithdrawalsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    withdrawalTableN = tableName (Proxy @SCB.Withdrawal)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COALESCE(SUM(amount), 0)"
          , " FROM " <> withdrawalTableN
          , " WHERE addr_id = $1"
          ]
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.singleRow adaSumDecoder

---------------------------------------------------------------------------
queryAddressInfoViewStmt :: HsqlStmt.Statement Id.StakeAddressId (Maybe Text.Text)
queryAddressInfoViewStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    stakeAddrTableN = tableName (Proxy @SS.StakeAddress)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT view"
          , " FROM " <> stakeAddrTableN
          , " WHERE id = $1"
          ]
    encoder = Id.idEncoder Id.getStakeAddressId
    decoder = HsqlD.rowMaybe $ HsqlD.column (HsqlD.nonNullable HsqlD.text)

-- Pipeline function
queryAddressInfoData :: MonadIO m => Id.StakeAddressId -> DbAction m (Ada, Ada, Maybe Text.Text)
queryAddressInfoData addrId =
  runDbSession (mkDbCallStack "queryAddressInfoData") $
    HsqlSes.pipeline $ do
      rewards <- HsqlP.statement addrId queryAddressInfoRewardsStmt
      withdrawals <- HsqlP.statement addrId queryAddressInfoWithdrawalsStmt
      view <- HsqlP.statement addrId queryAddressInfoViewStmt
      pure (rewards, withdrawals, view)

---------------------------------------------------------------------------

-- | Query reward for specific stake address and epoch
queryRewardForEpochStmt :: HsqlStmt.Statement (Word64, Id.StakeAddressId) (Maybe DbLovelace)
queryRewardForEpochStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    encoder =
      mconcat
        [ fst >$< HsqlE.param (HsqlE.nonNullable $ fromIntegral >$< HsqlE.int8)
        , snd >$< Id.idEncoder Id.getStakeAddressId
        ]
    decoder = HsqlD.rowMaybe dbLovelaceDecoder
    stakeAddressTable = tableName (Proxy @SS.StakeAddress)
    rewardTable = tableName (Proxy @SS.Reward)
    epochTable = tableName (Proxy @SEP.Epoch)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT rwd.amount"
          , " FROM " <> stakeAddressTable <> " saddr"
          , " INNER JOIN " <> rewardTable <> " rwd ON saddr.id = rwd.addr_id"
          , " INNER JOIN " <> epochTable <> " ep ON ep.no = rwd.earned_epoch"
          , " WHERE ep.no = $1"
          , " AND saddr.id = $2"
          , " ORDER BY ep.no ASC"
          ]

queryRewardForEpoch :: MonadIO m => Word64 -> Id.StakeAddressId -> DbAction m (Maybe DbLovelace)
queryRewardForEpoch epochNo saId =
  runDbSession (mkDbCallStack "queryRewardForEpoch") $
    HsqlSes.statement (epochNo, saId) queryRewardForEpochStmt

---------------------------------------------------------------------------
-- StakeDeregistration
---------------------------------------------------------------------------

queryDeregistrationScriptStmt :: HsqlStmt.Statement () [SS.StakeDeregistration]
queryDeregistrationScriptStmt =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    tableN = tableName (Proxy @SS.StakeDeregistration)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT addr_id, cert_index, epoch_no, tx_id, redeemer_id"
          , " FROM " <> tableN
          , " WHERE redeemer_id IS NOT NULL"
          ]

    decoder = HsqlD.rowList SS.stakeDeregistrationDecoder

queryDeregistrationScript :: MonadIO m => DbAction m [SS.StakeDeregistration]
queryDeregistrationScript =
  runDbSession (mkDbCallStack "queryDeregistrationScript") $
    HsqlSes.statement () queryDeregistrationScriptStmt
