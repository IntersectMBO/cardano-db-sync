{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.StakeDeligation where

import Cardano.Prelude (ByteString, MonadError (..), MonadIO, Proxy (..))
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Data.Word (Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (DbError (..))
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.StakeDeligation as SS
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertCheckUnique)
import Cardano.Db.Statement.Function.Query (countAll)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..), validateColumn)
import Cardano.Db.Types (DbAction, DbCallInfo (..), DbLovelace, RewardSource)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential (Ptr (..))

--------------------------------------------------------------------------------
-- Deligation
--------------------------------------------------------------------------------
insertDelegationStmt :: HsqlStmt.Statement SS.Delegation (Entity SS.Delegation)
insertDelegationStmt =
  insert
    SS.delegationEncoder
    (WithResult $ HsqlD.singleRow SS.entityDelegationDecoder)

insertDelegation :: MonadIO m => SS.Delegation -> DbAction m Id.DelegationId
insertDelegation delegation = do
  entity <- runDbSession (mkCallInfo "insertDelegation") $ HsqlSes.statement delegation insertDelegationStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- EpochStake
--------------------------------------------------------------------------------

-- | INSERT --------------------------------------------------------------------
insertBulkEpochStakeStmt :: HsqlStmt.Statement [SS.EpochStake] ()
insertBulkEpochStakeStmt =
  insertBulk
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

insertBulkEpochStake :: MonadIO m => [SS.EpochStake] -> DbAction m ()
insertBulkEpochStake epochStakes =
  runDbSession (mkCallInfo "insertBulkEpochStake") $
    HsqlSes.statement epochStakes insertBulkEpochStakeStmt

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
  runDbSession (mkCallInfo "queryEpochStakeCount") $
    HsqlSes.statement epoch queryEpochStakeCountStmt

--------------------------------------------------------------------------------
queryMinMaxEpochStakeStmt ::
  forall a.
  (DbInfo a) =>
  Text.Text ->
  HsqlStmt.Statement () (Maybe Word64, Maybe Word64)
queryMinMaxEpochStakeStmt colName =
  HsqlStmt.Statement sql HsqlE.noParams decoder True
  where
    table = tableName (Proxy @a)
    validCol = validateColumn @a colName

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT "
          , "(SELECT MIN("
          , validCol
          , ") FROM "
          , table
          , "), "
          , "(SELECT MAX("
          , validCol
          , ") FROM "
          , table
          , ")"
          ]

    decoder =
      HsqlD.singleRow $
        ((,) . fmap fromIntegral <$> HsqlD.column (HsqlD.nullable HsqlD.int8))
          <*> (fmap fromIntegral <$> HsqlD.column (HsqlD.nullable HsqlD.int8))

queryMinMaxEpochStake :: MonadIO m => DbAction m (Maybe Word64, Maybe Word64)
queryMinMaxEpochStake =
  runDbSession (mkCallInfo "queryMinMaxEpochStake") $
    HsqlSes.statement () $
      queryMinMaxEpochStakeStmt @SS.EpochStake "epoch_no"

--------------------------------------------------------------------------------
-- EpochProgress
--------------------------------------------------------------------------------
insertBulkEpochStakeProgressStmt :: HsqlStmt.Statement [SS.EpochStakeProgress] ()
insertBulkEpochStakeProgressStmt =
  insertBulk
    extractEpochStakeProgress
    SS.epochStakeProgressBulkEncoder
    NoResultBulk
  where
    extractEpochStakeProgress :: [SS.EpochStakeProgress] -> ([Word64], [Bool])
    extractEpochStakeProgress xs =
      ( map SS.epochStakeProgressEpochNo xs
      , map SS.epochStakeProgressCompleted xs
      )

insertBulkEpochStakeProgress :: MonadIO m => [SS.EpochStakeProgress] -> DbAction m ()
insertBulkEpochStakeProgress epochStakeProgresses =
  runDbSession (mkCallInfo "insertBulkEpochStakeProgress") $
    HsqlSes.statement epochStakeProgresses insertBulkEpochStakeProgressStmt

--------------------------------------------------------------------------------
-- Reward
--------------------------------------------------------------------------------

-- | INSERT ---------------------------------------------------------------------
insertBulkRewardsStmt :: HsqlStmt.Statement [SS.Reward] ()
insertBulkRewardsStmt =
  insertBulk
    extractReward
    SS.rewardBulkEncoder
    NoResultBulk
  where
    extractReward :: [SS.Reward] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64], [Id.PoolHashId])
    extractReward xs =
      ( map SS.rewardAddrId xs
      , map SS.rewardType xs
      , map SS.rewardAmount xs
      , map SS.rewardEarnedEpoch xs
      , map SS.rewardSpendableEpoch xs
      , map SS.rewardPoolId xs
      )

insertBulkRewards :: MonadIO m => [SS.Reward] -> DbAction m ()
insertBulkRewards rewards =
  runDbSession (mkCallInfo "insertBulkRewards") $
    HsqlSes.statement rewards insertBulkRewardsStmt

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
  runDbSession (mkCallInfo "queryNormalEpochRewardCount") $
    HsqlSes.statement epochNum queryNormalEpochRewardCountStmt

--------------------------------------------------------------------------------
queryRewardCount :: MonadIO m => DbAction m Word64
queryRewardCount =
  runDbSession (mkCallInfo "queryRewardCount") $
    HsqlSes.statement () (countAll @SS.Reward)

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
    extractRewardRest :: [SS.RewardRest] -> ([Id.StakeAddressId], [RewardSource], [DbLovelace], [Word64], [Word64])
    extractRewardRest xs =
      ( map SS.rewardRestAddrId xs
      , map SS.rewardRestType xs
      , map SS.rewardRestAmount xs
      , map SS.rewardRestEarnedEpoch xs
      , map SS.rewardRestSpendableEpoch xs
      )

insertBulkRewardRests :: MonadIO m => [SS.RewardRest] -> DbAction m ()
insertBulkRewardRests rewardRests =
  runDbSession (mkCallInfo "insertBulkRewardRests") $
    HsqlSes.statement rewardRests insertBulkRewardRestsStmt

--------------------------------------------------------------------------------
queryRewardRestCount :: MonadIO m => DbAction m Word64
queryRewardRestCount =
  runDbSession (mkCallInfo "queryRewardRestCount") $
    HsqlSes.statement () (countAll @SS.RewardRest)

--------------------------------------------------------------------------------
-- StakeAddress
--------------------------------------------------------------------------------
insertStakeAddressStmt :: HsqlStmt.Statement SS.StakeAddress (Entity SS.StakeAddress)
insertStakeAddressStmt =
  insertCheckUnique
    SS.stakeAddressEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeAddressDecoder)

insertStakeAddress :: MonadIO m => SS.StakeAddress -> DbAction m Id.StakeAddressId
insertStakeAddress stakeAddress =
  runDbSession (mkCallInfo "insertStakeAddress") $ do
    entity <-
      HsqlSes.statement stakeAddress insertStakeAddressStmt
    pure $ entityKey entity

insertStakeDeregistrationStmt :: HsqlStmt.Statement SS.StakeDeregistration (Entity SS.StakeDeregistration)
insertStakeDeregistrationStmt =
  insertCheckUnique
    SS.stakeDeregistrationEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeDeregistrationDecoder)

insertStakeDeregistration :: MonadIO m => SS.StakeDeregistration -> DbAction m Id.StakeDeregistrationId
insertStakeDeregistration stakeDeregistration =
  runDbSession (mkCallInfo "insertStakeDeregistration") $ do
    entity <-
      HsqlSes.statement stakeDeregistration insertStakeDeregistrationStmt
    pure $ entityKey entity

insertStakeRegistrationStmt :: HsqlStmt.Statement SS.StakeRegistration (Entity SS.StakeRegistration)
insertStakeRegistrationStmt =
  insert
    SS.stakeRegistrationEncoder
    (WithResult $ HsqlD.singleRow SS.entityStakeRegistrationDecoder)

insertStakeRegistration :: MonadIO m => SS.StakeRegistration -> DbAction m Id.StakeRegistrationId
insertStakeRegistration stakeRegistration = do
  entity <-
    runDbSession (mkCallInfo "insertStakeRegistration") $
      HsqlSes.statement stakeRegistration insertStakeRegistrationStmt
  pure $ entityKey entity

-- | Queries
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

queryStakeAddress :: MonadIO m => ByteString -> (ByteString -> Text.Text) -> DbAction m Id.StakeAddressId
queryStakeAddress addr toText = do
  result <- runDbSession callInfo $ HsqlSes.statement addr queryStakeAddressStmt
  case result of
    Just res -> pure res
    Nothing -> throwError $ DbError (dciCallSite callInfo) errorMsg Nothing
  where
    callInfo = mkCallInfo "queryStakeAddress"
    errorMsg = "StakeAddress " <> toText addr <> " not found"

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
        [ (\(Ptr (SlotNo s) _ _) -> s) >$< HsqlE.param (HsqlE.nonNullable (fromIntegral >$< HsqlE.int8))
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
  runDbSession (mkCallInfo "queryStakeRefPtr") $
    HsqlSes.statement ptr queryStakeRefPtrStmt

-- These tables handle stake addresses, delegation, and reward

-- delegation
-- epoch_stake
-- epoch_stake_progress
-- reward
-- reward_rest
-- stake_address
-- stake_deregistration
-- stake_registration
