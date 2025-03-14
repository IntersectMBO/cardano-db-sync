module Cardano.Db.Statement.Pool where

import qualified Hasql.Transaction as HsqlT

import Cardano.Db.Types (DbAction)
import qualified  Cardano.Db.Schema.Core.Pool as SP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db (DbWord64)

--------------------------------------------------------------------------------
-- | DelistedPool
--------------------------------------------------------------------------------
insertDelistedPool :: MonadIO m => SP.DelistedPool -> DbAction m Id.DelistedPoolId
insertDelistedPool delistedPool =
  runDbT TransWrite $ mkDbTransaction "insertDelistedPool" $ do
    entity <- insert
      delistedPoolEncoder
      (WithResult $ HsqlD.singleRow SP.entityDelistedPoolDecoder)
      delistedPool
    pure (entityKey entity)

--------------------------------------------------------------------------------
-- | PoolHash
--------------------------------------------------------------------------------
insertPoolHash :: MonadIO m => SP.PoolHash -> DbAction m Id.PoolHashId
insertPoolHash poolHash =
  runDbT TransWrite $ mkDbTransaction "insertPoolHash" $ do
    entity <- insert
      poolHashEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolHashDecoder)
      poolHash
    pure (entityKey entity)

queryPoolHashIdExists :: MonadIO m => Id.PoolHashId -> DbAction m Bool
queryPoolHashIdExists poolHashId = runDbT TransReadOnly $ mkDbTransaction "queryPoolHashIdExists" $
  queryIdExists @SP.PoolHash
    (Id.idEncoder Id.getPoolHashId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
    poolHashId

-- queryVotingAnchorIdExists :: MonadIO m => Id.PoolHashId -> DbAction m Bool
-- queryVotingAnchorIdExists poolHashId = runDbT TransReadOnly $ mkDbTransaction "queryVotingAnchorIdExists" $
--   queryIdExists @SP.PoolHash
--     (Id.idEncoder Id.getPoolHashId)
--     (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
--     poolHashId
--------------------------------------------------------------------------------
-- | PoolMetadataRef
--------------------------------------------------------------------------------
insertPoolMetadataRef :: MonadIO m => SP.PoolMetadataRef -> DbAction m Id.PoolMetadataRefId
insertPoolMetadataRef poolMetadataRef =
  runDbT TransWrite $ mkDbTransaction "insertPoolMetadataRef" $ do
    entity <- insert
      poolMetadataRefEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolMetadataRefDecoder)
      poolMetadataRef
    pure (entityKey entity)

queryPoolMetadataRefIdExists :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
queryPoolMetadataRefIdExists poolMetadataRefId = runDbT TransReadOnly $ mkDbTransaction "queryPoolMetadataRefIdExists" $
  queryIdExists @SP.PoolMetadataRef
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
    poolMetadataRefId

insertPoolOwner :: MonadIO m => SP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner =
  runDbT TransWrite $ mkDbTransaction "insertPoolOwner" $ do
    entity <- insert
      poolOwnerEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolOwnerDecoder)
      poolOwner
    pure (entityKey entity)

insertPoolRelay :: MonadIO m => SP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay =
  runDbT TransWrite $ mkDbTransaction "insertPoolRelay" $ do
    entity <- insert
      poolRelayEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolRelayDecoder)
      poolRelay
    pure (entityKey entity)

insertPoolRetire :: MonadIO m => SP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire =
  runDbT TransWrite $ mkDbTransaction "insertPoolRetire" $ do
    entity <- insert
      poolRetireEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolRetireDecoder)
      poolRetire
    pure (entityKey entity)

bulkInsertPoolStat :: MonadIO m => [SP.PoolStat] -> DbAction m ()
bulkInsertPoolStat poolStats = runDbT TransWrite $ mkDbTransaction "bulkInsertPoolStat" $
  bulkInsert
    extractPoolStat
    NoResult
    poolStats
  where
    extractPoolStat :: [PoolStat] -> ([Id.PoolHashId], [Word32], [DbWord64], [DbWord64], [DbWord64], [DbWord64])
    extractPoolStat xs =
        ( map poolStatPoolHashId xs
        , map poolStatEpochNo xs
        , map poolStatNumberOfBlocks xs
        , map poolStatNumberOfDelegators xs
        , map poolStatStake xs
        , map poolStatVotingPower xs
        )

insertPoolUpdate :: MonadIO m => SP.PoolUpdate -> DbAction m Id.PoolUpdateId
insertPoolUpdate poolUpdate =
  runDbT TransWrite $ mkDbTransaction "insertPoolUpdate" $ do
    entity <- insert
      poolUpdateEncoder
      (WithResult $ HsqlD.singleRow SP.entityPoolUpdateDecoder)
      poolUpdate
    pure (entityKey entity)

insertReservedPoolTicker :: MonadIO m => SP.ReservedPoolTicker -> DbAction m (maybe Id.ReservedPoolTickerId)
insertReservedPoolTicker reservedPool =
  runDbT TransWrite $ mkDbTransaction "insertReservedPoolTicker" $ do
    entity <- insertCheckUnique
      reservedPoolTickerEncoder
      (WithResult $ HsqlD.singleRow SP.entityReservedPoolTickerDecoder)
      reservedPool
    pure (entityKey entity)


-- These tables manage stake pool-related data, including pool registration, updates, and retirements.

-- delisted_pool
-- pool_hash
-- pool_metadata_ref
-- pool_owner
-- pool_relay
-- pool_retire
-- pool_stat
-- pool_update
-- reserved_pool_ticker
