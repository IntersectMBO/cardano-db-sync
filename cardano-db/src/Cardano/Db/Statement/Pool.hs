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
insertDelistedPool delistedPool = runDbT TransWrite $ mkDbTransaction "insertDelistedPool" $
  insert
    delistedPoolEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.DelistedPoolId))
    delistedPool

--------------------------------------------------------------------------------
-- | PoolHash
--------------------------------------------------------------------------------
insertPoolHash :: MonadIO m => SP.PoolHash -> DbAction m Id.PoolHashId
insertPoolHash poolHash = runDbT TransWrite $ mkDbTransaction "insertPoolHash" $
  insert
    poolHashEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolHashId))
    poolHash

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
insertPoolMetadataRef poolMetadataRef = runDbT TransWrite $ mkDbTransaction "insertPoolMetadataRef" $
  insert
    poolMetadataRefEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolMetadataRefId))
    poolMetadataRef

queryPoolMetadataRefIdExists :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
queryPoolMetadataRefIdExists poolMetadataRefId = runDbT TransReadOnly $ mkDbTransaction "queryPoolMetadataRefIdExists" $
  queryIdExists @SP.PoolMetadataRef
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))
    poolMetadataRefId

insertPoolOwner :: MonadIO m => SP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner = runDbT TransWrite $ mkDbTransaction "insertPoolOwner" $
  insert
    poolOwnerEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolOwnerId))
    poolOwner

insertPoolRelay :: MonadIO m => SP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay = runDbT TransWrite $ mkDbTransaction "insertPoolRelay" $
  insert
    poolRelayEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolRelayId))
    poolRelay

insertPoolRetire :: MonadIO m => SP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire = runDbT TransWrite $ mkDbTransaction "insertPoolRetire" $
  insert
    poolRetireEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolRetireId))
    poolRetire

insertManyPoolStat :: MonadIO m => [SP.PoolStat] -> DbAction m ()
insertManyPoolStat poolStats = runDbT TransWrite $ mkDbTransaction "insertManyPoolStat" $
  bulkInsertNoReturn
    extractPoolStat
    encodePoolStatMany
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
insertPoolUpdate poolUpdate = runDbT TransWrite $ mkDbTransaction "insertPoolUpdate" $
  insert
    poolUpdateEncoder
    (WithResult (HsqlD.singleRow $ Id.idDecoder Id.PoolUpdateId))
    poolUpdate

insertReservedPoolTicker :: MonadIO m => SP.ReservedPoolTicker -> DbAction m (maybe Id.ReservedPoolTickerId)
insertReservedPoolTicker reservedPool = runDbT TransWrite $ mkDbTransaction "insertReservedPoolTicker" $
  insertCheckUnique
    reservedPoolTickerEncoder
    (WithResult (HsqlD.singleRow $ Id.maybeIdDecoder Id.ReservedPoolTickerId))
    reservedPool


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
