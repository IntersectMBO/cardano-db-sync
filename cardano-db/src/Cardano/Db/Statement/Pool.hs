module Cardano.Db.Statement.Pool where

import Cardano.Db (DbWord64)
import qualified Cardano.Db.Schema.Core.Pool as SP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Types (DbAction)

--------------------------------------------------------------------------------

-- | DelistedPool

--------------------------------------------------------------------------------
insertDelistedPoolStmt :: HsqlS.Statement SP.DelistedPool (Entity SP.DelistedPool)
insertDelistedPoolStmt =
  insert
    SP.delistedPoolEncoder
    (WithResult $ HsqlD.singleRow SP.entityDelistedPoolDecoder)

insertDelistedPool :: MonadIO m => SP.DelistedPool -> DbAction m Id.DelistedPoolId
insertDelistedPool delistedPool = do
  entity <-
    runDbSession (mkCallInfo "insertDelistedPool") $
      HsqlS.statement delistedPool insertDelistedPoolStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------

-- | PoolHash

--------------------------------------------------------------------------------
insertPoolHashStmt :: HsqlS.Statement SP.PoolHash (Entity SP.PoolHash)
insertPoolHashStmt =
  insert
    SP.poolHashEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolHashDecoder)

insertPoolHash :: MonadIO m => SP.PoolHash -> DbAction m Id.PoolHashId
insertPoolHash poolHash = do
  entity <-
    runDbSession (mkCallInfo "insertPoolHash") $
      HsqlS.statement poolHash insertPoolHashStmt
  pure $ entityKey entity

queryPoolHashIdExists :: MonadIO m => Id.PoolHashId -> DbAction m Bool
queryPoolHashIdExists poolHashId =
  runDbSession (mkCallInfo "queryPoolHashIdExists") $
    HsqlS.statement poolHashId queryPoolHashIdExistsStmt

queryPoolHashIdExistsStmt :: HsqlS.Statement Id.PoolHashId Bool
queryPoolHashIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolHashId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryVotingAnchorIdStmt :: HsqlS.Statement Id.VotingAnchorId Bool
queryVotingAnchorIdStmt =
  existsById
    (Id.idEncoder Id.getVotingAnchorId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
queryVotingAnchorIdExists votingAnchorId =
  runDbSession (mkCallInfo "queryVotingAnchorIdExists") $
    HsqlS.statement votingAnchorId queryVotingAnchorIdStmt

--------------------------------------------------------------------------------

-- | PoolMetadataRef

--------------------------------------------------------------------------------
insertPoolMetadataRefStmt :: HsqlS.Statement SP.PoolMetadataRef (Entity SP.PoolMetadataRef)
insertPoolMetadataRefStmt =
  insert
    SP.poolMetadataRefEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolMetadataRefDecoder)

insertPoolMetadataRef :: MonadIO m => SP.PoolMetadataRef -> DbAction m Id.PoolMetadataRefId
insertPoolMetadataRef poolMetadataRef = do
  entity <-
    runDbSession (mkCallInfo "insertPoolMetadataRef") $
      HsqlS.statement poolMetadataRef insertPoolMetadataRefStmt
  pure $ entityKey entity

queryPoolMetadataRefIdExistsStmt :: HsqlS.Statement Id.PoolMetadataRefId Bool
queryPoolMetadataRefIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryPoolMetadataRefIdExists :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
queryPoolMetadataRefIdExists poolMetadataRefId =
  runDbSession (mkCallInfo "queryPoolMetadataRefIdExists") $
    HsqlS.statement poolMetadataRefId queryPoolMetadataRefIdExistsStmt

insertPoolOwnerStmt :: HsqlS.Statement SP.PoolOwner (Entity SP.PoolOwner)
insertPoolOwnerStmt =
  insert
    SP.poolOwnerEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolOwnerDecoder)

insertPoolOwner :: MonadIO m => SP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner = do
  entity <-
    runDbSession (mkCallInfo "insertPoolOwner") $
      HsqlS.statement poolOwner insertPoolOwnerStmt
  pure $ entityKey entity

insertPoolRelayStmt :: HsqlS.Statement SP.PoolRelay (Entity SP.PoolRelay)
insertPoolRelayStmt =
  insert
    SP.poolRelayEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolRelayDecoder)

insertPoolRelay :: MonadIO m => SP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay = do
  entity <- runDbSession (mkCallInfo "insertPoolRelay") $ HsqlS.statement poolRelay insertPoolRelayStmt
  pure $ entityKey entity

insertPoolRetireStmt :: HsqlS.Statement SP.PoolRetire (Entity SP.PoolRetire)
insertPoolRetireStmt =
  insert
    SP.poolRetireEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolRetireDecoder)

insertPoolRetire :: MonadIO m => SP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire = do
  entity <- runDbSession (mkCallInfo "insertPoolRetire") $ HsqlS.statement poolRetire insertPoolRetireStmt
  pure $ entityKey entity

bulkInsertPoolStatStmt :: HsqlS.Statement ([Id.PoolHashId], [Word32], [DbWord64], [DbWord64], [DbWord64], [DbWord64]) ()
bulkInsertPoolStatStmt =
  bulkInsert
    SP.poolStatBulkEncoder
    NoResultBulk

bulkInsertPoolStat :: MonadIO m => [SP.PoolStat] -> DbAction m ()
bulkInsertPoolStat poolStats = do
  runDbSession (mkCallInfo "bulkInsertPoolStat") $
    HsqlS.statement (extractPoolStat poolStat) bulkInsertPoolStatStmt
  where
    extractPoolStat :: [SP.PoolStat] -> ([Id.PoolHashId], [Word32], [DbWord64], [DbWord64], [DbWord64], [DbWord64])
    extractPoolStat xs =
      ( map SP.poolStatPoolHashId xs
      , map SP.poolStatEpochNo xs
      , map SP.poolStatNumberOfBlocks xs
      , map SP.poolStatNumberOfDelegators xs
      , map SP.poolStatStake xs
      , map SP.poolStatVotingPower xs
      )

insertPoolUpdateStmt :: HsqlS.Statement SP.PoolUpdate (Entity SP.PoolUpdate)
insertPoolUpdateStmt =
  insert
    SP.poolUpdateEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolUpdateDecoder)

insertPoolUpdate :: MonadIO m => SP.PoolUpdate -> DbAction m Id.PoolUpdateId
insertPoolUpdate poolUpdate = do
  entity <- runDbSession (mkCallInfo "insertPoolUpdate") $ HsqlS.statement poolUpdate insertPoolUpdateStmt
  pure $ entityKey entity

insertReservedPoolTickerStmt :: HsqlS.Statement SP.ReservedPoolTicker (Entity SP.ReservedPoolTicker)
insertReservedPoolTickerStmt =
  insert
    SP.reservedPoolTickerEncoder
    (WithResult $ HsqlD.singleRow SP.entityReservedPoolTickerDecoder)

insertReservedPoolTicker :: MonadIO m => SP.ReservedPoolTicker -> DbAction m Id.ReservedPoolTickerId
insertReservedPoolTicker reservedPool = do
  entity <- runDbSession (mkCallInfo "insertReservedPoolTicker") $ HsqlS.statement reservedPool insertReservedPoolTickerStmt
  pure $ entityKey entity

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
