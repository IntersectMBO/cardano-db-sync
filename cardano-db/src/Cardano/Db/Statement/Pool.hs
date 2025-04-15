{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Pool where

import Cardano.Db.Schema.Core.GovernanceAndVoting ()
import qualified Cardano.Db.Schema.Core.Pool as SP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk)
import Cardano.Db.Statement.Function.Query (existsById)
import Cardano.Db.Statement.Types (Entity (..))
import Cardano.Db.Types (DbAction, DbWord64)
import Cardano.Prelude (MonadIO, Word64)
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlS

--------------------------------------------------------------------------------
-- DelistedPool
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
      HsqlSes.statement delistedPool insertDelistedPoolStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- PoolHash
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
      HsqlSes.statement poolHash insertPoolHashStmt
  pure $ entityKey entity

queryPoolHashIdExists :: MonadIO m => Id.PoolHashId -> DbAction m Bool
queryPoolHashIdExists poolHashId =
  runDbSession (mkCallInfo "queryPoolHashIdExists") $
    HsqlSes.statement poolHashId queryPoolHashIdExistsStmt

queryPoolHashIdExistsStmt :: HsqlS.Statement Id.PoolHashId Bool
queryPoolHashIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolHashId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

-- queryVotingAnchorIdExists :: MonadIO m => Id.VotingAnchorId -> DbAction m Bool
-- queryVotingAnchorIdExists votingAnchorId =
--   runDbSession (mkCallInfo "queryVotingAnchorIdExists") $
--     HsqlSes.statement votingAnchorId queryVotingAnchorIdExistsStmt

-- queryVotingAnchorIdExistsStmt :: HsqlS.Statement Id.VotingAnchorId Bool
-- queryVotingAnchorIdExistsStmt =
--   existsById
--     (Id.idEncoder Id.getVotingAnchorId)
--     (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

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
      HsqlSes.statement poolMetadataRef insertPoolMetadataRefStmt
  pure $ entityKey entity

queryPoolMetadataRefIdExistsStmt :: HsqlS.Statement Id.PoolMetadataRefId Bool
queryPoolMetadataRefIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryPoolMetadataRefIdExists :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
queryPoolMetadataRefIdExists poolMetadataRefId =
  runDbSession (mkCallInfo "queryPoolMetadataRefIdExists") $
    HsqlSes.statement poolMetadataRefId queryPoolMetadataRefIdExistsStmt

insertPoolOwnerStmt :: HsqlS.Statement SP.PoolOwner (Entity SP.PoolOwner)
insertPoolOwnerStmt =
  insert
    SP.poolOwnerEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolOwnerDecoder)

insertPoolOwner :: MonadIO m => SP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner = do
  entity <-
    runDbSession (mkCallInfo "insertPoolOwner") $
      HsqlSes.statement poolOwner insertPoolOwnerStmt
  pure $ entityKey entity

insertPoolRelayStmt :: HsqlS.Statement SP.PoolRelay (Entity SP.PoolRelay)
insertPoolRelayStmt =
  insert
    SP.poolRelayEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolRelayDecoder)

insertPoolRelay :: MonadIO m => SP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay = do
  entity <- runDbSession (mkCallInfo "insertPoolRelay") $ HsqlSes.statement poolRelay insertPoolRelayStmt
  pure $ entityKey entity

insertPoolRetireStmt :: HsqlS.Statement SP.PoolRetire (Entity SP.PoolRetire)
insertPoolRetireStmt =
  insert
    SP.poolRetireEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolRetireDecoder)

insertPoolRetire :: MonadIO m => SP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire = do
  entity <- runDbSession (mkCallInfo "insertPoolRetire") $ HsqlSes.statement poolRetire insertPoolRetireStmt
  pure $ entityKey entity

insertBulkPoolStatStmt :: HsqlS.Statement [SP.PoolStat] ()
insertBulkPoolStatStmt =
  insertBulk
    extractPoolStat
    SP.poolStatBulkEncoder
    NoResultBulk
  where
    extractPoolStat :: [SP.PoolStat] -> ([Id.PoolHashId], [Word64], [DbWord64], [DbWord64], [DbWord64], [Maybe DbWord64])
    extractPoolStat xs =
      ( map SP.poolStatPoolHashId xs
      , map SP.poolStatEpochNo xs
      , map SP.poolStatNumberOfBlocks xs
      , map SP.poolStatNumberOfDelegators xs
      , map SP.poolStatStake xs
      , map SP.poolStatVotingPower xs
      )

insertBulkPoolStat :: MonadIO m => [SP.PoolStat] -> DbAction m ()
insertBulkPoolStat poolStats = do
  runDbSession (mkCallInfo "insertBulkPoolStat") $
    HsqlSes.statement poolStats insertBulkPoolStatStmt

insertPoolUpdateStmt :: HsqlS.Statement SP.PoolUpdate (Entity SP.PoolUpdate)
insertPoolUpdateStmt =
  insert
    SP.poolUpdateEncoder
    (WithResult $ HsqlD.singleRow SP.entityPoolUpdateDecoder)

insertPoolUpdate :: MonadIO m => SP.PoolUpdate -> DbAction m Id.PoolUpdateId
insertPoolUpdate poolUpdate = do
  entity <- runDbSession (mkCallInfo "insertPoolUpdate") $ HsqlSes.statement poolUpdate insertPoolUpdateStmt
  pure $ entityKey entity

insertReservedPoolTickerStmt :: HsqlS.Statement SP.ReservedPoolTicker (Entity SP.ReservedPoolTicker)
insertReservedPoolTickerStmt =
  insert
    SP.reservedPoolTickerEncoder
    (WithResult $ HsqlD.singleRow SP.entityReservedPoolTickerDecoder)

insertReservedPoolTicker :: MonadIO m => SP.ReservedPoolTicker -> DbAction m Id.ReservedPoolTickerId
insertReservedPoolTicker reservedPool = do
  entity <- runDbSession (mkCallInfo "insertReservedPoolTicker") $ HsqlSes.statement reservedPool insertReservedPoolTickerStmt
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
