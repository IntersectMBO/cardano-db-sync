module Cardano.Db.Statement.Pool where

import Cardano.Db.Types (DbAction)
import Cardano.Db.Schema.Ids (MaTxMintId)
import qualified Hasql.Transaction as HsqlT
import Cardano.Db (DbWord64)


insertManyPoolStat :: MonadIO m => [PoolStat] -> DbAction m ()
insertManyPoolStat poolStats = runDbT Write $ mkDbTransaction "insertManyPoolStat" $
  bulkInsertNoReturn
    extractPoolStat
    encodePoolStatMany
    poolStats
  where
    extractPoolStat :: [PoolStat] -> ([PoolHashId], [Word32], [DbWord64], [DbWord64], [DbWord64], [DbWord64])
    extractPoolStat xs =
        ( map poolStatPoolHashId xs
        , map poolStatEpochNo xs
        , map poolStatNumberOfBlocks xs
        , map poolStatNumberOfDelegators xs
        , map poolStatStake xs
        , map poolStatVotingPower xs
        )

-- These tables manage stake pool-related data, including pool registration, updates, and retirements.

-- pool_hash
-- pool_update
-- pool_retire
-- pool_owner
-- pool_metadata_ref
-- pool_relay
-- pool_stat
-- delisted_pool
-- reserved_pool_ticker
