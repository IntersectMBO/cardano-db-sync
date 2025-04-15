{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Pool where

import Cardano.Prelude (ByteString, MonadIO, Proxy (..), Word64, Int64)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkCallInfo, runDbSession)
import Cardano.Db.Statement.Function.Insert (insert, insertBulk, insertIfUnique)
import Cardano.Db.Statement.Function.Query (existsById, existsWhereByColumn)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (CertNo (..), DbAction, DbWord64, PoolCert (..), PoolCertAction (..))
import Cardano.Db.Statement.Function.Delete (parameterisedDeleteWhere)

--------------------------------------------------------------------------------
-- DelistedPool
--------------------------------------------------------------------------------
insertDelistedPoolStmt :: HsqlStmt.Statement SCP.DelistedPool (Entity SCP.DelistedPool)
insertDelistedPoolStmt =
  insert
    SCP.delistedPoolEncoder
    (WithResult $ HsqlD.singleRow SCP.entityDelistedPoolDecoder)

insertDelistedPool :: MonadIO m => SCP.DelistedPool -> DbAction m Id.DelistedPoolId
insertDelistedPool delistedPool = do
  entity <-
    runDbSession (mkCallInfo "insertDelistedPool") $
      HsqlSes.statement delistedPool insertDelistedPoolStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
queryDelistedPoolsStmt :: HsqlStmt.Statement () [ByteString]
queryDelistedPoolsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    delistedPoolTable = tableName (Proxy @SCP.DelistedPool)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT hash_raw FROM "
          , delistedPoolTable
          ]

    encoder = mempty
    decoder = HsqlD.rowList (HsqlD.column $ HsqlD.nonNullable HsqlD.bytea)

queryDelistedPools :: MonadIO m => DbAction m [ByteString]
queryDelistedPools =
  runDbSession (mkCallInfo "queryDelistedPools") $
    HsqlSes.statement () queryDelistedPoolsStmt

--------------------------------------------------------------------------------
existsDelistedPoolStmt :: HsqlStmt.Statement ByteString Bool
existsDelistedPoolStmt =
  existsWhereByColumn
    @SCP.DelistedPool  -- Specify the type explicitly
    "hash_raw"     -- Column to match on
    (HsqlE.param (HsqlE.nonNullable HsqlE.bytea))  -- ByteString encoder
    (WithResult $ HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool))

-- Updated function that takes a ByteString
existsDelistedPool :: MonadIO m => ByteString -> DbAction m Bool
existsDelistedPool ph =
  runDbSession (mkCallInfo "existsDelistedPool") $
    HsqlSes.statement ph existsDelistedPoolStmt

--------------------------------------------------------------------------------
deleteDelistedPoolStmt :: HsqlStmt.Statement ByteString Int64
deleteDelistedPoolStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "WITH deleted AS ("
      , "  DELETE FROM delisted_pool"
      , "  WHERE hash_raw = $1"
      , "  RETURNING *"
      , ")"
      , "SELECT COUNT(*)::bigint FROM deleted"
      ]

    encoder = id >$< HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

deleteDelistedPool :: MonadIO m => ByteString -> DbAction m Bool
deleteDelistedPool poolHash =
  runDbSession (mkCallInfo "deleteDelistedPool") $ do
    count <- HsqlSes.statement poolHash deleteDelistedPoolStmt
    pure $ count > 0


--------------------------------------------------------------------------------
-- PoolHash
--------------------------------------------------------------------------------
insertPoolHashStmt :: HsqlStmt.Statement SCP.PoolHash (Entity SCP.PoolHash)
insertPoolHashStmt =
  insert
    SCP.poolHashEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolHashDecoder)

insertPoolHash :: MonadIO m => SCP.PoolHash -> DbAction m Id.PoolHashId
insertPoolHash poolHash = do
  entity <-
    runDbSession (mkCallInfo "insertPoolHash") $
      HsqlSes.statement poolHash insertPoolHashStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
queryPoolHashIdStmt :: HsqlStmt.Statement ByteString (Maybe Id.PoolHashId)
queryPoolHashIdStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    table = tableName (Proxy @SCP.PoolHash)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT id FROM " <> table
          , " WHERE hash_raw = $1"
          , " LIMIT 1"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.bytea)
    decoder =
      HsqlD.rowMaybe
        ( HsqlD.column $
            HsqlD.nonNullable $
              Id.PoolHashId <$> HsqlD.int8
        )

queryPoolHashId :: MonadIO m => ByteString -> DbAction m (Maybe Id.PoolHashId)
queryPoolHashId hash =
  runDbSession (mkCallInfo "queryPoolHashId") $
    HsqlSes.statement hash queryPoolHashIdStmt

-----------------------------------------------------------------------------------
queryPoolHashIdExistsStmt :: HsqlStmt.Statement Id.PoolHashId Bool
queryPoolHashIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolHashId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryPoolHashIdExists :: MonadIO m => Id.PoolHashId -> DbAction m Bool
queryPoolHashIdExists poolHashId =
  runDbSession (mkCallInfo "queryPoolHashIdExists") $
    HsqlSes.statement poolHashId queryPoolHashIdExistsStmt

--------------------------------------------------------------------------------
-- PoolMetadataRef
--------------------------------------------------------------------------------
insertPoolMetadataRefStmt :: HsqlStmt.Statement SCP.PoolMetadataRef (Entity SCP.PoolMetadataRef)
insertPoolMetadataRefStmt =
  insert
    SCP.poolMetadataRefEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolMetadataRefDecoder)

insertPoolMetadataRef :: MonadIO m => SCP.PoolMetadataRef -> DbAction m Id.PoolMetadataRefId
insertPoolMetadataRef poolMetadataRef = do
  entity <-
    runDbSession (mkCallInfo "insertPoolMetadataRef") $
      HsqlSes.statement poolMetadataRef insertPoolMetadataRefStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
queryPoolMetadataRefIdExistsStmt :: HsqlStmt.Statement Id.PoolMetadataRefId Bool
queryPoolMetadataRefIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

queryPoolMetadataRefIdExists :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
queryPoolMetadataRefIdExists poolMetadataRefId =
  runDbSession (mkCallInfo "queryPoolMetadataRefIdExists") $
    HsqlSes.statement poolMetadataRefId queryPoolMetadataRefIdExistsStmt

--------------------------------------------------------------------------------
existsPoolMetadataRefIdStmt :: HsqlStmt.Statement Id.PoolMetadataRefId Bool
existsPoolMetadataRefIdStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

existsPoolMetadataRefId :: MonadIO m => Id.PoolMetadataRefId -> DbAction m Bool
existsPoolMetadataRefId pmrid =
  runDbSession (mkCallInfo "existsPoolMetadataRefId") $
    HsqlSes.statement pmrid existsPoolMetadataRefIdStmt

--------------------------------------------------------------------------------
deletePoolMetadataRefById :: MonadIO m => Id.PoolMetadataRefId -> DbAction m ()
deletePoolMetadataRefById pmrId =
  runDbSession (mkCallInfo "deletePoolMetadataRefById") $
    HsqlSes.statement pmrId (parameterisedDeleteWhere @SCP.PoolMetadataRef "id" ">=" $ Id.idEncoder Id.getPoolMetadataRefId)

--------------------------------------------------------------------------------
-- PoolRelay
--------------------------------------------------------------------------------

insertPoolRelayStmt :: HsqlStmt.Statement SCP.PoolRelay (Entity SCP.PoolRelay)
insertPoolRelayStmt =
  insert
    SCP.poolRelayEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolRelayDecoder)

insertPoolRelay :: MonadIO m => SCP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay = do
  entity <- runDbSession (mkCallInfo "insertPoolRelay") $ HsqlSes.statement poolRelay insertPoolRelayStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- PoolStat
--------------------------------------------------------------------------------
insertBulkPoolStatStmt :: HsqlStmt.Statement [SCP.PoolStat] ()
insertBulkPoolStatStmt =
  insertBulk
    extractPoolStat
    SCP.poolStatBulkEncoder
    NoResultBulk
  where
    extractPoolStat :: [SCP.PoolStat] -> ([Id.PoolHashId], [Word64], [DbWord64], [DbWord64], [DbWord64], [Maybe DbWord64])
    extractPoolStat xs =
      ( map SCP.poolStatPoolHashId xs
      , map SCP.poolStatEpochNo xs
      , map SCP.poolStatNumberOfBlocks xs
      , map SCP.poolStatNumberOfDelegators xs
      , map SCP.poolStatStake xs
      , map SCP.poolStatVotingPower xs
      )

insertBulkPoolStat :: MonadIO m => [SCP.PoolStat] -> DbAction m ()
insertBulkPoolStat poolStats = do
  runDbSession (mkCallInfo "insertBulkPoolStat") $
    HsqlSes.statement poolStats insertBulkPoolStatStmt

--------------------------------------------------------------------------------
-- PoolUpdate
--------------------------------------------------------------------------------

insertPoolUpdateStmt :: HsqlStmt.Statement SCP.PoolUpdate (Entity SCP.PoolUpdate)
insertPoolUpdateStmt =
  insert
    SCP.poolUpdateEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolUpdateDecoder)

insertPoolUpdate :: MonadIO m => SCP.PoolUpdate -> DbAction m Id.PoolUpdateId
insertPoolUpdate poolUpdate = do
  entity <- runDbSession (mkCallInfo "insertPoolUpdate") $ HsqlSes.statement poolUpdate insertPoolUpdateStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- PoolOwner
--------------------------------------------------------------------------------

insertPoolOwnerStmt :: HsqlStmt.Statement SCP.PoolOwner (Entity SCP.PoolOwner)
insertPoolOwnerStmt =
  insert
    SCP.poolOwnerEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolOwnerDecoder)

insertPoolOwner :: MonadIO m => SCP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner = do
  entity <-
    runDbSession (mkCallInfo "insertPoolOwner") $
      HsqlSes.statement poolOwner insertPoolOwnerStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
-- PoolRetire
--------------------------------------------------------------------------------

insertPoolRetireStmt :: HsqlStmt.Statement SCP.PoolRetire (Entity SCP.PoolRetire)
insertPoolRetireStmt =
  insert
    SCP.poolRetireEncoder
    (WithResult $ HsqlD.singleRow SCP.entityPoolRetireDecoder)

insertPoolRetire :: MonadIO m => SCP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire = do
  entity <- runDbSession (mkCallInfo "insertPoolRetire") $ HsqlSes.statement poolRetire insertPoolRetireStmt
  pure $ entityKey entity

--------------------------------------------------------------------------------
queryRetiredPoolsStmt :: HsqlStmt.Statement (Maybe ByteString) [PoolCert]
queryRetiredPoolsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolRetireN = tableName (Proxy @SCP.PoolRetire)
    poolHashN = tableName (Proxy @SCP.PoolHash)
    txN = tableName (Proxy @SCB.Tx)
    blockN = tableName (Proxy @SCB.Block)

    sql = TextEnc.encodeUtf8 $ Text.concat
      [ "SELECT ph.hash_raw, pr.retiring_epoch, blk.block_no, tx.block_index, pr.cert_index"
      , " FROM " <> poolRetireN <> " pr"
      , " INNER JOIN " <> poolHashN <> " ph ON pr.hash_id = ph.id"
      , " INNER JOIN " <> txN <> " tx ON pr.announced_tx_id = tx.id"
      , " INNER JOIN " <> blockN <> " blk ON tx.block_id = blk.id"
      , " WHERE ($1::bytea IS NULL OR ph.hash_raw = $1)"
      ]

    encoder = HsqlE.param (HsqlE.nullable HsqlE.bytea)

    decoder = HsqlD.rowList $ do
      hsh <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      retEpoch <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      blkNo <- HsqlD.column (HsqlD.nullable $ fromIntegral <$> HsqlD.int8)
      txIndex <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      retIndex <- HsqlD.column (HsqlD.nonNullable $ fromIntegral <$> HsqlD.int8)
      pure $ PoolCert
        { pcHash = hsh
        , pcCertAction = Retirement retEpoch
        , pcCertNo = CertNo blkNo txIndex retIndex
        }

queryRetiredPools :: MonadIO m => Maybe ByteString -> DbAction m [PoolCert]
queryRetiredPools mPoolHash =
  runDbSession (mkCallInfo "queryRetiredPools") $
    HsqlSes.statement mPoolHash queryRetiredPoolsStmt

--------------------------------------------------------------------------------
-- PoolUpdate
--------------------------------------------------------------------------------

-- Check if there are other PoolUpdates in the same blocks for the same pool
queryPoolUpdateByBlockStmt :: HsqlStmt.Statement (Id.BlockId, Id.PoolHashId) Bool
queryPoolUpdateByBlockStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    blockTable = tableName (Proxy @SCB.Block)
    txTable = tableName (Proxy @SCB.Tx)
    poolUpdateTable = tableName (Proxy @SCP.PoolUpdate)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS (SELECT 1 FROM "
          , blockTable
          , " blk"
          , " INNER JOIN "
          , txTable
          , " tx ON blk.id = tx.block_id"
          , " INNER JOIN "
          , poolUpdateTable
          , " poolUpdate ON tx.id = poolUpdate.registered_tx_id"
          , " WHERE poolUpdate.hash_id = $1"
          , " AND blk.id = $2"
          , " LIMIT 1)"
          ]

    encoder =
      mconcat
        [ snd >$< HsqlE.param (HsqlE.nonNullable (Id.getPoolHashId >$< HsqlE.int8))
        , fst >$< HsqlE.param (HsqlE.nonNullable (Id.getBlockId >$< HsqlE.int8))
        ]
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.bool)

queryPoolUpdateByBlock :: MonadIO m => Id.BlockId -> Id.PoolHashId -> DbAction m Bool
queryPoolUpdateByBlock blkId poolHashId =
  runDbSession (mkCallInfo "queryPoolUpdateByBlock") $
    HsqlSes.statement (blkId, poolHashId) queryPoolUpdateByBlockStmt

--------------------------------------------------------------------------------
queryPoolRegisterStmt :: HsqlStmt.Statement (Maybe ByteString) [PoolCert]
queryPoolRegisterStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolUpdateTable = tableName (Proxy @SCP.PoolUpdate)
    poolHashTable = tableName (Proxy @SCP.PoolHash)
    poolMetadataRefTable = tableName (Proxy @SCP.PoolMetadataRef)
    txTable = tableName (Proxy @SCB.Tx)
    blockTable = tableName (Proxy @SCB.Block)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT ph.hash_raw, pmr.hash, blk.block_no, tx.block_index, pu.cert_index"
          , " FROM "
          , poolUpdateTable
          , " pu"
          , " INNER JOIN "
          , poolHashTable
          , " ph ON pu.hash_id = ph.id"
          , " INNER JOIN "
          , poolMetadataRefTable
          , " pmr ON pu.meta_id = pmr.id"
          , " INNER JOIN "
          , txTable
          , " tx ON pu.registered_tx_id = tx.id"
          , " INNER JOIN "
          , blockTable
          , " blk ON tx.block_id = blk.id"
          , " WHERE ($1 IS NULL OR ph.hash_raw = $1)"
          ]

    encoder =
      id >$< HsqlE.param (HsqlE.nullable HsqlE.bytea)

    decoder = HsqlD.rowList $ do
      poolHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      metaHash <- HsqlD.column (HsqlD.nonNullable HsqlD.bytea)
      blkNo <- HsqlD.column (HsqlD.nullable (fromIntegral <$> HsqlD.int8))
      txIndex <- HsqlD.column (HsqlD.nonNullable (fromIntegral <$> HsqlD.int8))
      certIndex <- HsqlD.column (HsqlD.nonNullable (fromIntegral <$> HsqlD.int8))
      pure $
        PoolCert
          { pcHash = poolHash
          , pcCertAction = Register metaHash
          , pcCertNo = CertNo blkNo txIndex certIndex
          }

queryPoolRegister :: MonadIO m => Maybe ByteString -> DbAction m [PoolCert]
queryPoolRegister mPoolHash =
  runDbSession (mkCallInfo "queryPoolRegister") $
    HsqlSes.statement mPoolHash queryPoolRegisterStmt

--------------------------------------------------------------------------------
-- ReservedPoolTicker
--------------------------------------------------------------------------------

insertReservedPoolTickerStmt :: HsqlStmt.Statement SCP.ReservedPoolTicker (Maybe (Entity SCP.ReservedPoolTicker))
insertReservedPoolTickerStmt =
  insertIfUnique
    SCP.reservedPoolTickerEncoder
    SCP.entityReservedPoolTickerDecoder

insertReservedPoolTicker :: MonadIO m => SCP.ReservedPoolTicker -> DbAction m (Maybe Id.ReservedPoolTickerId)
insertReservedPoolTicker reservedPool = do
  mEntity <-
    runDbSession (mkCallInfo "insertReservedPoolTicker") $
      HsqlSes.statement reservedPool insertReservedPoolTickerStmt
  pure $ entityKey <$> mEntity

--------------------------------------------------------------------------------
queryReservedTickerStmt :: HsqlStmt.Statement Text.Text (Maybe ByteString)
queryReservedTickerStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    reservedPoolTickerTable = tableName (Proxy @SCP.ReservedPoolTicker)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT ticker.pool_hash FROM "
          , reservedPoolTickerTable
          , " ticker"
          , " WHERE ticker.name = $1"
          , " LIMIT 1"
          ]

    encoder =
      id >$< HsqlE.param (HsqlE.nonNullable HsqlE.text)

    decoder = HsqlD.rowMaybe (HsqlD.column $ HsqlD.nonNullable HsqlD.bytea)

queryReservedTicker :: MonadIO m => Text.Text -> DbAction m (Maybe ByteString)
queryReservedTicker tickerName =
  runDbSession (mkCallInfo "queryReservedTicker") $
    HsqlSes.statement tickerName queryReservedTickerStmt

--------------------------------------------------------------------------------
queryReservedTickersStmt :: HsqlStmt.Statement () [SCP.ReservedPoolTicker]
queryReservedTickersStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    reservedPoolTickerTable = tableName (Proxy @SCP.ReservedPoolTicker)
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT * FROM "
          , reservedPoolTickerTable
          ]
    encoder = mempty
    decoder = HsqlD.rowList (entityVal <$> SCP.entityReservedPoolTickerDecoder)

queryReservedTickers :: MonadIO m => DbAction m [SCP.ReservedPoolTicker]
queryReservedTickers =
  runDbSession (mkCallInfo "queryReservedTickers") $
    HsqlSes.statement () queryReservedTickersStmt

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
