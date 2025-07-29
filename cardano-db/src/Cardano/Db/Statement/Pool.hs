{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Pool where

import Cardano.Prelude (ByteString, Int64, MonadIO, Proxy (..), Word64)
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
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), mkDbCallStack, runDbSessionMain)
import Cardano.Db.Statement.Function.Delete (parameterisedDeleteWhere)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertIfUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Function.Query (existsById, existsWhereByColumn)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (CertNo (..), DbAction, DbWord64, PoolCert (..), PoolCertAction (..))

--------------------------------------------------------------------------------
-- DelistedPool
--------------------------------------------------------------------------------
insertDelistedPoolStmt :: HsqlStmt.Statement SCP.DelistedPool Id.DelistedPoolId
insertDelistedPoolStmt =
  insertCheckUnique
    SCP.delistedPoolEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DelistedPoolId)

insertDelistedPool :: MonadIO m => SCP.DelistedPool -> DbAction m Id.DelistedPoolId
insertDelistedPool delistedPool =
  runDbSessionMain (mkDbCallStack "insertDelistedPool") $
    HsqlSes.statement delistedPool insertDelistedPoolStmt

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
  runDbSessionMain (mkDbCallStack "queryDelistedPools") $
    HsqlSes.statement () queryDelistedPoolsStmt

--------------------------------------------------------------------------------
existsDelistedPoolStmt :: HsqlStmt.Statement ByteString Bool
existsDelistedPoolStmt =
  existsWhereByColumn
    @SCP.DelistedPool -- Specify the type explicitly
    "hash_raw" -- Column to match on
    (HsqlE.param (HsqlE.nonNullable HsqlE.bytea)) -- ByteString encoder
    (WithResult $ HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool))

-- Updated function that takes a ByteString
existsDelistedPool :: MonadIO m => ByteString -> DbAction m Bool
existsDelistedPool ph =
  runDbSessionMain (mkDbCallStack "existsDelistedPool") $
    HsqlSes.statement ph existsDelistedPoolStmt

--------------------------------------------------------------------------------
deleteDelistedPoolStmt :: HsqlStmt.Statement ByteString Int64
deleteDelistedPoolStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
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
  runDbSessionMain (mkDbCallStack "deleteDelistedPool") $ do
    count <- HsqlSes.statement poolHash deleteDelistedPoolStmt
    pure $ count > 0

--------------------------------------------------------------------------------
-- PoolHash
--------------------------------------------------------------------------------
insertPoolHashStmt :: HsqlStmt.Statement SCP.PoolHash Id.PoolHashId
insertPoolHashStmt =
  insertCheckUnique
    SCP.poolHashEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolHashId)

insertPoolHash :: MonadIO m => SCP.PoolHash -> DbAction m Id.PoolHashId
insertPoolHash poolHash =
  runDbSessionMain (mkDbCallStack "insertPoolHash") $
    HsqlSes.statement poolHash insertPoolHashStmt

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
  runDbSessionMain (mkDbCallStack "queryPoolHashId") $
    HsqlSes.statement hash queryPoolHashIdStmt

-----------------------------------------------------------------------------------
queryPoolHashIdExistsStmt :: HsqlStmt.Statement Id.PoolHashId Bool
queryPoolHashIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolHashId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

--------------------------------------------------------------------------------
-- PoolMetadataRef
--------------------------------------------------------------------------------
insertPoolMetadataRefStmt :: HsqlStmt.Statement SCP.PoolMetadataRef Id.PoolMetadataRefId
insertPoolMetadataRefStmt =
  insert
    SCP.poolMetadataRefEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolMetadataRefId)

insertPoolMetadataRef :: MonadIO m => SCP.PoolMetadataRef -> DbAction m Id.PoolMetadataRefId
insertPoolMetadataRef poolMetadataRef =
  runDbSessionMain (mkDbCallStack "insertPoolMetadataRef") $
    HsqlSes.statement poolMetadataRef insertPoolMetadataRefStmt

--------------------------------------------------------------------------------
queryPoolMetadataRefIdExistsStmt :: HsqlStmt.Statement Id.PoolMetadataRefId Bool
queryPoolMetadataRefIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

--------------------------------------------------------------------------------
deletePoolMetadataRefById :: MonadIO m => Id.PoolMetadataRefId -> DbAction m ()
deletePoolMetadataRefById pmrId =
  runDbSessionMain (mkDbCallStack "deletePoolMetadataRefById") $
    HsqlSes.statement pmrId (parameterisedDeleteWhere @SCP.PoolMetadataRef "id" ">=" $ Id.idEncoder Id.getPoolMetadataRefId)

--------------------------------------------------------------------------------
-- PoolRelay
--------------------------------------------------------------------------------

insertPoolRelayStmt :: HsqlStmt.Statement SCP.PoolRelay Id.PoolRelayId
insertPoolRelayStmt =
  insert
    SCP.poolRelayEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolRelayId)

insertPoolRelay :: MonadIO m => SCP.PoolRelay -> DbAction m Id.PoolRelayId
insertPoolRelay poolRelay =
  runDbSessionMain (mkDbCallStack "insertPoolRelay") $ HsqlSes.statement poolRelay insertPoolRelayStmt

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
insertBulkPoolStat poolStats =
  runDbSessionMain (mkDbCallStack "insertBulkPoolStat") $
    HsqlSes.statement poolStats insertBulkPoolStatStmt

--------------------------------------------------------------------------------
-- PoolOwner
--------------------------------------------------------------------------------

insertPoolOwnerStmt :: HsqlStmt.Statement SCP.PoolOwner Id.PoolOwnerId
insertPoolOwnerStmt =
  insert
    SCP.poolOwnerEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolOwnerId)

insertPoolOwner :: MonadIO m => SCP.PoolOwner -> DbAction m Id.PoolOwnerId
insertPoolOwner poolOwner =
  runDbSessionMain (mkDbCallStack "insertPoolOwner") $
    HsqlSes.statement poolOwner insertPoolOwnerStmt

--------------------------------------------------------------------------------
-- PoolRetire
--------------------------------------------------------------------------------

insertPoolRetireStmt :: HsqlStmt.Statement SCP.PoolRetire Id.PoolRetireId
insertPoolRetireStmt =
  insert
    SCP.poolRetireEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolRetireId)

insertPoolRetire :: MonadIO m => SCP.PoolRetire -> DbAction m Id.PoolRetireId
insertPoolRetire poolRetire =
  runDbSessionMain (mkDbCallStack "insertPoolRetire") $ HsqlSes.statement poolRetire insertPoolRetireStmt

--------------------------------------------------------------------------------
queryRetiredPoolsStmt :: HsqlStmt.Statement (Maybe ByteString) [PoolCert]
queryRetiredPoolsStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolRetireN = tableName (Proxy @SCP.PoolRetire)
    poolHashN = tableName (Proxy @SCP.PoolHash)
    txN = tableName (Proxy @SCB.Tx)
    blockN = tableName (Proxy @SCB.Block)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
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
      pure $
        PoolCert
          { pcHash = hsh
          , pcCertAction = Retirement retEpoch
          , pcCertNo = CertNo blkNo txIndex retIndex
          }

queryRetiredPools :: MonadIO m => Maybe ByteString -> DbAction m [PoolCert]
queryRetiredPools mPoolHash =
  runDbSessionMain (mkDbCallStack "queryRetiredPools") $
    HsqlSes.statement mPoolHash queryRetiredPoolsStmt

--------------------------------------------------------------------------------
-- PoolUpdate
--------------------------------------------------------------------------------

insertPoolUpdateStmt :: HsqlStmt.Statement SCP.PoolUpdate Id.PoolUpdateId
insertPoolUpdateStmt =
  insert
    SCP.poolUpdateEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolUpdateId)

insertPoolUpdate :: MonadIO m => SCP.PoolUpdate -> DbAction m Id.PoolUpdateId
insertPoolUpdate poolUpdate =
  runDbSessionMain (mkDbCallStack "insertPoolUpdate") $ HsqlSes.statement poolUpdate insertPoolUpdateStmt

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
  runDbSessionMain (mkDbCallStack "queryPoolUpdateByBlock") $
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
  runDbSessionMain (mkDbCallStack "queryPoolRegister") $
    HsqlSes.statement mPoolHash queryPoolRegisterStmt

--------------------------------------------------------------------------------
-- ReservedPoolTicker
--------------------------------------------------------------------------------

insertReservedPoolTickerStmt :: HsqlStmt.Statement SCP.ReservedPoolTicker (Maybe Id.ReservedPoolTickerId)
insertReservedPoolTickerStmt =
  insertIfUnique
    SCP.reservedPoolTickerEncoder
    (Id.idDecoder Id.ReservedPoolTickerId)

insertReservedPoolTicker :: MonadIO m => SCP.ReservedPoolTicker -> DbAction m (Maybe Id.ReservedPoolTickerId)
insertReservedPoolTicker reservedPool =
  runDbSessionMain (mkDbCallStack "insertReservedPoolTicker") $
    HsqlSes.statement reservedPool insertReservedPoolTickerStmt

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
  runDbSessionMain (mkDbCallStack "queryReservedTicker") $
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
  runDbSessionMain (mkDbCallStack "queryReservedTickers") $
    HsqlSes.statement () queryReservedTickersStmt
