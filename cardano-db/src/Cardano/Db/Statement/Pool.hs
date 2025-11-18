{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Pool where

import Cardano.Prelude (ByteString, Int64, Proxy (..), Word64)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSes
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (mkDbCallStack)
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.Pool as SCP
import qualified Cardano.Db.Schema.Ids as Id
import Cardano.Db.Statement.Function.Core (ResultType (..), ResultTypeBulk (..), runSession)
import Cardano.Db.Statement.Function.Delete (parameterisedDeleteWhere)
import Cardano.Db.Statement.Function.Insert (insert, insertCheckUnique, insertIfUnique)
import Cardano.Db.Statement.Function.InsertBulk (insertBulk)
import Cardano.Db.Statement.Function.Query (existsById, existsWhereByColumn)
import Cardano.Db.Statement.Types (DbInfo (..), Entity (..))
import Cardano.Db.Types (CertNo (..), DbM, DbWord64, PoolCert (..), PoolCertAction (..))

--------------------------------------------------------------------------------
-- DelistedPool
--------------------------------------------------------------------------------
insertDelistedPoolStmt :: HsqlStmt.Statement SCP.DelistedPool Id.DelistedPoolId
insertDelistedPoolStmt =
  insertCheckUnique
    SCP.delistedPoolEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.DelistedPoolId)

insertDelistedPool :: SCP.DelistedPool -> DbM Id.DelistedPoolId
insertDelistedPool delistedPool =
  runSession mkDbCallStack $ HsqlSes.statement delistedPool insertDelistedPoolStmt

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

queryDelistedPools :: DbM [ByteString]
queryDelistedPools =
  runSession mkDbCallStack $ HsqlSes.statement () queryDelistedPoolsStmt

--------------------------------------------------------------------------------
existsDelistedPoolStmt :: HsqlStmt.Statement ByteString Bool
existsDelistedPoolStmt =
  existsWhereByColumn
    @SCP.DelistedPool -- Specify the type explicitly
    "hash_raw" -- Column to match on
    (HsqlE.param (HsqlE.nonNullable HsqlE.bytea)) -- ByteString encoder
    (WithResult $ HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool))

-- Updated function that takes a ByteString
existsDelistedPool :: ByteString -> DbM Bool
existsDelistedPool ph =
  runSession mkDbCallStack $
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

deleteDelistedPool :: ByteString -> DbM Bool
deleteDelistedPool poolHash =
  runSession mkDbCallStack $ do
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

insertPoolHash :: SCP.PoolHash -> DbM Id.PoolHashId
insertPoolHash poolHash =
  runSession mkDbCallStack $ HsqlSes.statement poolHash insertPoolHashStmt

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

queryPoolHashId :: ByteString -> DbM (Maybe Id.PoolHashId)
queryPoolHashId hash =
  runSession mkDbCallStack $ HsqlSes.statement hash queryPoolHashIdStmt

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

insertPoolMetadataRef :: SCP.PoolMetadataRef -> DbM Id.PoolMetadataRefId
insertPoolMetadataRef poolMetadataRef =
  runSession mkDbCallStack $ HsqlSes.statement poolMetadataRef insertPoolMetadataRefStmt

--------------------------------------------------------------------------------
queryPoolMetadataRefIdExistsStmt :: HsqlStmt.Statement Id.PoolMetadataRefId Bool
queryPoolMetadataRefIdExistsStmt =
  existsById
    (Id.idEncoder Id.getPoolMetadataRefId)
    (WithResult (HsqlD.singleRow $ HsqlD.column (HsqlD.nonNullable HsqlD.bool)))

--------------------------------------------------------------------------------
deletePoolMetadataRefById :: Id.PoolMetadataRefId -> DbM ()
deletePoolMetadataRefById pmrId =
  runSession mkDbCallStack $
    HsqlSes.statement pmrId (parameterisedDeleteWhere @SCP.PoolMetadataRef "id" ">=" $ Id.idEncoder Id.getPoolMetadataRefId)

--------------------------------------------------------------------------------
-- PoolRelay
--------------------------------------------------------------------------------

insertPoolRelayStmt :: HsqlStmt.Statement SCP.PoolRelay Id.PoolRelayId
insertPoolRelayStmt =
  insert
    SCP.poolRelayEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolRelayId)

insertPoolRelay :: SCP.PoolRelay -> DbM Id.PoolRelayId
insertPoolRelay poolRelay =
  runSession mkDbCallStack $ HsqlSes.statement poolRelay insertPoolRelayStmt

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

insertBulkPoolStat :: [SCP.PoolStat] -> DbM ()
insertBulkPoolStat poolStats =
  runSession mkDbCallStack $ HsqlSes.statement poolStats insertBulkPoolStatStmt

--------------------------------------------------------------------------------
queryPoolStatCountStmt :: HsqlStmt.Statement () Int64
queryPoolStatCountStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolStatTable = tableName (Proxy @SCP.PoolStat)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint FROM "
          , poolStatTable
          ]

    encoder = mempty
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

queryPoolStatCount :: DbM Int64
queryPoolStatCount =
  runSession mkDbCallStack $ HsqlSes.statement () queryPoolStatCountStmt

--------------------------------------------------------------------------------
queryPoolStatDuplicatesStmt :: HsqlStmt.Statement () Int64
queryPoolStatDuplicatesStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    poolStatTable = tableName (Proxy @SCP.PoolStat)

    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT COUNT(*)::bigint FROM ("
          , "  SELECT pool_hash_id, epoch_no"
          , "  FROM "
          , poolStatTable
          , "  GROUP BY pool_hash_id, epoch_no"
          , "  HAVING COUNT(*) > 1"
          , ") AS duplicates"
          ]

    encoder = mempty
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.int8)

queryPoolStatDuplicates :: DbM Int64
queryPoolStatDuplicates =
  runSession mkDbCallStack $ HsqlSes.statement () queryPoolStatDuplicatesStmt

--------------------------------------------------------------------------------
-- PoolOwner
--------------------------------------------------------------------------------

insertPoolOwnerStmt :: HsqlStmt.Statement SCP.PoolOwner Id.PoolOwnerId
insertPoolOwnerStmt =
  insert
    SCP.poolOwnerEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolOwnerId)

insertPoolOwner :: SCP.PoolOwner -> DbM Id.PoolOwnerId
insertPoolOwner poolOwner =
  runSession mkDbCallStack $ HsqlSes.statement poolOwner insertPoolOwnerStmt

--------------------------------------------------------------------------------
-- PoolRetire
--------------------------------------------------------------------------------

insertPoolRetireStmt :: HsqlStmt.Statement SCP.PoolRetire Id.PoolRetireId
insertPoolRetireStmt =
  insert
    SCP.poolRetireEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolRetireId)

insertPoolRetire :: SCP.PoolRetire -> DbM Id.PoolRetireId
insertPoolRetire poolRetire =
  runSession mkDbCallStack $ HsqlSes.statement poolRetire insertPoolRetireStmt

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

queryRetiredPools :: Maybe ByteString -> DbM [PoolCert]
queryRetiredPools mPoolHash =
  runSession mkDbCallStack $ HsqlSes.statement mPoolHash queryRetiredPoolsStmt

--------------------------------------------------------------------------------
-- PoolUpdate
--------------------------------------------------------------------------------

insertPoolUpdateStmt :: HsqlStmt.Statement SCP.PoolUpdate Id.PoolUpdateId
insertPoolUpdateStmt =
  insert
    SCP.poolUpdateEncoder
    (WithResult $ HsqlD.singleRow $ Id.idDecoder Id.PoolUpdateId)

insertPoolUpdate :: SCP.PoolUpdate -> DbM Id.PoolUpdateId
insertPoolUpdate poolUpdate =
  runSession mkDbCallStack $ HsqlSes.statement poolUpdate insertPoolUpdateStmt

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

queryPoolUpdateByBlock :: Id.BlockId -> Id.PoolHashId -> DbM Bool
queryPoolUpdateByBlock blkId poolHashId =
  runSession mkDbCallStack $ HsqlSes.statement (blkId, poolHashId) queryPoolUpdateByBlockStmt

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

queryPoolRegister :: Maybe ByteString -> DbM [PoolCert]
queryPoolRegister mPoolHash =
  runSession mkDbCallStack $ HsqlSes.statement mPoolHash queryPoolRegisterStmt

--------------------------------------------------------------------------------
-- ReservedPoolTicker
--------------------------------------------------------------------------------

insertReservedPoolTickerStmt :: HsqlStmt.Statement SCP.ReservedPoolTicker (Maybe Id.ReservedPoolTickerId)
insertReservedPoolTickerStmt =
  insertIfUnique
    SCP.reservedPoolTickerEncoder
    (Id.idDecoder Id.ReservedPoolTickerId)

insertReservedPoolTicker :: SCP.ReservedPoolTicker -> DbM (Maybe Id.ReservedPoolTickerId)
insertReservedPoolTicker reservedPool =
  runSession mkDbCallStack $ HsqlSes.statement reservedPool insertReservedPoolTickerStmt

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

queryReservedTicker :: Text.Text -> DbM (Maybe ByteString)
queryReservedTicker tickerName =
  runSession mkDbCallStack $ HsqlSes.statement tickerName queryReservedTickerStmt

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

queryReservedTickers :: DbM [SCP.ReservedPoolTicker]
queryReservedTickers =
  runSession mkDbCallStack $ HsqlSes.statement () queryReservedTickersStmt
