{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Insert
  ( insertAdaPots
  , insertBlock
  , insertCollateralTxIn
  , insertDelegation
  , insertEpoch
  , insertEpochParam
  , insertEpochRewardTotalReceived
  , insertEpochSyncTime
  , insertManyEpochStakes
  , insertManyRewards
  , insertManyTxIn
  , insertMaTxMint
  , insertMaTxOut
  , insertManyMaTxOut
  , insertMeta
  , insertMultiAsset
  , insertMultiAssetUnchecked
  , insertParamProposal
  , insertPotTransfer
  , insertPoolHash
  , insertPoolMetadataRef
  , insertPoolOwner
  , insertPoolRelay
  , insertPoolRetire
  , insertPoolUpdate
  , insertReserve
  , insertScript
  , insertSlotLeader
  , insertStakeAddress
  , insertStakeDeregistration
  , insertStakeRegistration
  , insertTreasury
  , insertTx
  , insertTxIn
  , insertTxMetadata
  , insertTxOut
  , insertWithdrawal
  , insertRedeemer
  , insertCostModel
  , insertDatum
  , insertCheckPoolOfflineData
  , insertCheckPoolOfflineFetchError
  , insertReservedPoolTicker
  , insertDelistedPool

  -- Export mainly for testing.
  , insertBlockChecked
  , insertCheckUnique
  , insertManyUncheckedUnique
  , insertUnchecked
  ) where


import           Control.Exception.Lifted (Exception, handle, throwIO)
import           Control.Monad (unless, void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import qualified Data.Text as Text

import           Database.Persist.Class (AtLeastOneUniqueKey, PersistEntityBackend, checkUnique,
                   insert, insertBy, replaceUnique)
import           Database.Persist.EntityDef.Internal (entityDB, entityUniques)
import           Database.Persist.Sql (OnlyOneUniqueKey, PersistRecordBackend, SqlBackend,
                   UniqueDef, entityDef, rawExecute, rawSql, toPersistFields, toPersistValue,
                   uniqueDBName, uniqueFields)
import qualified Database.Persist.Sql.Util as Util
import           Database.Persist.Types (ConstraintNameDB (..), EntityNameDB (..), FieldNameDB (..),
                   PersistValue, entityKey)
import           Database.PostgreSQL.Simple (SqlError)

import           Cardano.Db.Query
import           Cardano.Db.Schema


-- The original naive way of inserting rows into Postgres was:
--
--     insertByReturnKey :: record -> ReaderT SqlBackend m recordId
--        res <- getByValue value
--        case res of
--          Nothing -> insertBy value
--          Just ident -> pure ident
--
-- Unfortunately this is relatively slow if the row is not already found in the database.
--
-- One alternative is to just use `insert` but that fails on some uniquness constraints on some
-- tables (about 6 out of a total of 25+).
--
-- Instead we use `insertUnchecked` for tables where uniqueness constraints are unlikley to be hit
-- and `insertChecked` for tables where the uniqueness constraint might can be hit.

insertAdaPots :: (MonadBaseControl IO m, MonadIO m) => AdaPots -> ReaderT SqlBackend m AdaPotsId
insertAdaPots = insertCheckUnique "AdaPots"

insertBlock :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlock = insertUnchecked "Block"

insertBlockChecked :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlockChecked = insertCheckUnique "Block"

insertCollateralTxIn :: (MonadBaseControl IO m, MonadIO m) => CollateralTxIn -> ReaderT SqlBackend m CollateralTxInId
insertCollateralTxIn = insertUnchecked "CollateralTxIn"

insertDelegation :: (MonadBaseControl IO m, MonadIO m) => Delegation -> ReaderT SqlBackend m DelegationId
insertDelegation = insertCheckUnique "Delegation"

insertEpoch :: (MonadBaseControl IO m, MonadIO m) => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertUnchecked "Epoch"

insertEpochParam :: (MonadBaseControl IO m, MonadIO m) => EpochParam -> ReaderT SqlBackend m EpochParamId
insertEpochParam = insertUnchecked "EpochParam"

insertEpochRewardTotalReceived :: (MonadBaseControl IO m, MonadIO m) => EpochRewardTotalReceived -> ReaderT SqlBackend m EpochRewardTotalReceivedId
insertEpochRewardTotalReceived = insertCheckUnique "EpochRewardTotalReceived"

insertEpochSyncTime :: (MonadBaseControl IO m, MonadIO m) => EpochSyncTime -> ReaderT SqlBackend m EpochSyncTimeId
insertEpochSyncTime = insertReplace "EpochSyncTime"

insertManyEpochStakes :: (MonadBaseControl IO m, MonadIO m) => [EpochStake] -> ReaderT SqlBackend m ()
insertManyEpochStakes = insertManyUncheckedUnique "Many EpochStake"

insertManyRewards :: (MonadBaseControl IO m, MonadIO m) => [Reward] -> ReaderT SqlBackend m ()
insertManyRewards = insertManyUncheckedUnique "Many Rewards"

insertManyTxIn :: (MonadBaseControl IO m, MonadIO m) => [TxIn] -> ReaderT SqlBackend m ()
insertManyTxIn = insertManyUncheckedUnique "Many TxIn"

insertMaTxMint :: (MonadBaseControl IO m, MonadIO m) => MaTxMint -> ReaderT SqlBackend m MaTxMintId
insertMaTxMint = insertCheckUnique "insertMaTxMint"

insertMaTxOut :: (MonadBaseControl IO m, MonadIO m) => MaTxOut -> ReaderT SqlBackend m MaTxOutId
insertMaTxOut = insertCheckUnique "insertMaTxOut"

insertManyMaTxOut :: (MonadBaseControl IO m, MonadIO m) => [MaTxOut] -> ReaderT SqlBackend m ()
insertManyMaTxOut = insertManyUncheckedUnique "Many MaTxOut"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertCheckUnique "Meta"

insertMultiAsset :: (MonadBaseControl IO m, MonadIO m) => MultiAsset -> ReaderT SqlBackend m MultiAssetId
insertMultiAsset = insertCheckUnique "MultiAsset"

insertMultiAssetUnchecked :: (MonadBaseControl IO m, MonadIO m) => MultiAsset -> ReaderT SqlBackend m MultiAssetId
insertMultiAssetUnchecked = insertUnchecked "MultiAsset"

insertParamProposal :: (MonadBaseControl IO m, MonadIO m) => ParamProposal -> ReaderT SqlBackend m ParamProposalId
insertParamProposal = insertUnchecked "ParamProposal"

insertPotTransfer :: (MonadBaseControl IO m, MonadIO m) => PotTransfer -> ReaderT SqlBackend m PotTransferId
insertPotTransfer = insertUnchecked "PotTransfer"

insertPoolHash :: (MonadBaseControl IO m, MonadIO m) => PoolHash -> ReaderT SqlBackend m PoolHashId
insertPoolHash = insertCheckUnique "PoolHash"

insertPoolMetadataRef :: (MonadBaseControl IO m, MonadIO m) => PoolMetadataRef -> ReaderT SqlBackend m PoolMetadataRefId
insertPoolMetadataRef = insertCheckUnique "PoolMetadataRef"

insertPoolOwner :: (MonadBaseControl IO m, MonadIO m) => PoolOwner -> ReaderT SqlBackend m PoolOwnerId
insertPoolOwner = insertCheckUnique "PoolOwner"

insertPoolRelay :: (MonadBaseControl IO m, MonadIO m) => PoolRelay -> ReaderT SqlBackend m PoolRelayId
insertPoolRelay = insertUnchecked "PoolRelay"

insertPoolRetire :: (MonadBaseControl IO m, MonadIO m) => PoolRetire -> ReaderT SqlBackend m PoolRetireId
insertPoolRetire = insertUnchecked "PoolRetire"

insertPoolUpdate :: (MonadBaseControl IO m, MonadIO m) => PoolUpdate -> ReaderT SqlBackend m PoolUpdateId
insertPoolUpdate = insertCheckUnique "PoolUpdate"

insertReserve :: (MonadBaseControl IO m, MonadIO m) => Reserve -> ReaderT SqlBackend m ReserveId
insertReserve = insertUnchecked "Reserve"

insertScript :: (MonadBaseControl IO m, MonadIO m) => Script -> ReaderT SqlBackend m ScriptId
insertScript = insertCheckUnique "insertScript"

insertSlotLeader :: (MonadBaseControl IO m, MonadIO m) => SlotLeader -> ReaderT SqlBackend m SlotLeaderId
insertSlotLeader = insertCheckUnique "SlotLeader"

insertStakeAddress :: (MonadBaseControl IO m, MonadIO m) => StakeAddress -> ReaderT SqlBackend m StakeAddressId
insertStakeAddress = insertCheckUnique "StakeAddress"

insertStakeDeregistration :: (MonadBaseControl IO m, MonadIO m) => StakeDeregistration -> ReaderT SqlBackend m StakeDeregistrationId
insertStakeDeregistration = insertUnchecked "StakeDeregistration"

insertStakeRegistration :: (MonadBaseControl IO m, MonadIO m) => StakeRegistration -> ReaderT SqlBackend m StakeRegistrationId
insertStakeRegistration = insertUnchecked "StakeRegistration"

insertTreasury :: (MonadBaseControl IO m, MonadIO m) => Treasury -> ReaderT SqlBackend m TreasuryId
insertTreasury = insertUnchecked "Treasury"

insertTx :: (MonadBaseControl IO m, MonadIO m) => Tx -> ReaderT SqlBackend m TxId
insertTx tx = insertUnchecked ("Tx: " ++ show (BS.length (txHash tx))) tx

insertTxIn :: (MonadBaseControl IO m, MonadIO m) => TxIn -> ReaderT SqlBackend m TxInId
insertTxIn = insertUnchecked "TxIn"

insertTxMetadata :: (MonadBaseControl IO m, MonadIO m) => TxMetadata -> ReaderT SqlBackend m TxMetadataId
insertTxMetadata = insertCheckUnique "TxMetadata"

insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertUnchecked "TxOut"

insertWithdrawal :: (MonadBaseControl IO m, MonadIO m) => Withdrawal  -> ReaderT SqlBackend m WithdrawalId
insertWithdrawal = insertUnchecked "Withdrawal"

insertRedeemer :: (MonadBaseControl IO m, MonadIO m) => Redeemer -> ReaderT SqlBackend m RedeemerId
insertRedeemer = insertCheckUnique "Redeemer"

insertCostModel :: (MonadBaseControl IO m, MonadIO m) => CostModel -> ReaderT SqlBackend m CostModelId
insertCostModel = insertCheckUnique "CostModel"

insertDatum :: (MonadBaseControl IO m, MonadIO m) => Datum -> ReaderT SqlBackend m DatumId
insertDatum = insertCheckUnique "Datum"

insertCheckPoolOfflineData :: (MonadBaseControl IO m, MonadIO m) => PoolOfflineData -> ReaderT SqlBackend m ()
insertCheckPoolOfflineData pod = do
  foundPool <- existsPoolHashId (poolOfflineDataPoolId pod)
  foundMeta <- existsPoolMetadataRefId (poolOfflineDataPmrId pod)
  when (foundPool && foundMeta) . void $ insertCheckUnique "PoolOfflineData" pod

insertCheckPoolOfflineFetchError :: (MonadBaseControl IO m, MonadIO m) => PoolOfflineFetchError -> ReaderT SqlBackend m ()
insertCheckPoolOfflineFetchError pofe = do
  foundPool <- existsPoolHashId (poolOfflineFetchErrorPoolId pofe)
  foundMeta <- existsPoolMetadataRefId (poolOfflineFetchErrorPmrId pofe)
  when (foundPool && foundMeta) . void $ insertCheckUnique "PoolOfflineFetchError" pofe

insertReservedPoolTicker :: (MonadBaseControl IO m, MonadIO m) => ReservedPoolTicker -> ReaderT SqlBackend m (Maybe ReservedPoolTickerId)
insertReservedPoolTicker ticker = do
  isUnique <- checkUnique ticker
  case isUnique of
    Nothing -> Just <$> insertUnchecked "ReservedPoolTicker" ticker
    Just _key -> pure Nothing

insertDelistedPool :: (MonadBaseControl IO m, MonadIO m) => DelistedPool -> ReaderT SqlBackend m DelistedPoolId
insertDelistedPool = insertCheckUnique "DelistedPool"

-- -----------------------------------------------------------------------------

data DbInsertException
  = DbInsertException String SqlError
  deriving Show

instance Exception DbInsertException

insertManyUncheckedUnique
    :: forall m record.
        ( MonadBaseControl IO m
        , MonadIO m
        , OnlyOneUniqueKey record
        )
    => String -> [record] -> ReaderT SqlBackend m ()
insertManyUncheckedUnique vtype records =
    unless (null records) $
      handle exceptHandler (rawExecute query values)
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ records)
        , " (", Util.commaSeparated fieldNames
        , ") VALUES "
        ,  Util.commaSeparated . replicate (length records)
             . Util.parenWrapped . Util.commaSeparated $ placeholders
        , " ON CONFLICT ON CONSTRAINT "
        , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
        , " DO NOTHING"
        ]

    values :: [PersistValue]
    values = concatMap (map toPersistValue . toPersistFields) records

    fieldNames, placeholders :: [Text]
    (fieldNames, placeholders) =
      unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

-- Insert, getting PostgreSQL to check the uniqueness constaint. If it is violated,
-- simply returns the Key, without changing anything.
insertCheckUnique
    :: forall m record.
        ( MonadBaseControl IO m
        , MonadIO m
        , OnlyOneUniqueKey record
        , PersistRecordBackend record SqlBackend
        )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertCheckUnique vtype record = do
    res <- handle exceptHandler $ rawSql query values
    case res of
      [ident] -> pure ident
      _other -> error $ mconcat [ "insertCheckUnique: Inserting ", vtype, " failed with ", show res ]
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ Just record)
        , " (", Util.commaSeparated fieldNames
        , ") VALUES (", Util.commaSeparated placeholders
        , ") ON CONFLICT ON CONSTRAINT "
        , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
        -- An update is necessary, to force Postgres to return the Id. 'EXCLUDED'
        -- is used for the new row. 'dummyUpdateField' is a part of the Unique key
        -- so even if it is updated with the new value on conflict, no actual
        -- effect will take place.
        , " DO UPDATE SET ", dummyUpdateField, " = EXCLUDED.", dummyUpdateField
        , " RETURNING id ;"
        ]

    values :: [PersistValue]
    values = map toPersistValue (toPersistFields record)

    fieldNames, placeholders :: [Text]
    (fieldNames, placeholders) =
      unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

    -- The first field of the Unique key
    dummyUpdateField :: Text
    dummyUpdateField = escapeFieldName . snd . NonEmpty.head . uniqueFields $ onlyOneUniqueDef (Proxy @record)

insertReplace
    :: forall m record.
        ( AtLeastOneUniqueKey record
        , Eq (Unique record)
        , MonadBaseControl IO m
        , MonadIO m
        , PersistRecordBackend record SqlBackend
        )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertReplace vtype record =
    handle exceptHandler $ do
      eres <- insertBy record
      case eres of
        Right rid -> pure rid
        Left rec -> do
          mres <- replaceUnique (entityKey rec) record
          maybe (pure $ entityKey rec) (const . pure $ entityKey rec) mres
  where
    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

-- Insert without checking uniqueness constraints. This should be safe for most tables
-- even tables with uniqueness constraints, especially block, tx and many others, where
-- uniqueness is enforced by the ledger.
insertUnchecked
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , MonadBaseControl IO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => String -> record -> ReaderT SqlBackend m (Key record)
insertUnchecked vtype =
    handle exceptHandler . insert
  where
    exceptHandler :: MonadIO m => SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)


-- This is cargo culted from Persistent because it is not exported.
escapeFieldName :: FieldNameDB -> Text
escapeFieldName (FieldNameDB s) =
    Text.pack $ '"' : go (Text.unpack s) ++ "\""
  where
    go "" = ""
    go ('"':xs) = "\"\"" ++ go xs
    go (x:xs) = x : go xs

-- This is cargo culted from Persistent because it is not exported.
-- https://github.com/yesodweb/persistent/issues/1194
onlyOneUniqueDef :: OnlyOneUniqueKey record => proxy record -> UniqueDef
onlyOneUniqueDef prxy =
    case entityUniques (entityDef prxy) of
        [uniq] -> uniq
        _ -> error "impossible due to OnlyOneUniqueKey constraint"
