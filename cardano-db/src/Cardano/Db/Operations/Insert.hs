{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Db.Operations.Insert (
  ) where

-- insertAdaPots,
-- insertBlock,
-- insertCollateralTxIn,
-- insertReferenceTxIn,
-- insertDelegation,
-- insertEpoch,
-- insertEpochParam,
-- insertEpochSyncTime,
-- insertExtraKeyWitness,
--  insertManyEpochStakes,
--  insertManyRewards,
--  insertManyRewardRests,
--  insertManyDrepDistr,
--  insertManyTxIn,
-- insertMaTxMint,
-- insertMeta,
-- insertMultiAssetUnchecked,
-- insertParamProposal,
-- insertPotTransfer,
-- insertPoolHash,
-- insertPoolMetadataRef,
-- insertPoolOwner,
-- insertPoolRelay,
-- insertPoolRetire,
-- insertPoolUpdate,
-- insertReserve,
-- insertScript,
-- insertSlotLeader,
-- insertStakeAddress,
-- insertStakeDeregistration,
-- insertStakeRegistration,
-- insertTreasury,
-- insertTx,
-- insertTxCBOR,
-- insertTxIn,
-- insertManyTxMint,
-- insertManyTxMetadata,
-- insertWithdrawal,
-- insertRedeemer,
-- insertCostModel,
-- insertDatum,
-- insertRedeemerData,
-- insertReverseIndex,
-- insertCheckOffChainPoolData,
-- insertCheckOffChainPoolFetchError,
-- insertOffChainVoteData,
-- insertOffChainVoteGovActionData,
-- insertOffChainVoteDrepData,
-- insertManyOffChainVoteAuthors,
-- insertManyOffChainVoteReference,
-- insertOffChainVoteExternalUpdate,
-- insertOffChainVoteFetchError,
-- insertReservedPoolTicker,
-- insertDelistedPool,
-- insertExtraMigration,
-- insertEpochStakeProgress,
-- updateSetComplete,
-- updateGovActionEnacted,
-- updateGovActionRatified,
-- updateGovActionDropped,
-- updateGovActionExpired,
-- setNullEnacted,
-- setNullRatified,
-- setNullExpired,
-- setNullDropped,
-- replaceAdaPots,
-- insertAnchor,
-- insertConstitution,
-- insertGovActionProposal,
-- insertTreasuryWithdrawal,
-- insertCommittee,
-- insertCommitteeMember,
-- insertVotingProcedure,
-- insertDrepHash,
-- insertCommitteeHash,
-- insertDelegationVote,
-- insertCommitteeRegistration,
-- insertCommitteeDeRegistration,
-- insertDrepRegistration,
-- insertEpochState,
-- insertManyPoolStat,
-- insertDrepHashAlwaysAbstain,
-- insertAlwaysNoConfidence,
-- insertUnchecked,
-- insertMany',
-- Export mainly for testing.
-- insertBlockChecked,

-- import Cardano.Db.Operations.Query
-- import Cardano.Db.Schema.Core
-- import Cardano.Db.Types
-- import Cardano.Prelude (textShow)
-- import Control.Exception.Lifted (Exception, handle, throwIO)
-- import Control.Monad (unless, void, when)
-- import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Control.Monad.Trans.Reader (ReaderT)
-- import qualified Data.ByteString.Char8 as BS
-- import Data.Int (Int64)
-- import qualified Data.List.NonEmpty as NonEmpty
-- import Data.Proxy (Proxy (..))
-- import Data.Text (Text)
-- import qualified Data.Text as Text
-- import Data.Word (Word64)
-- import Database.Persist (updateWhere, (!=.), (=.), (==.), (>.))
import Database.Persist.Class (
  AtLeastOneUniqueKey,
  PersistEntity,
  PersistEntityBackend,
  SafeToInsert,
  checkUnique,
  insert,
  insertBy,
  replaceUnique,
 )
import Database.Persist.EntityDef.Internal (entityDB, entityUniques)

-- import Database.Persist.Postgresql (upsertWhere)
import Database.Persist.Sql (
  OnlyOneUniqueKey,
  PersistRecordBackend,
  SqlBackend,
  UniqueDef,
  entityDef,
  insertMany,
  rawExecute,
  rawSql,
  replace,
  toPersistFields,
  toPersistValue,
  uniqueDBName,
  uniqueFields,
  updateWhereCount,
 )

-- import qualified Database.Persist.Sql.Util as Util
import Database.Persist.Types (
  ConstraintNameDB (..),
  Entity (..),
  EntityNameDB (..),
  FieldNameDB (..),
  PersistValue,
  entityKey,
 )

-- import Database.PostgreSQL.Simple (SqlError)
-- import Hasql.Statement (Statement)

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
-- Instead we use `insertUnchecked` for tables where there is no uniqueness constraints
-- and `insertChecked` for tables where the uniqueness constraint might hit.

-- insertManyEpochStakes ::
--   (MonadBaseControl IO m, MonadIO m) =>
--   -- | Does constraint already exists
--   Bool ->
--   ConstraintNameDB ->
--   [EpochStake] ->
--   ReaderT SqlBackend m ()
-- insertManyEpochStakes = insertManyWithManualUnique "Many EpochStake"

-- insertManyRewards ::
--   (MonadBaseControl IO m, MonadIO m) =>
--   -- | Does constraint already exists
--   Bool ->
--   ConstraintNameDB ->
--   [Reward] ->
--   ReaderT SqlBackend m ()
-- insertManyRewards = insertManyWithManualUnique "Many Rewards"

-- insertManyRewardRests ::
--   (MonadBaseControl IO m, MonadIO m) =>
--   [RewardRest] ->
--   ReaderT SqlBackend m ()
-- insertManyRewardRests = insertManyUnique "Many Rewards Rest" Nothing

-- insertManyDrepDistr ::
--   (MonadBaseControl IO m, MonadIO m) =>
--   [DrepDistr] ->
--   ReaderT SqlBackend m ()
-- insertManyDrepDistr = insertManyCheckUnique "Many DrepDistr"

-- updateSetComplete :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
-- updateSetComplete epoch = do
--   upsertWhere (EpochStakeProgress epoch True) [EpochStakeProgressCompleted Database.Persist.=. True] [EpochStakeProgressEpochNo Database.Persist.==. epoch]

-- replaceAdaPots :: (MonadBaseControl IO m, MonadIO m) => BlockId -> AdaPots -> ReaderT SqlBackend m Bool
-- replaceAdaPots blockId adapots = do
--   mAdaPotsId <- queryAdaPotsId blockId
--   case mAdaPotsId of
--     Nothing -> pure False
--     Just adaPotsDB
--       | entityVal adaPotsDB == adapots ->
--           pure False
--     Just adaPotsDB -> do
--       replace (entityKey adaPotsDB) adapots
--       pure True

-- --------------------------------------------------------------------------------
-- -- Custom insert functions
-- --------------------------------------------------------------------------------
-- data DbInsertException
--   = DbInsertException String SqlError
--   deriving (Show)

-- instance Exception DbInsertException

-- insertMany' ::
--   forall m record.
--   ( MonadBaseControl IO m
--   , MonadIO m
--   , PersistRecordBackend record SqlBackend
--   , SafeToInsert record
--   ) =>
--   String ->
--   [record] ->
--   ReaderT SqlBackend m [Key record]
-- insertMany' vtype records = handle exceptHandler (insertMany records)
--   where
--     exceptHandler :: SqlError -> ReaderT SqlBackend m [Key record]
--     exceptHandler e =
--       liftIO $ throwIO (DbInsertException vtype e)

-- --
-- insertManyUnique ::
--   forall m record.
--   ( MonadBaseControl IO m
--   , MonadIO m
--   , PersistEntity record
--   ) =>
--   String ->
--   -- | Does constraint already exists
--   Maybe ConstraintNameDB ->
--   [record] ->
--   ReaderT SqlBackend m ()
-- insertManyUnique vtype mConstraintName records = do
--   unless (null records) $
--     handle exceptHandler (rawExecute query values)
--   where
--     query :: Text
--     query =
--       Text.concat
--         [ "INSERT INTO "
--         , unEntityNameDB (entityDB . entityDef $ records)
--         , " ("
--         , Util.commaSeparated fieldNames
--         , ") VALUES "
--         , Util.commaSeparated
--             . replicate (length records)
--             . Util.parenWrapped
--             . Util.commaSeparated
--             $ placeholders
--         , conflictQuery
--         ]

--     values :: [PersistValue]
--     values = concatMap Util.mkInsertValues records

--     conflictQuery :: Text
--     conflictQuery =
--       case mConstraintName of
--         Just constraintName ->
--           Text.concat
--             [ " ON CONFLICT ON CONSTRAINT "
--             , unConstraintNameDB constraintName
--             , " DO NOTHING"
--             ]
--         _ -> ""

--     fieldNames, placeholders :: [Text]
--     (fieldNames, placeholders) =
--       unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

--     exceptHandler :: SqlError -> ReaderT SqlBackend m a
--     exceptHandler e =
--       liftIO $ throwIO (DbInsertException vtype e)

-- insertManyWithManualUnique ::
--   forall m record.
--   ( MonadBaseControl IO m
--   , MonadIO m
--   , PersistRecordBackend record SqlBackend
--   ) =>
--   String ->
--   -- | Does constraint already exists
--   Bool ->
--   ConstraintNameDB ->
--   [record] ->
--   ReaderT SqlBackend m ()
-- insertManyWithManualUnique str contraintExists constraintName =
--   insertManyUnique str mConstraintName
--   where
--     mConstraintName = if contraintExists then Just constraintName else Nothing

-- -- insertManyCheckUnique ::
-- --   forall m record.
-- --   ( MonadBaseControl IO m
-- --   , MonadIO m
-- --   , OnlyOneUniqueKey record
-- --   ) =>
-- --   String ->
-- --   [record] ->
-- --   ReaderT SqlBackend m ()
-- -- insertManyCheckUnique vtype records = do
-- --   let constraintName = uniqueDBName $ onlyOneUniqueDef (Proxy @record)
-- --   insertManyUnique vtype (Just constraintName) records

-- -- Insert, getting PostgreSQL to check the uniqueness constaint. If it is violated,
-- -- simply returns the Key, without changing anything.
-- insertCheckUnique ::
--   forall m record.
--   ( MonadBaseControl IO m
--   , MonadIO m
--   , OnlyOneUniqueKey record
--   , PersistRecordBackend record SqlBackend
--   ) =>
--   String ->
--   record ->
--   ReaderT SqlBackend m (Key record)
-- insertCheckUnique vtype record = do
--   res <- handle exceptHandler $ rawSql query values
--   case res of
--     [ident] -> pure ident
--     _other -> error $ mconcat ["insertCheckUnique: Inserting ", vtype, " failed with ", show res]
--   where
--     query :: Text
--     query =
--       Text.concat
--         [ "INSERT INTO "
--         , unEntityNameDB (entityDB . entityDef $ Just record)
--         , " ("
--         , Util.commaSeparated fieldNames
--         , ") VALUES ("
--         , Util.commaSeparated placeholders
--         , ") ON CONFLICT ON CONSTRAINT "
--         , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
--         , -- An update is necessary, to force Postgres to return the Id. 'EXCLUDED'
--           -- is used for the new row. 'dummyUpdateField' is a part of the Unique key
--           -- so even if it is updated with the new value on conflict, no actual
--           -- effect will take place.
--           " DO UPDATE SET "
--         , dummyUpdateField
--         , " = EXCLUDED."
--         , dummyUpdateField
--         , " RETURNING id ;"
--         ]

--     values :: [PersistValue]
--     values = map toPersistValue (toPersistFields record)

--     fieldNames, placeholders :: [Text]
--     (fieldNames, placeholders) =
--       unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

--     exceptHandler :: SqlError -> ReaderT SqlBackend m a
--     exceptHandler e =
--       liftIO $ throwIO (DbInsertException vtype e)

--     -- The first field of the Unique key
--     dummyUpdateField :: Text
--     dummyUpdateField = escapeFieldName . snd . NonEmpty.head . uniqueFields $ onlyOneUniqueDef (Proxy @record)

-- insertReplace ::
--   forall m record.
--   ( AtLeastOneUniqueKey record
--   , Eq (Unique record)
--   , MonadBaseControl IO m
--   , MonadIO m
--   , PersistRecordBackend record SqlBackend
--   , SafeToInsert record
--   ) =>
--   String ->
--   record ->
--   ReaderT SqlBackend m (Key record)
-- insertReplace vtype record =
--   handle exceptHandler $ do
--     eres <- insertBy record
--     case eres of
--       Right rid -> pure rid
--       Left rec -> do
--         mres <- replaceUnique (entityKey rec) record
--         maybe (pure $ entityKey rec) (const . pure $ entityKey rec) mres
--   where
--     exceptHandler :: SqlError -> ReaderT SqlBackend m a
--     exceptHandler e =
--       liftIO $ throwIO (DbInsertException vtype e)

-- -- Insert without checking uniqueness constraints. This should be safe for most tables
-- -- even tables with uniqueness constraints, especially block, tx and many others, where
-- -- uniqueness is enforced by the ledger.
-- insertUnchecked ::
--   ( MonadIO m
--   , MonadBaseControl IO m
--   , PersistEntityBackend record ~ SqlBackend
--   , SafeToInsert record
--   , PersistEntity record
--   ) =>
--   String ->
--   record ->
--   ReaderT SqlBackend m (Key record)
-- insertUnchecked vtype =
--   handle exceptHandler . insert
--   where
--     exceptHandler :: MonadIO m => SqlError -> ReaderT SqlBackend m a
--     exceptHandler e =
--       liftIO $ throwIO (DbInsertException vtype e)

-- -- This is cargo culted from Persistent because it is not exported.
-- escapeFieldName :: FieldNameDB -> Text
-- escapeFieldName (FieldNameDB s) =
--   Text.pack $ '"' : go (Text.unpack s) ++ "\""
--   where
--     go "" = ""
--     go ('"' : xs) = "\"\"" ++ go xs
--     go (x : xs) = x : go xs

-- This is cargo culted from Persistent because it is not exported.
-- https://github.com/yesodweb/persistent/issues/1194
-- onlyOneUniqueDef :: OnlyOneUniqueKey record => proxy record -> UniqueDef
-- onlyOneUniqueDef prxy =
--   case entityUniques (entityDef prxy) of
--     [uniq] -> uniq
--     _ -> error "impossible due to OnlyOneUniqueKey constraint"

-- Used in tests

-- insertBlockChecked :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
-- insertBlockChecked = insertCheckUnique "Block"
