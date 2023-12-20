{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Db.Insert (
  insertAdaPots,
  insertBlock,
  insertCollateralTxIn,
  insertReferenceTxIn,
  insertDelegation,
  insertEpoch,
  insertEpochParam,
  insertEpochSyncTime,
  insertExtraKeyWitness,
  insertManyEpochStakes,
  insertManyRewards,
  insertManyDrepDistr,
  insertManyTxIn,
  insertMaTxMint,
  insertManyMaTxOut,
  insertMeta,
  insertMultiAssetUnchecked,
  insertParamProposal,
  insertPotTransfer,
  insertPoolHash,
  insertPoolMetadataRef,
  insertPoolOwner,
  insertPoolRelay,
  insertPoolRetire,
  insertPoolUpdate,
  insertReserve,
  insertScript,
  insertSlotLeader,
  insertStakeAddress,
  insertStakeDeregistration,
  insertStakeRegistration,
  insertTreasury,
  insertTx,
  insertTxIn,
  insertManyTxMint,
  insertManyTxMetadata,
  insertTxOut,
  insertCollateralTxOut,
  insertManyTxOut,
  insertWithdrawal,
  insertRedeemer,
  insertCostModel,
  insertDatum,
  insertRedeemerData,
  insertReverseIndex,
  insertCheckOffChainPoolData,
  insertCheckOffChainPoolFetchError,
  insertOffChainVoteData,
  insertOffChainVoteFetchError,
  insertReservedPoolTicker,
  insertDelistedPool,
  insertExtraMigration,
  insertEpochStakeProgress,
  updateSetComplete,
  updateGovActionEnacted,
  updateGovActionRatified,
  setNullEnacted,
  setNullRatified,
  replaceAdaPots,
  insertAnchor,
  insertConstitution,
  insertGovActionProposal,
  insertTreasuryWithdrawal,
  insertNewCommittee,
  insertVotingProcedure,
  insertDrepHash,
  insertDelegationVote,
  insertCommitteeRegistration,
  insertCommitteeDeRegistration,
  insertDrepRegistration,
  insertAlwaysAbstainDrep,
  insertAlwaysNoConfidence,
  insertUnchecked,
  insertMany',
  -- Export mainly for testing.
  insertBlockChecked,
) where

import Cardano.Db.Query
import Cardano.Db.Schema
import Cardano.Db.Text
import Cardano.Db.Types
import Control.Exception.Lifted (Exception, handle, throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Database.Persist (updateWhere, (!=.), (=.), (==.), (>.))
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
 )
import qualified Database.Persist.Sql.Util as Util
import Database.Persist.Types (
  ConstraintNameDB (..),
  Entity (..),
  EntityNameDB (..),
  FieldNameDB (..),
  PersistValue,
  entityKey,
 )
import Database.PostgreSQL.Simple (SqlError)

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

insertAdaPots :: (MonadBaseControl IO m, MonadIO m) => AdaPots -> ReaderT SqlBackend m AdaPotsId
insertAdaPots = insertUnchecked "AdaPots"

insertBlock :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlock = insertUnchecked "Block"

insertCollateralTxIn :: (MonadBaseControl IO m, MonadIO m) => CollateralTxIn -> ReaderT SqlBackend m CollateralTxInId
insertCollateralTxIn = insertUnchecked "CollateralTxIn"

insertReferenceTxIn :: (MonadBaseControl IO m, MonadIO m) => ReferenceTxIn -> ReaderT SqlBackend m ReferenceTxInId
insertReferenceTxIn = insertUnchecked "ReferenceTxIn"

insertDelegation :: (MonadBaseControl IO m, MonadIO m) => Delegation -> ReaderT SqlBackend m DelegationId
insertDelegation = insertUnchecked "Delegation"

insertEpoch :: (MonadBaseControl IO m, MonadIO m) => Epoch -> ReaderT SqlBackend m EpochId
insertEpoch = insertCheckUnique "Epoch"

insertEpochParam :: (MonadBaseControl IO m, MonadIO m) => EpochParam -> ReaderT SqlBackend m EpochParamId
insertEpochParam = insertUnchecked "EpochParam"

insertEpochSyncTime :: (MonadBaseControl IO m, MonadIO m) => EpochSyncTime -> ReaderT SqlBackend m EpochSyncTimeId
insertEpochSyncTime = insertReplace "EpochSyncTime"

insertExtraKeyWitness :: (MonadBaseControl IO m, MonadIO m) => ExtraKeyWitness -> ReaderT SqlBackend m ExtraKeyWitnessId
insertExtraKeyWitness = insertUnchecked "ExtraKeyWitness"

insertManyEpochStakes ::
  (MonadBaseControl IO m, MonadIO m) =>
  -- | Does constraint already exists
  Bool ->
  ConstraintNameDB ->
  [EpochStake] ->
  ReaderT SqlBackend m ()
insertManyEpochStakes = insertManyWithManualUnique "Many EpochStake"

insertManyRewards ::
  (MonadBaseControl IO m, MonadIO m) =>
  -- | Does constraint already exists
  Bool ->
  ConstraintNameDB ->
  [Reward] ->
  ReaderT SqlBackend m ()
insertManyRewards = insertManyWithManualUnique "Many Rewards"

insertManyDrepDistr ::
  (MonadBaseControl IO m, MonadIO m) =>
  [DrepDistr] ->
  ReaderT SqlBackend m ()
insertManyDrepDistr = insertManyCheckUnique "Many DrepDistr"

insertManyTxIn :: (MonadBaseControl IO m, MonadIO m) => [TxIn] -> ReaderT SqlBackend m [TxInId]
insertManyTxIn = insertMany' "Many TxIn"

insertMaTxMint :: (MonadBaseControl IO m, MonadIO m) => MaTxMint -> ReaderT SqlBackend m MaTxMintId
insertMaTxMint = insertUnchecked "insertMaTxMint"

insertManyMaTxOut :: (MonadBaseControl IO m, MonadIO m) => [MaTxOut] -> ReaderT SqlBackend m [MaTxOutId]
insertManyMaTxOut = insertMany' "Many MaTxOut"

insertMeta :: (MonadBaseControl IO m, MonadIO m) => Meta -> ReaderT SqlBackend m MetaId
insertMeta = insertCheckUnique "Meta"

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
insertPoolOwner = insertUnchecked "PoolOwner"

insertPoolRelay :: (MonadBaseControl IO m, MonadIO m) => PoolRelay -> ReaderT SqlBackend m PoolRelayId
insertPoolRelay = insertUnchecked "PoolRelay"

insertPoolRetire :: (MonadBaseControl IO m, MonadIO m) => PoolRetire -> ReaderT SqlBackend m PoolRetireId
insertPoolRetire = insertUnchecked "PoolRetire"

insertPoolUpdate :: (MonadBaseControl IO m, MonadIO m) => PoolUpdate -> ReaderT SqlBackend m PoolUpdateId
insertPoolUpdate = insertUnchecked "PoolUpdate"

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

insertManyTxMetadata :: (MonadBaseControl IO m, MonadIO m) => [TxMetadata] -> ReaderT SqlBackend m [TxMetadataId]
insertManyTxMetadata = insertMany' "TxMetadata"

insertManyTxMint :: (MonadBaseControl IO m, MonadIO m) => [MaTxMint] -> ReaderT SqlBackend m [MaTxMintId]
insertManyTxMint = insertMany' "TxMint"

insertTxOut :: (MonadBaseControl IO m, MonadIO m) => TxOut -> ReaderT SqlBackend m TxOutId
insertTxOut = insertUnchecked "TxOut"

insertCollateralTxOut :: (MonadBaseControl IO m, MonadIO m) => CollateralTxOut -> ReaderT SqlBackend m CollateralTxOutId
insertCollateralTxOut = insertUnchecked "CollateralTxOut"

insertManyTxOut :: (MonadBaseControl IO m, MonadIO m) => [TxOut] -> ReaderT SqlBackend m [TxOutId]
insertManyTxOut = insertMany' "TxOut"

insertWithdrawal :: (MonadBaseControl IO m, MonadIO m) => Withdrawal -> ReaderT SqlBackend m WithdrawalId
insertWithdrawal = insertUnchecked "Withdrawal"

insertRedeemer :: (MonadBaseControl IO m, MonadIO m) => Redeemer -> ReaderT SqlBackend m RedeemerId
insertRedeemer = insertUnchecked "Redeemer"

insertCostModel :: (MonadBaseControl IO m, MonadIO m) => CostModel -> ReaderT SqlBackend m CostModelId
insertCostModel = insertCheckUnique "CostModel"

insertDatum :: (MonadBaseControl IO m, MonadIO m) => Datum -> ReaderT SqlBackend m DatumId
insertDatum = insertCheckUnique "Datum"

insertRedeemerData :: (MonadBaseControl IO m, MonadIO m) => RedeemerData -> ReaderT SqlBackend m RedeemerDataId
insertRedeemerData = insertCheckUnique "RedeemerData"

insertReverseIndex :: (MonadBaseControl IO m, MonadIO m) => ReverseIndex -> ReaderT SqlBackend m ReverseIndexId
insertReverseIndex = insertUnchecked "ReverseIndex"

insertCheckOffChainPoolData :: (MonadBaseControl IO m, MonadIO m) => OffChainPoolData -> ReaderT SqlBackend m ()
insertCheckOffChainPoolData pod = do
  foundPool <- existsPoolHashId (offChainPoolDataPoolId pod)
  foundMeta <- existsPoolMetadataRefId (offChainPoolDataPmrId pod)
  when (foundPool && foundMeta) . void $ insertCheckUnique "OffChainPoolData" pod

insertCheckOffChainPoolFetchError :: (MonadBaseControl IO m, MonadIO m) => OffChainPoolFetchError -> ReaderT SqlBackend m ()
insertCheckOffChainPoolFetchError pofe = do
  foundPool <- existsPoolHashId (offChainPoolFetchErrorPoolId pofe)
  foundMeta <- existsPoolMetadataRefId (offChainPoolFetchErrorPmrId pofe)
  when (foundPool && foundMeta) . void $ insertCheckUnique "OffChainPoolFetchError" pofe

insertOffChainVoteData :: (MonadBaseControl IO m, MonadIO m) => OffChainVoteData -> ReaderT SqlBackend m ()
insertOffChainVoteData ocvd = do
  foundVotingAnchor <- existsVotingAnchorId (offChainVoteDataVotingAnchorId ocvd)
  when foundVotingAnchor . void $ insertCheckUnique "OffChainVoteData" ocvd

insertOffChainVoteFetchError :: (MonadBaseControl IO m, MonadIO m) => OffChainVoteFetchError -> ReaderT SqlBackend m ()
insertOffChainVoteFetchError ocvfe = do
  foundVotingAnchor <- existsVotingAnchorId (offChainVoteFetchErrorVotingAnchorId ocvfe)
  when foundVotingAnchor . void $ insertCheckUnique "OffChainVoteFetchError" ocvfe

insertReservedPoolTicker :: (MonadBaseControl IO m, MonadIO m) => ReservedPoolTicker -> ReaderT SqlBackend m (Maybe ReservedPoolTickerId)
insertReservedPoolTicker ticker = do
  isUnique <- checkUnique ticker
  case isUnique of
    Nothing -> Just <$> insertUnchecked "ReservedPoolTicker" ticker
    Just _key -> pure Nothing

insertDelistedPool :: (MonadBaseControl IO m, MonadIO m) => DelistedPool -> ReaderT SqlBackend m DelistedPoolId
insertDelistedPool = insertCheckUnique "DelistedPool"

insertExtraMigration :: (MonadBaseControl IO m, MonadIO m) => ExtraMigration -> ReaderT SqlBackend m ()
insertExtraMigration token = void . insert $ ExtraMigrations (textShow token) (Just $ extraDescription token)

insertEpochStakeProgress :: (MonadBaseControl IO m, MonadIO m) => [EpochStakeProgress] -> ReaderT SqlBackend m ()
insertEpochStakeProgress =
  insertManyCheckUnique "Many EpochStakeProgress"

updateSetComplete :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
updateSetComplete epoch = do
  updateWhere [EpochStakeProgressEpochNo Database.Persist.==. epoch] [EpochStakeProgressCompleted Database.Persist.=. True]

updateGovActionEnacted :: MonadIO m => GovActionProposalId -> Word64 -> ReaderT SqlBackend m ()
updateGovActionEnacted gaid eNo =
  updateWhere [GovActionProposalId ==. gaid, GovActionProposalEnactedEpoch ==. Nothing] [GovActionProposalEnactedEpoch =. Just eNo]

updateGovActionRatified :: MonadIO m => GovActionProposalId -> Word64 -> ReaderT SqlBackend m ()
updateGovActionRatified gaid eNo =
  updateWhere [GovActionProposalId ==. gaid, GovActionProposalRatifiedEpoch ==. Nothing] [GovActionProposalRatifiedEpoch =. Just eNo]

setNullEnacted :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
setNullEnacted eNo =
  updateWhere [GovActionProposalEnactedEpoch !=. Nothing, GovActionProposalEnactedEpoch >. Just eNo] [GovActionProposalEnactedEpoch =. Nothing]

setNullRatified :: MonadIO m => Word64 -> ReaderT SqlBackend m ()
setNullRatified eNo =
  updateWhere [GovActionProposalRatifiedEpoch !=. Nothing, GovActionProposalRatifiedEpoch >. Just eNo] [GovActionProposalRatifiedEpoch =. Nothing]

replaceAdaPots :: (MonadBaseControl IO m, MonadIO m) => BlockId -> AdaPots -> ReaderT SqlBackend m Bool
replaceAdaPots blockId adapots = do
  mAdaPotsId <- queryAdaPotsId blockId
  case mAdaPotsId of
    Nothing -> pure False
    Just adaPotsDB
      | entityVal adaPotsDB == adapots ->
          pure False
    Just adaPotsDB -> do
      replace (entityKey adaPotsDB) adapots
      pure True

insertAnchor :: (MonadBaseControl IO m, MonadIO m) => VotingAnchor -> ReaderT SqlBackend m VotingAnchorId
insertAnchor = insertCheckUnique "VotingAnchor"

insertConstitution :: (MonadBaseControl IO m, MonadIO m) => Constitution -> ReaderT SqlBackend m ConstitutionId
insertConstitution = insertUnchecked "Constitution"

insertGovActionProposal :: (MonadBaseControl IO m, MonadIO m) => GovActionProposal -> ReaderT SqlBackend m GovActionProposalId
insertGovActionProposal = insertUnchecked "GovActionProposal"

insertTreasuryWithdrawal :: (MonadBaseControl IO m, MonadIO m) => TreasuryWithdrawal -> ReaderT SqlBackend m TreasuryWithdrawalId
insertTreasuryWithdrawal = insertUnchecked "TreasuryWithdrawal"

insertNewCommittee :: (MonadBaseControl IO m, MonadIO m) => NewCommittee -> ReaderT SqlBackend m NewCommitteeId
insertNewCommittee = insertUnchecked "NewCommittee"

insertVotingProcedure :: (MonadBaseControl IO m, MonadIO m) => VotingProcedure -> ReaderT SqlBackend m VotingProcedureId
insertVotingProcedure = insertUnchecked "VotingProcedure"

insertDrepHash :: (MonadBaseControl IO m, MonadIO m) => DrepHash -> ReaderT SqlBackend m DrepHashId
insertDrepHash = insertCheckUnique "DrepHash"

insertDelegationVote :: (MonadBaseControl IO m, MonadIO m) => DelegationVote -> ReaderT SqlBackend m DelegationVoteId
insertDelegationVote = insertUnchecked "DelegationVote"

insertCommitteeRegistration :: (MonadBaseControl IO m, MonadIO m) => CommitteeRegistration -> ReaderT SqlBackend m CommitteeRegistrationId
insertCommitteeRegistration = insertUnchecked "CommitteeRegistration"

insertCommitteeDeRegistration :: (MonadBaseControl IO m, MonadIO m) => CommitteeDeRegistration -> ReaderT SqlBackend m CommitteeDeRegistrationId
insertCommitteeDeRegistration = insertUnchecked "CommitteeDeRegistration"

insertDrepRegistration :: (MonadBaseControl IO m, MonadIO m) => DrepRegistration -> ReaderT SqlBackend m DrepRegistrationId
insertDrepRegistration = insertUnchecked "DrepRegistration"

insertAlwaysAbstainDrep :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m DrepHashId
insertAlwaysAbstainDrep = do
  qr <- queryDrepHashAlwaysAbstain
  maybe ins pure qr
  where
    ins =
      insertUnchecked "DrepHashAlwaysAbstain" $
        DrepHash
          { drepHashRaw = Nothing
          , drepHashView = hardcodedAlwaysAbstain
          , drepHashHasScript = False
          }

insertAlwaysNoConfidence :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m DrepHashId
insertAlwaysNoConfidence = do
  qr <- queryDrepHashAlwaysNoConfidence
  maybe ins pure qr
  where
    ins =
      insertUnchecked "DrepHashAlwaysNoConfidence" $
        DrepHash
          { drepHashRaw = Nothing
          , drepHashView = hardcodedAlwaysNoConfidence
          , drepHashHasScript = False
          }

-- -----------------------------------------------------------------------------

data DbInsertException
  = DbInsertException String SqlError
  deriving (Show)

instance Exception DbInsertException

insertMany' ::
  forall m record.
  ( MonadBaseControl IO m
  , MonadIO m
  , PersistRecordBackend record SqlBackend
  , SafeToInsert record
  ) =>
  String ->
  [record] ->
  ReaderT SqlBackend m [Key record]
insertMany' vtype records = handle exceptHandler (insertMany records)
  where
    exceptHandler :: SqlError -> ReaderT SqlBackend m [Key record]
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

--
insertManyUnique ::
  forall m record.
  ( MonadBaseControl IO m
  , MonadIO m
  , PersistEntity record
  ) =>
  String ->
  -- | Does constraint already exists
  Bool ->
  ConstraintNameDB ->
  [record] ->
  ReaderT SqlBackend m ()
insertManyUnique vtype constraintExists constraintName records = do
  unless (null records) $
    handle exceptHandler (rawExecute query values)
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ records)
        , " ("
        , Util.commaSeparated fieldNames
        , ") VALUES "
        , Util.commaSeparated
            . replicate (length records)
            . Util.parenWrapped
            . Util.commaSeparated
            $ placeholders
        , conflictQuery
        ]

    values :: [PersistValue]
    values = concatMap (map toPersistValue . toPersistFields) records

    conflictQuery :: Text
    conflictQuery =
      if constraintExists
        then
          Text.concat
            [ " ON CONFLICT ON CONSTRAINT "
            , unConstraintNameDB constraintName
            , " DO NOTHING"
            ]
        else ""

    fieldNames, placeholders :: [Text]
    (fieldNames, placeholders) =
      unzip (Util.mkInsertPlaceholders (entityDef (Proxy @record)) escapeFieldName)

    exceptHandler :: SqlError -> ReaderT SqlBackend m a
    exceptHandler e =
      liftIO $ throwIO (DbInsertException vtype e)

insertManyWithManualUnique ::
  forall m record.
  ( MonadBaseControl IO m
  , MonadIO m
  , PersistRecordBackend record SqlBackend
  ) =>
  String ->
  -- | Does constraint already exists
  Bool ->
  ConstraintNameDB ->
  [record] ->
  ReaderT SqlBackend m ()
insertManyWithManualUnique = insertManyUnique

insertManyCheckUnique ::
  forall m record.
  ( MonadBaseControl IO m
  , MonadIO m
  , OnlyOneUniqueKey record
  ) =>
  String ->
  [record] ->
  ReaderT SqlBackend m ()
insertManyCheckUnique vtype records = do
  let constraintName = uniqueDBName $ onlyOneUniqueDef (Proxy @record)
  insertManyUnique vtype True constraintName records

-- Insert, getting PostgreSQL to check the uniqueness constaint. If it is violated,
-- simply returns the Key, without changing anything.
insertCheckUnique ::
  forall m record.
  ( MonadBaseControl IO m
  , MonadIO m
  , OnlyOneUniqueKey record
  , PersistRecordBackend record SqlBackend
  ) =>
  String ->
  record ->
  ReaderT SqlBackend m (Key record)
insertCheckUnique vtype record = do
  res <- handle exceptHandler $ rawSql query values
  case res of
    [ident] -> pure ident
    _other -> error $ mconcat ["insertCheckUnique: Inserting ", vtype, " failed with ", show res]
  where
    query :: Text
    query =
      Text.concat
        [ "INSERT INTO "
        , unEntityNameDB (entityDB . entityDef $ Just record)
        , " ("
        , Util.commaSeparated fieldNames
        , ") VALUES ("
        , Util.commaSeparated placeholders
        , ") ON CONFLICT ON CONSTRAINT "
        , unConstraintNameDB (uniqueDBName $ onlyOneUniqueDef (Proxy @record))
        , -- An update is necessary, to force Postgres to return the Id. 'EXCLUDED'
          -- is used for the new row. 'dummyUpdateField' is a part of the Unique key
          -- so even if it is updated with the new value on conflict, no actual
          -- effect will take place.
          " DO UPDATE SET "
        , dummyUpdateField
        , " = EXCLUDED."
        , dummyUpdateField
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

insertReplace ::
  forall m record.
  ( AtLeastOneUniqueKey record
  , Eq (Unique record)
  , MonadBaseControl IO m
  , MonadIO m
  , PersistRecordBackend record SqlBackend
  , SafeToInsert record
  ) =>
  String ->
  record ->
  ReaderT SqlBackend m (Key record)
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
insertUnchecked ::
  ( MonadIO m
  , MonadBaseControl IO m
  , PersistEntityBackend record ~ SqlBackend
  , SafeToInsert record
  , PersistEntity record
  ) =>
  String ->
  record ->
  ReaderT SqlBackend m (Key record)
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
    go ('"' : xs) = "\"\"" ++ go xs
    go (x : xs) = x : go xs

-- This is cargo culted from Persistent because it is not exported.
-- https://github.com/yesodweb/persistent/issues/1194
onlyOneUniqueDef :: OnlyOneUniqueKey record => proxy record -> UniqueDef
onlyOneUniqueDef prxy =
  case entityUniques (entityDef prxy) of
    [uniq] -> uniq
    _ -> error "impossible due to OnlyOneUniqueKey constraint"

-- Used in tests

insertBlockChecked :: (MonadBaseControl IO m, MonadIO m) => Block -> ReaderT SqlBackend m BlockId
insertBlockChecked = insertCheckUnique "Block"
