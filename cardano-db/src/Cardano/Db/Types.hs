{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Db.Types (
  DbAction (..),
  DbTxMode (..),
  DbTransaction (..),
  DbEnv (..),
  Ada (..),
  AnchorType (..),
  AssetFingerprint (..),
  DbLovelace (..),
  DbInt65 (..),
  DbWord64 (..),
  RewardSource (..),
  SyncState (..),
  ScriptPurpose (..),
  ScriptType (..),
  PoolCertAction (..),
  PruneConsumeMigration (..),
  CertNo (..),
  PoolCert (..),
  ExtraMigration (..),
  MigrationValues (..),
  VoteUrl (..),
  VoteMetaHash (..),
  Vote (..),
  VoterRole (..),
  GovActionType (..),
  BootstrapState (..),
  dbInt65Decoder,
  dbInt65Encoder,
  rewardSourceDecoder,
  rewardSourceEncoder,
  dbLovelaceDecoder,
  maybeDbLovelaceDecoder,
  dbLovelaceEncoder,
  maybeDbLovelaceEncoder,
  dbWord64Decoder,
  maybeDbWord64Decoder,
  dbWord64Encoder,
  maybeDbWord64Encoder,
  processMigrationValues,
  isStakeDistrComplete,
  bootstrapState,
  extraDescription,
  deltaCoinToDbInt65,
  integerToDbInt65,
  lovelaceToAda,
  mkAssetFingerprint,
  renderAda,
  scientificToAda,
  rewardSourceFromText,
  syncStateToText,
  syncStateFromText,
  syncStateDecoder,
  syncStateEncoder,
  scriptPurposeDecoder,
  scriptPurposeEncoder,
  scriptPurposeFromText,
  scriptPurposeToText,
  scriptTypeEncoder,
  scriptTypeDecoder,
  scriptTypeFromText,
  scriptTypeToText,
  rewardSourceToText,
  voteEncoder,
  voteDecoder,
  voterRoleEncoder,
  voterRoleDecoder,
  voteToText,
  voteFromText,
  voterRoleToText,
  voterRoleFromText,
  voteUrlDecoder,
  voteUrlEncoder,
  govActionTypeToText,
  govActionTypeFromText,
  govActionTypeDecoder,
  govActionTypeEncoder,
  anchorTypeToText,
  anchorTypeFromText,
  anchorTypeDecoder,
  anchorTypeEncoder,
  word64ToAda,
  word128Decoder,
  word128Encoder,
  hardcodedAlwaysAbstain,
  hardcodedAlwaysNoConfidence,
) where

import Cardano.Ledger.Coin (DeltaCoin (..))
import qualified Codec.Binary.Bech32 as Bech32
import Crypto.Hash (Blake2b_160)
import qualified Crypto.Hash
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.Either (fromRight)
import Data.Fixed (Micro, showFixed)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Quiet (Quiet (..))
import Data.Int (Int64)
import Cardano.Prelude (Bifunctor(..), MonadError (..), MonadIO (..), ask, MonadReader, when)
import Data.Bits (Bits(..))
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Data.Functor.Contravariant ((>$<))
import Data.WideWord (Word128 (..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Cardano.Db.Error (DbError (..), CallSite (..))
import qualified Hasql.Connection as HsqlC
import qualified Hasql.Session as HsqlS
import qualified Hasql.Transaction as HsqlT
import qualified Hasql.Transaction.Sessions as HsqlT
import Cardano.BM.Trace (Trace, logDebug)
import GHC.Stack (SrcLoc (..), HasCallStack, getCallStack, callStack)
import Data.Time (getCurrentTime, diffUTCTime)


newtype DbAction m a = DbAction
  { runDbAction :: ExceptT DbError (ReaderT DbEnv m) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadError DbError
    , MonadReader DbEnv
    , MonadIO
    )

data DbTxMode = Write | ReadOnly

-- Environment with transaction settings
data DbEnv = DbEnv
  { dbConnection :: !HsqlC.Connection
  , dbEnableLogging :: !Bool
  ,dbTracer :: !(Trace IO Text)
  }

-- | Transaction wrapper for debuging/logging.
data DbTransaction a = DbTransaction
  { dtFunctionName :: !Text
  , dtCallSite :: !CallSite
  , dtTx :: !(HsqlT.Transaction a)
  }

newtype Ada = Ada
  { unAda :: Micro
  }
  deriving (Eq, Num, Ord, Generic)

instance FromJSON Ada where
  parseJSON =
    Aeson.withScientific "Ada" (pure . scientificToAda)

instance ToJSON Ada where
  -- toJSON (Ada ada) = Data.Aeson.Types.Number $ fromRational $ toRational ada
  -- `Number` results in it becoming `7.3112484749601107e10` while the old explorer is returning `73112484749.601107`
  toEncoding (Ada ada) =
    unsafeToEncoding $
      Builder.string8 $ -- convert ByteString to Aeson's
        showFixed True ada -- convert String to ByteString using Latin1 encoding
        -- convert Micro to String chopping off trailing zeros

  toJSON = error "Ada.toJSON not supported due to numeric issues. Use toEncoding instead."

instance Show Ada where
  show (Ada ada) = showFixed True ada

newtype AssetFingerprint = AssetFingerprint
  { unAssetFingerprint :: Text
  }
  deriving (Eq, Show)

mkAssetFingerprint :: ByteString -> ByteString -> AssetFingerprint
mkAssetFingerprint policyBs assetNameBs =
  AssetFingerprint
    . Bech32.encodeLenient hrp
    . Bech32.dataPartFromBytes
    . ByteArray.convert
    $ Crypto.Hash.hash @_ @Blake2b_160 (policyBs <> assetNameBs)
  where
    hrp :: Bech32.HumanReadablePart
    hrp =
      fromRight (error "mkAssetFingerprint: Bad human readable part") $
        Bech32.humanReadablePartFromText "asset" -- Should never happen

-- This is horrible. Need a 'Word64' with an extra sign bit.
-- data DbInt65
--   = PosInt65 !Word64
--   | NegInt65 !Word64
--   deriving (Eq, Generic, Show)

newtype DbInt65 = DbInt65 { unDbInt65 :: Word64 }
  deriving (Eq, Generic)

instance Show DbInt65 where
  show = show . fromDbInt65

instance Read DbInt65 where
  readsPrec d = map (first toDbInt65) . readsPrec d

dbInt65Decoder :: D.Value DbInt65
dbInt65Decoder = toDbInt65 <$> D.int8

dbInt65Encoder :: E.Value DbInt65
dbInt65Encoder = fromDbInt65 >$< E.int8

-- Helper functions to pack/unpack the sign and value
toDbInt65 :: Int64 -> DbInt65
toDbInt65 n = DbInt65 $
  if n >= 0
    then fromIntegral n
    else setBit (fromIntegral (abs n)) 63  -- Set sign bit for negative


fromDbInt65 :: DbInt65 -> Int64
fromDbInt65 (DbInt65 w) =
  if testBit w 63
    then negate $ fromIntegral (clearBit w 63)  -- Clear sign bit for value
    else fromIntegral w

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbLovelace = DbLovelace {unDbLovelace :: Word64}
  deriving (Eq, Generic, Ord)
  deriving (Read, Show) via (Quiet DbLovelace)

dbLovelaceEncoder :: E.Params DbLovelace
dbLovelaceEncoder = E.param $ E.nonNullable $ fromIntegral . unDbLovelace >$< E.int8

maybeDbLovelaceEncoder :: E.Params (Maybe DbLovelace)
maybeDbLovelaceEncoder = E.param $ E.nullable $ fromIntegral . unDbLovelace >$< E.int8

dbLovelaceDecoder :: D.Row DbLovelace
dbLovelaceDecoder = D.column (D.nonNullable (DbLovelace . fromIntegral <$> D.int8))

maybeDbLovelaceDecoder :: D.Row (Maybe DbLovelace)
maybeDbLovelaceDecoder = D.column (D.nullable (DbLovelace . fromIntegral <$> D.int8))

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbWord64 = DbWord64 {unDbWord64 :: Word64}
  deriving (Eq, Generic, Num)
  deriving (Read, Show) via (Quiet DbWord64)

dbWord64Encoder :: E.Params DbWord64
dbWord64Encoder = E.param $ E.nonNullable $ fromIntegral . unDbWord64 >$< E.int8

maybeDbWord64Encoder :: E.Params (Maybe DbWord64)
maybeDbWord64Encoder = E.param $ E.nullable $ fromIntegral . unDbWord64 >$< E.int8

dbWord64Decoder :: D.Row DbWord64
dbWord64Decoder = D.column (D.nonNullable (DbWord64 . fromIntegral <$> D.int8))

maybeDbWord64Decoder :: D.Row (Maybe DbWord64)
maybeDbWord64Decoder = D.column (D.nullable (DbWord64 . fromIntegral <$> D.int8))

--------------------------------------------------------------------------------
-- The following must be in alphabetic order.
data RewardSource
  = RwdLeader
  | RwdMember
  | RwdReserves
  | RwdTreasury
  | RwdDepositRefund
  | RwdProposalRefund
  deriving (Bounded, Enum, Eq, Ord, Show)

rewardSourceDecoder :: D.Value RewardSource
rewardSourceDecoder = D.enum $ \case
  "leader" -> Just RwdLeader
  "member" -> Just RwdMember
  "reserves" -> Just RwdReserves
  "treasury" -> Just RwdTreasury
  "deposit_refund" -> Just RwdDepositRefund
  "proposal_refund" -> Just RwdProposalRefund
  _ -> Nothing

rewardSourceEncoder :: E.Value RewardSource
rewardSourceEncoder = E.enum $ \case
  RwdLeader -> "leader"
  RwdMember -> "member"
  RwdReserves -> "reserves"
  RwdTreasury -> "treasury"
  RwdDepositRefund -> "deposit_refund"
  RwdProposalRefund -> "proposal_refund"

--------------------------------------------------------------------------------
data SyncState
  = SyncLagging -- Local tip is lagging the global chain tip.
  | SyncFollowing -- Local tip is following global chain tip.
  deriving (Eq, Show)

syncStateDecoder :: D.Value SyncState
syncStateDecoder = D.enum $ \case
  "lagging" -> Just SyncLagging
  "following" -> Just SyncFollowing
  _ -> Nothing

syncStateEncoder :: E.Value SyncState
syncStateEncoder = E.enum $ \case
  SyncLagging -> "lagging"
  SyncFollowing -> "following"

--------------------------------------------------------------------------------
data ScriptPurpose
  = Spend
  | Mint
  | Cert
  | Rewrd
  | Vote
  | Propose
  deriving (Eq, Generic, Show)

scriptPurposeDecoder :: D.Value ScriptPurpose
scriptPurposeDecoder = D.enum $ \case
  "spend" -> Just Spend
  "mint" -> Just Mint
  "cert" -> Just Cert
  "reward" -> Just Rewrd
  "vote" -> Just Vote
  "propose" -> Just Propose
  _ -> Nothing

scriptPurposeEncoder :: E.Value ScriptPurpose
scriptPurposeEncoder = E.enum $ \case
  Spend -> "spend"
  Mint -> "mint"
  Cert -> "cert"
  Rewrd -> "reward"
  Vote -> "vote"
  Propose -> "propose"

--------------------------------------------------------------------------------
data ScriptType
  = MultiSig
  | Timelock
  | PlutusV1
  | PlutusV2
  | PlutusV3
  deriving (Eq, Generic, Show)

scriptTypeDecoder :: D.Value ScriptType
scriptTypeDecoder = D.enum $ \case
  "multisig" -> Just MultiSig
  "timelock" -> Just Timelock
  "plutusv1" -> Just PlutusV1
  "plutusv2" -> Just PlutusV2
  "plutusv3" -> Just PlutusV3
  _ -> Nothing

scriptTypeEncoder :: E.Value ScriptType
scriptTypeEncoder = E.enum $ \case
  MultiSig -> "multisig"
  Timelock -> "timelock"
  PlutusV1 -> "plutusv1"
  PlutusV2 -> "plutusv2"
  PlutusV3 -> "plutusv3"

--------------------------------------------------------------------------------
data PoolCertAction
  = Retirement !Word64 -- retirement epoch
  | Register !ByteString -- metadata hash
  deriving (Eq, Show)

-- | A Unique identifier for a certificate in the
-- blockchain. Ord instance gives a chronological order.
data CertNo = CertNo
  { ciBlockNo :: !(Maybe Word64)
  , ciTxIndex :: !Word64
  , ciCertIndex :: !Word16
  }
  deriving (Eq, Ord, Show)

data PoolCert = PoolCert
  { pcHash :: !ByteString
  , pcCertAction :: !PoolCertAction
  , pcCertNo :: !CertNo
  }
  deriving (Eq, Show)

data ExtraMigration
  = StakeDistrEnded
  | PruneTxOutFlagPreviouslySet
  | BootstrapStarted
  | BootstrapFinished
  | ConsumeTxOutPreviouslySet
  | TxOutAddressPreviouslySet
  deriving (Eq, Show, Read)

data MigrationValues = MigrationValues
  { isStakeDistrEnded :: !Bool
  , isPruneTxOutPreviouslySet :: !Bool
  , isConsumeTxOutPreviouslySet :: !Bool
  , isTxOutAddressPreviouslySet :: !Bool
  , pruneConsumeMigration :: !PruneConsumeMigration
  }
  deriving (Eq, Show)

processMigrationValues :: [ExtraMigration] -> PruneConsumeMigration -> MigrationValues
processMigrationValues migrations pcm =
  MigrationValues
    { isStakeDistrEnded = StakeDistrEnded `elem` migrations
    , isPruneTxOutPreviouslySet = PruneTxOutFlagPreviouslySet `elem` migrations
    , isConsumeTxOutPreviouslySet = ConsumeTxOutPreviouslySet `elem` migrations
    , isTxOutAddressPreviouslySet = TxOutAddressPreviouslySet `elem` migrations
    , pruneConsumeMigration = pcm
    }

isStakeDistrComplete :: [ExtraMigration] -> Bool
isStakeDistrComplete = elem StakeDistrEnded

data BootstrapState
  = BootstrapNotStarted
  | BootstrapInProgress
  | BootstrapDone

bootstrapState :: [ExtraMigration] -> BootstrapState
bootstrapState ls =
  case (BootstrapStarted `elem` ls, BootstrapFinished `elem` ls) of
    (False, False) -> BootstrapNotStarted
    (True, False) -> BootstrapInProgress
    (_, True) -> BootstrapDone

data PruneConsumeMigration = PruneConsumeMigration
  { pcmPruneTxOut :: Bool
  , -- we make the assumption that if the user is using prune flag
    -- they will also want consume automatically set for them.
    pcmConsumedTxOut :: Bool
  , pcmSkipTxIn :: Bool
  }
  deriving (Eq, Show)

extraDescription :: ExtraMigration -> Text
extraDescription = \case
  StakeDistrEnded ->
    "The epoch_stake table has been migrated. It is now populated earlier during the previous era. \
    \Also the epoch_stake_progress table is introduced."
  PruneTxOutFlagPreviouslySet ->
    "The --prune-tx-out flag has previously been enabled, now db-sync can't be run without the flag enabled"
  BootstrapStarted ->
    "The bootstrap syncing is in progress"
  BootstrapFinished ->
    "The bootstrap is finalised"
  ConsumeTxOutPreviouslySet ->
    "The --consume-tx-out flag has previously been enabled"
  TxOutAddressPreviouslySet ->
    "The addition of a Address table for TxOuts was previously set"
instance Ord PoolCert where
  compare a b = compare (pcCertNo a) (pcCertNo b)

--------------------------------------------------------------------------------
-- | The vote url wrapper so we have some additional safety.
newtype VoteUrl = VoteUrl {unVoteUrl :: Text}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet VoteUrl)

voteUrlDecoder :: D.Value VoteUrl
voteUrlDecoder = VoteUrl <$> D.text

voteUrlEncoder :: E.Value VoteUrl
voteUrlEncoder = unVoteUrl >$< E.text

--------------------------------------------------------------------------------
-- | The raw binary hash of a vote metadata.
newtype VoteMetaHash = VoteMetaHash {unVoteMetaHash :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet VoteMetaHash)

--------------------------------------------------------------------------------
data Vote = VoteYes | VoteNo | VoteAbstain
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet Vote)

voteDecoder :: D.Value Vote
voteDecoder = D.enum $ \case
  "yes" -> Just VoteYes
  "no" -> Just VoteNo
  "abstain" -> Just VoteAbstain
  _ -> Nothing

voteEncoder :: E.Value Vote
voteEncoder = E.enum $ \case
  VoteYes -> "yes"
  VoteNo -> "no"
  VoteAbstain -> "abstain"

--------------------------------------------------------------------------------
data VoterRole = ConstitutionalCommittee | DRep | SPO
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet VoterRole)

voterRoleDecoder :: D.Value VoterRole
voterRoleDecoder = D.enum $ \case
  "constitutional-committee" -> Just ConstitutionalCommittee
  "drep" -> Just DRep
  "spo" -> Just SPO
  _ -> Nothing

voterRoleEncoder :: E.Value VoterRole
voterRoleEncoder = E.enum $ \case
  ConstitutionalCommittee -> "constitutional-committee"
  DRep -> "drep"
  SPO -> "spo"

--------------------------------------------------------------------------------
-- | The type of governance action.
data GovActionType
  = ParameterChange
  | HardForkInitiation
  | TreasuryWithdrawals
  | NoConfidence
  | NewCommitteeType
  | NewConstitution
  | InfoAction
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet GovActionType)

govActionTypeDecoder :: D.Value GovActionType
govActionTypeDecoder = D.enum $ \case
  "parameter-change" -> Just ParameterChange
  "hard-fork-initiation" -> Just HardForkInitiation
  "treasury-withdrawals" -> Just TreasuryWithdrawals
  "no-confidence" -> Just NoConfidence
  "new-committee" -> Just NewCommitteeType
  "new-constitution" -> Just NewConstitution
  "info-action" -> Just InfoAction
  _ -> Nothing

govActionTypeEncoder :: E.Value GovActionType
govActionTypeEncoder = E.enum $ \case
  ParameterChange -> "parameter-change"
  HardForkInitiation -> "hard-fork-initiation"
  TreasuryWithdrawals -> "treasury-withdrawals"
  NoConfidence -> "no-confidence"
  NewCommitteeType -> "new-committee"
  NewConstitution -> "new-constitution"
  InfoAction -> "info-action"

--------------------------------------------------------------------------------
-- | The type of anchor.
data AnchorType
  = GovActionAnchor
  | DrepAnchor
  | OtherAnchor
  | VoteAnchor
  | CommitteeDeRegAnchor
  | ConstitutionAnchor
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet AnchorType)

anchorTypeDecoder :: D.Value AnchorType
anchorTypeDecoder = D.enum $ \case
  "gov-action" -> Just GovActionAnchor
  "drep" -> Just DrepAnchor
  "other" -> Just OtherAnchor
  "vote" -> Just VoteAnchor
  "committee-dereg" -> Just CommitteeDeRegAnchor
  "constitution" -> Just ConstitutionAnchor
  _ -> Nothing

anchorTypeEncoder :: E.Value AnchorType
anchorTypeEncoder = E.enum $ \case
  GovActionAnchor -> "gov-action"
  DrepAnchor -> "drep"
  OtherAnchor -> "other"
  VoteAnchor -> "vote"
  CommitteeDeRegAnchor -> "committee-dereg"
  ConstitutionAnchor -> "constitution"

deltaCoinToDbInt65 :: DeltaCoin -> DbInt65
deltaCoinToDbInt65 (DeltaCoin dc) =
  toDbInt65 (fromIntegral dc)

integerToDbInt65 :: Integer -> DbInt65
integerToDbInt65 i
  | i > fromIntegral (maxBound :: Int64) = error "Integer too large for DbInt65"
  | i < fromIntegral (minBound :: Int64) = error "Integer too small for DbInt65"
  | otherwise = toDbInt65 (fromIntegral i)

-- deltaCoinToDbInt65 :: DeltaCoin -> DbInt65
-- deltaCoinToDbInt65 (DeltaCoin dc) =
--   if dc < 0
--     then NegInt65 (fromIntegral $ abs dc)
--     else PosInt65 (fromIntegral dc)

-- integerToDbInt65 :: Integer -> DbInt65
-- integerToDbInt65 i =
--   if i >= 0
--     then PosInt65 (fromIntegral i)
--     else NegInt65 (fromIntegral $ negate i)

word128Decoder :: D.Value Word128
word128Decoder = D.composite $ do
  hi <- D.field (D.nonNullable $ fromIntegral <$> D.int8)
  lo <- D.field (D.nonNullable $ fromIntegral <$> D.int8)
  pure $ Word128 hi lo

word128Encoder :: E.Value Word128
word128Encoder = E.composite $
  E.field (E.nonNullable $ fromIntegral . word128Hi64 >$< E.int8) <>
  E.field (E.nonNullable $ fromIntegral . word128Lo64 >$< E.int8)

lovelaceToAda :: Micro -> Ada
lovelaceToAda ll =
  Ada (ll / 1000000)

renderAda :: Ada -> Text
renderAda (Ada a) = Text.pack (show a)

scientificToAda :: Scientific -> Ada
scientificToAda s =
  word64ToAda $ floor (s * 1000000)

rewardSourceFromText :: Text -> RewardSource
rewardSourceFromText txt =
  case txt of
    "member" -> RwdMember
    "leader" -> RwdLeader
    "reserves" -> RwdReserves
    "treasury" -> RwdTreasury
    "refund" -> RwdDepositRefund
    "proposal_refund" -> RwdProposalRefund
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "rewardSourceFromText: Unknown RewardSource " ++ show txt

syncStateFromText :: Text -> SyncState
syncStateFromText txt =
  case txt of
    "lagging" -> SyncLagging
    "following" -> SyncFollowing
    -- This should never happen. On the Postgres side we defined an ENUM with
    -- only the two values as above.
    _other -> error $ "syncStateToText: Unknown SyncState " ++ show txt

syncStateToText :: SyncState -> Text
syncStateToText ss =
  case ss of
    SyncFollowing -> "following"
    SyncLagging -> "lagging"

scriptPurposeFromText :: ScriptPurpose -> Text
scriptPurposeFromText ss =
  case ss of
    Spend -> "spend"
    Mint -> "mint"
    Cert -> "cert"
    Rewrd -> "reward"
    Vote -> "vote"
    Propose -> "propose"

scriptPurposeToText :: Text -> ScriptPurpose
scriptPurposeToText txt =
  case txt of
    "spend" -> Spend
    "mint" -> Mint
    "cert" -> Cert
    "reward" -> Rewrd
    "vote" -> Vote
    "propose" -> Propose
    _other -> error $ "scriptPurposeFromText: Unknown ScriptPurpose " ++ show txt

rewardSourceToText :: RewardSource -> Text
rewardSourceToText rs =
  case rs of
    RwdMember -> "member"
    RwdLeader -> "leader"
    RwdReserves -> "reserves"
    RwdTreasury -> "treasury"
    RwdDepositRefund -> "refund"
    RwdProposalRefund -> "proposal_refund"

scriptTypeToText :: ScriptType -> Text
scriptTypeToText st =
  case st of
    MultiSig -> "multisig"
    Timelock -> "timelock"
    PlutusV1 -> "plutusV1"
    PlutusV2 -> "plutusV2"
    PlutusV3 -> "plutusV3"

scriptTypeFromText :: Text -> ScriptType
scriptTypeFromText txt =
  case txt of
    "multisig" -> MultiSig
    "timelock" -> Timelock
    "plutusV1" -> PlutusV1
    "plutusV2" -> PlutusV2
    "plutusV3" -> PlutusV3
    _other -> error $ "scriptTypeFromText: Unknown ScriptType " ++ show txt

voteToText :: Vote -> Text
voteToText ss =
  case ss of
    VoteYes -> "Yes"
    VoteNo -> "No"
    VoteAbstain -> "Abstain"

voteFromText :: Text -> Vote
voteFromText txt =
  case txt of
    "Yes" -> VoteYes
    "No" -> VoteNo
    "Abstain" -> VoteAbstain
    _other -> error $ "readVote: Unknown Vote " ++ show txt

voterRoleToText :: VoterRole -> Text
voterRoleToText ss =
  case ss of
    ConstitutionalCommittee -> "ConstitutionalCommittee"
    DRep -> "DRep"
    SPO -> "SPO"

voterRoleFromText :: Text -> VoterRole
voterRoleFromText txt =
  case txt of
    "ConstitutionalCommittee" -> ConstitutionalCommittee
    "DRep" -> DRep
    "SPO" -> SPO
    _other -> error $ "voterRoleFromText: Unknown VoterRole " ++ show txt

govActionTypeToText :: GovActionType -> Text
govActionTypeToText gav =
  case gav of
    ParameterChange -> "ParameterChange"
    HardForkInitiation -> "HardForkInitiation"
    TreasuryWithdrawals -> "TreasuryWithdrawals"
    NoConfidence -> "NoConfidence"
    NewCommitteeType -> "NewCommittee"
    NewConstitution -> "NewConstitution"
    InfoAction -> "InfoAction"

govActionTypeFromText :: Text -> GovActionType
govActionTypeFromText txt =
  case txt of
    "ParameterChange" -> ParameterChange
    "HardForkInitiation" -> HardForkInitiation
    "TreasuryWithdrawals" -> TreasuryWithdrawals
    "NoConfidence" -> NoConfidence
    "NewCommittee" -> NewCommitteeType
    "NewConstitution" -> NewConstitution
    _other -> error $ "govActionTypeFromText: Unknown GovActionType " ++ show txt

anchorTypeToText :: AnchorType -> Text
anchorTypeToText gav =
  case gav of
    GovActionAnchor -> "gov_action"
    DrepAnchor -> "drep"
    OtherAnchor -> "other"
    VoteAnchor -> "vote"
    CommitteeDeRegAnchor -> "committee_dereg"
    ConstitutionAnchor -> "constitution"

anchorTypeFromText :: Text -> AnchorType
anchorTypeFromText txt =
  case txt of
    "gov_action" -> GovActionAnchor
    "drep" -> DrepAnchor
    "other" -> OtherAnchor
    "vote" -> VoteAnchor
    "committee_dereg" -> CommitteeDeRegAnchor
    "constitution" -> ConstitutionAnchor
    _other -> error $ "anchorTypeFromText: Unknown AnchorType " ++ show txt

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)

hardcodedAlwaysAbstain :: Text
hardcodedAlwaysAbstain = "drep_always_abstain"

hardcodedAlwaysNoConfidence :: Text
hardcodedAlwaysNoConfidence = "drep_always_no_confidence"
