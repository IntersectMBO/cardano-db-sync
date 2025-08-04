{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Types where

import Cardano.BM.Trace (Trace)
import Cardano.Ledger.Coin (DeltaCoin (..))
import Cardano.Prelude (Bifunctor (..), MonadIO (..), MonadReader, ReaderT, fromMaybe)
import qualified Codec.Binary.Bech32 as Bech32
import Crypto.Hash (Blake2b_160)
import qualified Crypto.Hash
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import Data.Bits (Bits (..))
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import Data.Either (fromRight)
import Data.Fixed (Micro, showFixed)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Scientific (Scientific (..), scientific, toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.WideWord (Word128 (..))
import Data.Word (Word16, Word64)
import GHC.Generics
import qualified Hasql.Connection as HsqlCon
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import Quiet (Quiet (..))

----------------------------------------------------------------------------
-- Connection Type
----------------------------------------------------------------------------

-- | Specifies which type of database connection to use for operations
data ConnectionType
  = -- | Use the persistent main connection (for sequential operations, transactions)
    UseConnection
  | -- | Use a connection from the pool (for parallel/async operations)
    UsePoolConnection
  deriving (Show, Eq)

----------------------------------------------------------------------------
-- DbM
----------------------------------------------------------------------------

-- | Database session monad that wraps Hasql Session
-- Functions can return Either DbError a to handle validation errors
newtype DbM a = DbM
  {runDbM :: ReaderT DbEnv IO a}
  deriving (Functor, Applicative, Monad, MonadReader DbEnv, MonadIO)

----------------------------------------------------------------------------
-- DbEnv
----------------------------------------------------------------------------
data DbEnv = DbEnv
  { dbConnection :: !HsqlCon.Connection
  , dbPoolConnection :: !(Maybe (Pool HsqlCon.Connection)) -- not all operations use a pool connection
  , dbTracer :: !(Maybe (Trace IO Text))
  }

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

-- | Convert a `Scientific` to `Ada`.
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

newtype DbInt65 = DbInt65 {unDbInt65 :: Word64}
  deriving (Eq, Generic)

instance Show DbInt65 where
  show = show . fromDbInt65

instance Read DbInt65 where
  readsPrec d = map (first toDbInt65) . readsPrec d

dbInt65Decoder :: HsqlD.Value DbInt65
dbInt65Decoder = toDbInt65 <$> HsqlD.int8

dbInt65Encoder :: HsqlE.Value DbInt65
dbInt65Encoder = fromDbInt65 >$< HsqlE.int8

-- Helper functions to pack/unpack the sign and value
toDbInt65 :: Int64 -> DbInt65
toDbInt65 n
  | n >= 0 = DbInt65 (fromIntegral n)
  | n == minBound = DbInt65 (setBit 0 63) -- Special: magnitude 0 + sign bit = minBound
  | otherwise = DbInt65 (setBit (fromIntegral (abs n)) 63)

fromDbInt65 :: DbInt65 -> Int64
fromDbInt65 (DbInt65 w) =
  if testBit w 63
    then
      let magnitude = clearBit w 63
       in if magnitude == 0
            then minBound -- Special: magnitude 0 + sign bit = minBound
            else negate (fromIntegral magnitude)
    else fromIntegral w

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbLovelace = DbLovelace {unDbLovelace :: Word64}
  deriving (Eq, Generic, Ord)
  deriving (Read, Show) via (Quiet DbLovelace)

dbLovelaceEncoder :: HsqlE.Params DbLovelace
dbLovelaceEncoder = HsqlE.param $ HsqlE.nonNullable $ (\x -> scientific (toInteger $ unDbLovelace x) 0) >$< HsqlE.numeric

dbLovelaceBulkEncoder :: HsqlE.NullableOrNot HsqlE.Value DbLovelace
dbLovelaceBulkEncoder = HsqlE.nonNullable $ (\x -> scientific (toInteger $ unDbLovelace x) 0) >$< HsqlE.numeric

dbLovelaceValueEncoder :: HsqlE.NullableOrNot HsqlE.Value DbLovelace
dbLovelaceValueEncoder = HsqlE.nonNullable $ (\x -> scientific (toInteger $ unDbLovelace x) 0) >$< HsqlE.numeric

maybeDbLovelaceEncoder :: HsqlE.Params (Maybe DbLovelace)
maybeDbLovelaceEncoder = HsqlE.param $ HsqlE.nullable $ (\x -> scientific (toInteger $ unDbLovelace x) 0) >$< HsqlE.numeric

dbLovelaceDecoder :: HsqlD.Row DbLovelace
dbLovelaceDecoder = HsqlD.column (HsqlD.nonNullable (DbLovelace . fromMaybe 0 . toBoundedInteger <$> HsqlD.numeric))

maybeDbLovelaceDecoder :: HsqlD.Row (Maybe DbLovelace)
maybeDbLovelaceDecoder = HsqlD.column (HsqlD.nullable (DbLovelace . fromMaybe 0 . toBoundedInteger <$> HsqlD.numeric))

-- Newtype wrapper around Word64 so we can hand define a PersistentField instance.
newtype DbWord64 = DbWord64 {unDbWord64 :: Word64}
  deriving (Eq, Generic, Num)
  deriving (Read, Show) via (Quiet DbWord64)

maybeDbWord64Encoder :: HsqlE.Params (Maybe DbWord64)
maybeDbWord64Encoder = HsqlE.param $ HsqlE.nullable $ fromIntegral . unDbWord64 >$< HsqlE.int8

maybeDbWord64Decoder :: HsqlD.Row (Maybe DbWord64)
maybeDbWord64Decoder = HsqlD.column (HsqlD.nullable (DbWord64 . fromIntegral <$> HsqlD.int8))

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

rewardSourceDecoder :: HsqlD.Value RewardSource
rewardSourceDecoder = HsqlD.enum $ \case
  "leader" -> Just RwdLeader
  "member" -> Just RwdMember
  "reserves" -> Just RwdReserves
  "treasury" -> Just RwdTreasury
  "refund" -> Just RwdDepositRefund
  "proposal_refund" -> Just RwdProposalRefund
  _ -> Nothing

rewardSourceEncoder :: HsqlE.Value RewardSource
rewardSourceEncoder = HsqlE.enum $ \case
  RwdLeader -> "leader"
  RwdMember -> "member"
  RwdReserves -> "reserves"
  RwdTreasury -> "treasury"
  RwdDepositRefund -> "refund"
  RwdProposalRefund -> "proposal_refund"

--------------------------------------------------------------------------------
data SyncState
  = SyncLagging -- Local tip is lagging the global chain tip.
  | SyncFollowing -- Local tip is following global chain tip.
  deriving (Eq, Show)

syncStateDecoder :: HsqlD.Value SyncState
syncStateDecoder = HsqlD.enum $ \case
  "lagging" -> Just SyncLagging
  "following" -> Just SyncFollowing
  _ -> Nothing

syncStateEncoder :: HsqlE.Value SyncState
syncStateEncoder = HsqlE.enum $ \case
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

scriptPurposeDecoder :: HsqlD.Value ScriptPurpose
scriptPurposeDecoder = HsqlD.enum $ \case
  "spend" -> Just Spend
  "mint" -> Just Mint
  "cert" -> Just Cert
  "reward" -> Just Rewrd
  "vote" -> Just Vote
  "propose" -> Just Propose
  _ -> Nothing

scriptPurposeEncoder :: HsqlE.Value ScriptPurpose
scriptPurposeEncoder = HsqlE.enum $ \case
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

scriptTypeDecoder :: HsqlD.Value ScriptType
scriptTypeDecoder = HsqlD.enum $ \case
  "multisig" -> Just MultiSig
  "timelock" -> Just Timelock
  "plutusV1" -> Just PlutusV1
  "plutusV2" -> Just PlutusV2
  "plutusV3" -> Just PlutusV3
  _ -> Nothing

scriptTypeEncoder :: HsqlE.Value ScriptType
scriptTypeEncoder = HsqlE.enum $ \case
  MultiSig -> "multisig"
  Timelock -> "timelock"
  PlutusV1 -> "plutusV1"
  PlutusV2 -> "plutusV2"
  PlutusV3 -> "plutusV3"

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

voteUrlDecoder :: HsqlD.Value VoteUrl
voteUrlDecoder = VoteUrl <$> HsqlD.text

voteUrlEncoder :: HsqlE.Value VoteUrl
voteUrlEncoder = unVoteUrl >$< HsqlE.text

--------------------------------------------------------------------------------

-- | The raw binary hash of a vote metadata.
newtype VoteMetaHash = VoteMetaHash {unVoteMetaHash :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet VoteMetaHash)

--------------------------------------------------------------------------------
data Vote = VoteYes | VoteNo | VoteAbstain
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet Vote)

voteDecoder :: HsqlD.Value Vote
voteDecoder = HsqlD.enum $ \case
  "Yes" -> Just VoteYes
  "No" -> Just VoteNo
  "Abstain" -> Just VoteAbstain
  _ -> Nothing

voteEncoder :: HsqlE.Value Vote
voteEncoder = HsqlE.enum $ \case
  VoteYes -> "Yes"
  VoteNo -> "No"
  VoteAbstain -> "Abstain"

--------------------------------------------------------------------------------
data VoterRole = ConstitutionalCommittee | DRep | SPO
  deriving (Eq, Ord, Generic)
  deriving (Show) via (Quiet VoterRole)

voterRoleDecoder :: HsqlD.Value VoterRole
voterRoleDecoder = HsqlD.enum $ \case
  "ConstitutionalCommittee" -> Just ConstitutionalCommittee
  "DRep" -> Just DRep
  "SPO" -> Just SPO
  _ -> Nothing

voterRoleEncoder :: HsqlE.Value VoterRole
voterRoleEncoder = HsqlE.enum $ \case
  ConstitutionalCommittee -> "ConstitutionalCommittee"
  DRep -> "DRep"
  SPO -> "SPO"

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

govActionTypeDecoder :: HsqlD.Value GovActionType
govActionTypeDecoder = HsqlD.enum $ \case
  "ParameterChange" -> Just ParameterChange
  "HardForkInitiation" -> Just HardForkInitiation
  "TreasuryWithdrawals" -> Just TreasuryWithdrawals
  "NoConfidence" -> Just NoConfidence
  "NewCommittee" -> Just NewCommitteeType
  "NewConstitution" -> Just NewConstitution
  "InfoAction" -> Just InfoAction
  _ -> Nothing

govActionTypeEncoder :: HsqlE.Value GovActionType
govActionTypeEncoder = HsqlE.enum $ \case
  ParameterChange -> "ParameterChange"
  HardForkInitiation -> "HardForkInitiation"
  TreasuryWithdrawals -> "TreasuryWithdrawals"
  NoConfidence -> "NoConfidence"
  NewCommitteeType -> "NewCommittee"
  NewConstitution -> "NewConstitution"
  InfoAction -> "InfoAction"

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

anchorTypeDecoder :: HsqlD.Value AnchorType
anchorTypeDecoder = HsqlD.enum $ \case
  "gov_action" -> Just GovActionAnchor
  "drep" -> Just DrepAnchor
  "other" -> Just OtherAnchor
  "vote" -> Just VoteAnchor
  "committee_dereg" -> Just CommitteeDeRegAnchor
  "constitution" -> Just ConstitutionAnchor
  _ -> Nothing

anchorTypeEncoder :: HsqlE.Value AnchorType
anchorTypeEncoder = HsqlE.enum $ \case
  GovActionAnchor -> "gov_action"
  DrepAnchor -> "drep"
  OtherAnchor -> "other"
  VoteAnchor -> "vote"
  CommitteeDeRegAnchor -> "committee_dereg"
  ConstitutionAnchor -> "constitution"

deltaCoinToDbInt65 :: DeltaCoin -> DbInt65
deltaCoinToDbInt65 (DeltaCoin dc) =
  toDbInt65 (fromIntegral dc)

integerToDbInt65 :: Integer -> DbInt65
integerToDbInt65 i
  | i > fromIntegral (maxBound :: Int64) = error "Integer too large for DbInt65"
  | i < fromIntegral (minBound :: Int64) = error "Integer too small for DbInt65"
  | otherwise = toDbInt65 (fromIntegral i)

word128Encoder :: HsqlE.Value Word128
word128Encoder = fromInteger . toInteger >$< HsqlE.numeric

word128Decoder :: HsqlD.Value Word128
word128Decoder = fromInteger . fromIntegral . coefficient <$> HsqlD.numeric

lovelaceToAda :: Micro -> Ada
lovelaceToAda ll =
  Ada (ll / 1000000)

renderAda :: Ada -> Text
renderAda (Ada a) = Text.pack (show a)

scientificToAda :: Scientific -> Ada
scientificToAda s =
  word64ToAda $ floor (s * 1000000)

word64ToAda :: Word64 -> Ada
word64ToAda w =
  Ada (fromIntegral w / 1000000)

hardcodedAlwaysAbstain :: Text
hardcodedAlwaysAbstain = "drep_always_abstain"

hardcodedAlwaysNoConfidence :: Text
hardcodedAlwaysNoConfidence = "drep_always_no_confidence"
