{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Ledger.Types where

import Cardano.BM.Trace (Trace)
import Cardano.Binary (Decoder, Encoding, FromCBOR (..), ToCBOR (..))
import Cardano.DbSync.Config.Types (LedgerStateDir)
import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import Cardano.DbSync.Ledger.Event (LedgerEvent)
import Cardano.DbSync.Types (
  CardanoBlock,
  CardanoInterpreter,
  CardanoPoint,
  PoolKeyHash,
  SlotDetails,
 )
import Cardano.Ledger.Alonzo.Scripts (Prices)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (NewEpochState ())
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (
  EpochNo (..),
  SlotNo (..),
  WithOrigin (..),
 )
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar,
 )
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Data.Map.Strict as Map
import Data.SOP.Strict
import qualified Data.Set as Set
import qualified Data.Strict.Maybe as Strict
import Lens.Micro (Traversal')
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock, CardanoLedgerState)
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.Ledger.Abstract (getTipSlot)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (..), ShelleyBlock)
import Ouroboros.Network.AnchoredSeq (Anchorable (..), AnchoredSeq (..))
import Prelude (fail, id)

--------------------------------------------------------------------------
-- Ledger Types
--------------------------------------------------------------------------

data HasLedgerEnv = HasLedgerEnv
  { leTrace :: Trace IO Text
  , leUseLedger :: !Bool
  , leHasRewards :: !Bool
  , leProtocolInfo :: !(Consensus.ProtocolInfo CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leSystemStart :: !SystemStart
  , leAbortOnPanic :: !Bool
  , leSnapshotEveryFollowing :: !Word64
  , leSnapshotEveryLagging :: !Word64
  , leInterpreter :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar :: !(StrictTVar IO (Strict.Maybe LedgerDB))
  , leStateWriteQueue :: !(TBQueue (FilePath, CardanoLedgerState))
  }

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(ExtLedgerState CardanoBlock)
  , clsEpochBlockNo :: !EpochBlockNo
  }

-- The height of the block in the current Epoch. We maintain this
-- data next to the ledger state and store it in the same blob file.
data EpochBlockNo
  = GenesisEpochBlockNo
  | EBBEpochBlockNo
  | EpochBlockNo !Word64

instance ToCBOR EpochBlockNo where
  toCBOR GenesisEpochBlockNo = toCBOR (0 :: Word8)
  toCBOR EBBEpochBlockNo = toCBOR (1 :: Word8)
  toCBOR (EpochBlockNo n) =
    toCBOR (2 :: Word8) <> toCBOR n

instance FromCBOR EpochBlockNo where
  fromCBOR = do
    tag :: Word8 <- fromCBOR
    case tag of
      0 -> pure GenesisEpochBlockNo
      1 -> pure EBBEpochBlockNo
      2 -> EpochBlockNo <$> fromCBOR
      n -> fail $ "unexpected EpochBlockNo value " <> show n

encodeCardanoLedgerState :: (ExtLedgerState CardanoBlock -> Encoding) -> CardanoLedgerState -> Encoding
encodeCardanoLedgerState encodeExt cls =
  mconcat
    [ encodeExt (clsState cls)
    , toCBOR (clsEpochBlockNo cls)
    ]

decodeCardanoLedgerState ::
  (forall s. Decoder s (ExtLedgerState CardanoBlock)) ->
  (forall s. Decoder s CardanoLedgerState)
decodeCardanoLedgerState decodeExt = do
  ldgrState <- decodeExt
  CardanoLedgerState ldgrState <$> fromCBOR

data LedgerStateFile = LedgerStateFile
  { lsfSlotNo :: !SlotNo
  , lsfHash :: !ByteString
  , lsNewEpoch :: !(Strict.Maybe EpochNo)
  , lsfFilePath :: !FilePath
  }
  deriving (Show)

newtype DepositsMap = DepositsMap {unDepositsMap :: Map ByteString Coin}

lookupDepositsMap :: ByteString -> DepositsMap -> Maybe Coin
lookupDepositsMap bs = Map.lookup bs . unDepositsMap

emptyDepositsMap :: DepositsMap
emptyDepositsMap = DepositsMap Map.empty

-- The result of applying a new block. This includes all the data that insertions require.
data ApplyResult = ApplyResult
  { apPrices :: !(Strict.Maybe Prices) -- prices after the block application
  , apGovExpiresAfter :: !(Strict.Maybe Ledger.EpochInterval)
  , apPoolsRegistered :: !(Set.Set PoolKeyHash) -- registered before the block application
  , apNewEpoch :: !(Strict.Maybe Generic.NewEpoch) -- Only Just for a single block at the epoch boundary
  , apOldLedger :: !(Strict.Maybe CardanoLedgerState)
  , apDeposits :: !(Strict.Maybe Generic.Deposits) -- The current required deposits
  , apSlotDetails :: !SlotDetails
  , apStakeSlice :: !Generic.StakeSliceRes
  , apEvents :: ![LedgerEvent]
  , apGovActionState :: !(Maybe (ConwayGovState StandardConway))
  , apDepositsMap :: !DepositsMap
  }

defaultApplyResult :: SlotDetails -> ApplyResult
defaultApplyResult slotDetails =
  ApplyResult
    { apPrices = Strict.Nothing
    , apGovExpiresAfter = Strict.Nothing
    , apPoolsRegistered = Set.empty
    , apNewEpoch = Strict.Nothing
    , apOldLedger = Strict.Nothing
    , apDeposits = Strict.Nothing
    , apSlotDetails = slotDetails
    , apStakeSlice = Generic.NoSlices
    , apEvents = []
    , apGovActionState = Nothing
    , apDepositsMap = emptyDepositsMap
    }

getGovExpiresAt :: ApplyResult -> EpochNo -> Maybe EpochNo
getGovExpiresAt applyResult e = case apGovExpiresAfter applyResult of
  Strict.Just ei -> Just $ Ledger.addEpochInterval e ei
  Strict.Nothing -> Nothing

-- TODO reuse this function rom ledger after it's exported.
updatedCommittee ::
  Set.Set (Credential 'ColdCommitteeRole StandardCrypto) ->
  Map.Map (Credential 'ColdCommitteeRole StandardCrypto) EpochNo ->
  Ledger.UnitInterval ->
  Ledger.StrictMaybe (Committee StandardConway) ->
  Committee StandardConway
updatedCommittee membersToRemove membersToAdd newQuorum committee =
  case committee of
    Ledger.SNothing -> Committee membersToAdd newQuorum
    Ledger.SJust (Committee currentMembers _) ->
      let newCommitteeMembers =
            Map.union
              membersToAdd
              (currentMembers `Map.withoutKeys` membersToRemove)
       in Committee
            newCommitteeMembers
            newQuorum

newtype LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: AnchoredSeq (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState
  }

instance Anchorable (WithOrigin SlotNo) CardanoLedgerState CardanoLedgerState where
  asAnchor = id
  getAnchorMeasure _ = getTipSlot . clsState

data SnapshotPoint = OnDisk LedgerStateFile | InMemory CardanoPoint

-- | Per-era pure getters and setters on @NewEpochState@. Note this is a bit of an abuse
-- of the cardano-ledger/ouroboros-consensus public APIs, because ledger state is not
-- designed to be updated this way. We are only replaying the chain, so this should be
-- safe.
class HasNewEpochState era where
  getNewEpochState :: ExtLedgerState CardanoBlock -> Maybe (NewEpochState era)

  applyNewEpochState ::
    NewEpochState era ->
    ExtLedgerState CardanoBlock ->
    ExtLedgerState CardanoBlock

instance HasNewEpochState StandardShelley where
  getNewEpochState st = case ledgerState st of
    LedgerStateShelley shelley -> Just (shelleyLedgerState shelley)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn (applyNewEpochState' st) :* fn id :* fn id :* fn id :* fn id :* fn id :* Nil

instance HasNewEpochState StandardAllegra where
  getNewEpochState st = case ledgerState st of
    LedgerStateAllegra allegra -> Just (shelleyLedgerState allegra)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id :* fn (applyNewEpochState' st) :* fn id :* fn id :* fn id :* fn id :* Nil

instance HasNewEpochState StandardMary where
  getNewEpochState st = case ledgerState st of
    LedgerStateMary mary -> Just (shelleyLedgerState mary)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id :* fn id :* fn (applyNewEpochState' st) :* fn id :* fn id :* fn id :* Nil

instance HasNewEpochState StandardAlonzo where
  getNewEpochState st = case ledgerState st of
    LedgerStateAlonzo alonzo -> Just (shelleyLedgerState alonzo)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id :* fn id :* fn id :* fn (applyNewEpochState' st) :* fn id :* fn id :* Nil

instance HasNewEpochState StandardBabbage where
  getNewEpochState st = case ledgerState st of
    LedgerStateBabbage babbage -> Just (shelleyLedgerState babbage)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id :* fn id :* fn id :* fn id :* fn (applyNewEpochState' st) :* fn id :* Nil

instance HasNewEpochState StandardConway where
  getNewEpochState st = case ledgerState st of
    LedgerStateConway conway -> Just (shelleyLedgerState conway)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id :* fn id :* fn id :* fn id :* fn id :* fn (applyNewEpochState' st) :* Nil

hApplyExtLedgerState ::
  NP (LedgerState -.-> LedgerState) (CardanoShelleyEras StandardCrypto) ->
  ExtLedgerState CardanoBlock ->
  ExtLedgerState CardanoBlock
hApplyExtLedgerState f ledger =
  case ledgerState ledger of
    HardForkLedgerState hfState ->
      let newHfState = hap (fn id :* f) hfState
       in updateLedgerState $ HardForkLedgerState newHfState
  where
    updateLedgerState st = ledger {ledgerState = st}

applyNewEpochState' ::
  NewEpochState era ->
  LedgerState (ShelleyBlock proto era) ->
  LedgerState (ShelleyBlock proto era)
applyNewEpochState' newEpochState' ledger =
  ledger {shelleyLedgerState = newEpochState'}

-- | A @Traversal@ that targets the @NewEpochState@ from the extended ledger state
newEpochStateT ::
  HasNewEpochState era =>
  Traversal' (ExtLedgerState CardanoBlock) (NewEpochState era)
newEpochStateT f ledger =
  case getNewEpochState ledger of
    Just newEpochState' -> flip applyNewEpochState ledger <$> f newEpochState'
    Nothing -> pure ledger
