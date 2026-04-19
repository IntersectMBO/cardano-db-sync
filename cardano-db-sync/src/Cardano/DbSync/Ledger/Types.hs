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
import Cardano.DbSync.Config.Types (LedgerBackend (..), LedgerStateDir)
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
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import Cardano.Ledger.Shelley.LedgerState (NewEpochState ())
import Cardano.Prelude hiding (atomically)
import Cardano.Slotting.Slot (
  EpochNo (..),
 )
import Control.Concurrent.Class.MonadSTM.Strict (
  StrictTVar, newTVarIO,
 )
import Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Data.Map.Strict as Map
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict
import qualified Data.Set as Set
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Strict.Maybe as Strict
import Lens.Micro (Traversal', (^.))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import Ouroboros.Consensus.Cardano.Block hiding (CardanoBlock, CardanoLedgerState)
import Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState (..))
import Ouroboros.Consensus.Ledger.Abstract ()
import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot, SnapshotManager)
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq (LedgerTablesHandle (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq as Consensus (StateRef (..))
import qualified Ouroboros.Consensus.Node.ProtocolInfo as Consensus
import Ouroboros.Consensus.Shelley.Ledger (LedgerState (..), ShelleyBlock)
import Data.List.NonEmpty ()
import Prelude (id)

--------------------------------------------------------------------------
-- Ledger Types
--------------------------------------------------------------------------

-- | Consensus StateRef type used for snapshot operations.
type ConsensusStateRef = Consensus.StateRef IO (ExtLedgerState CardanoBlock)

data HasLedgerEnv = HasLedgerEnv
  { leTrace :: !(Trace IO Text)
  , leUseLedger :: !Bool
  , leHasRewards :: !Bool
  , leProtocolInfo :: !(Consensus.ProtocolInfo CardanoBlock)
  , leDir :: !LedgerStateDir
  , leNetwork :: !Ledger.Network
  , leMaxSupply :: !Word64
  , leSystemStart :: !SystemStart
  , leAbortOnPanic :: !Bool
  , leSnapshotNearTipEpoch :: !Word64
  , leInterpreter :: !(StrictTVar IO (Strict.Maybe CardanoInterpreter))
  , leStateVar :: !(StrictTVar IO (Strict.Maybe LedgerDB))
  , leSnapshotQueue :: !(TBQueue DbSyncStateRef)
  -- ^ Queue for async snapshot writing
  , leLedgerBackend :: !LedgerBackend
  , leSnapshotManager :: !(SnapshotManager IO IO CardanoBlock ConsensusStateRef)
  -- ^ Consensus snapshot manager for save/load/list/cleanup
  , leInitGenesis :: !(IO ConsensusStateRef)
  -- ^ Create the initial consensus StateRef from genesis
  , leLoadSnapshot :: !(DiskSnapshot -> IO (Either Text ConsensusStateRef))
  -- ^ Load a snapshot from disk using the appropriate backend
  }

-- | Pure ledger state, stored in LedgerDB checkpoints.
-- Does not hold handle or close-related resources.
-- | Block number within the current epoch. Used by getStakeSlice
-- to insert epoch stake incrementally.
data EpochBlockNo
  = EpochBlockNo !Word64  -- ^ Shelley+: block number within epoch
  | ByronEpochBlockNo     -- ^ Byron: no stake slicing needed

data CardanoLedgerState = CardanoLedgerState
  { clsState :: !(ExtLedgerState CardanoBlock EmptyMK)
  , clsEpochBlockNo :: !EpochBlockNo
  }

-- | Full state with handle, used during block application and snapshotting.
data DbSyncStateRef = DbSyncStateRef
  { srState :: !CardanoLedgerState
  , srTables :: !(LedgerTablesHandle IO (ExtLedgerState CardanoBlock))
  , srCanClose :: !(StrictTVar IO Bool)
  }

-- | Derive EpochBlockNo from the ledger state.
-- For Shelley+, sums BlocksMade (nesBcur) to get the block count in the current epoch.
-- For Byron, returns ByronEpochBlockNo.
deriveEpochBlockNo :: ExtLedgerState CardanoBlock mk -> EpochBlockNo
deriveEpochBlockNo st =
  case ledgerState st of
    LedgerStateByron _ -> ByronEpochBlockNo
    LedgerStateShelley sls -> countBlocks sls
    LedgerStateAllegra als -> countBlocks als
    LedgerStateMary mls -> countBlocks mls
    LedgerStateAlonzo als -> countBlocks als
    LedgerStateBabbage bls -> countBlocks bls
    LedgerStateConway cls -> countBlocks cls
    LedgerStateDijkstra dls -> countBlocks dls
  where
    countBlocks :: LedgerState (ShelleyBlock p era) mk -> EpochBlockNo
    countBlocks lstate =
      let nes = shelleyLedgerState lstate
          bm = nes ^. Shelley.nesBcurL
      in EpochBlockNo $ fromIntegral $ sum bm

-- | Convert a db-sync StateRef to a consensus StateRef for snapshot operations.
toConsensusStateRef :: DbSyncStateRef -> ConsensusStateRef
toConsensusStateRef sr = Consensus.StateRef (clsState $ srState sr) (srTables sr)

-- | Convert a consensus StateRef to a db-sync DbSyncStateRef.
fromConsensusStateRef :: EpochBlockNo -> ConsensusStateRef -> IO DbSyncStateRef
fromConsensusStateRef ebn (Consensus.StateRef st tbl) = do
  canClose <- newTVarIO True
  pure
    DbSyncStateRef
      { srState = CardanoLedgerState
          { clsState = st
          , clsEpochBlockNo = ebn
          }
      , srTables = tbl
      , srCanClose = canClose
      }

-- | Create initial db-sync state from genesis using the consensus API.
initCardanoLedgerState :: HasLedgerEnv -> IO DbSyncStateRef
initCardanoLedgerState env = do
  consensusRef <- leInitGenesis env
  fromConsensusStateRef ByronEpochBlockNo consensusRef

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
  , apGovActionState :: !(Maybe (ConwayGovState ConwayEra))
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
  Set.Set (Credential ColdCommitteeRole) ->
  Map.Map (Credential ColdCommitteeRole) EpochNo ->
  Ledger.UnitInterval ->
  Ledger.StrictMaybe (Committee ConwayEra) ->
  Committee ConwayEra
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

-- | In-memory ledger DB. Checkpoints are stored newest-first.
-- Uses StrictSeq for strict spine and elements.
data LedgerDB = LedgerDB
  { ledgerDbCheckpoints :: !(StrictSeq DbSyncStateRef)
  }

data SnapshotPoint = OnDisk DiskSnapshot | InMemory CardanoPoint

-- | Per-era pure getters and setters on @NewEpochState@. Note this is a bit of an abuse
-- of the cardano-ledger/ouroboros-consensus public APIs, because ledger state is not
-- designed to be updated this way. We are only replaying the chain, so this should be
-- safe.
class HasNewEpochState era where
  getNewEpochState :: ExtLedgerState CardanoBlock mk -> Maybe (NewEpochState era)

  applyNewEpochState ::
    NewEpochState era ->
    ExtLedgerState CardanoBlock mk ->
    ExtLedgerState CardanoBlock mk

instance HasNewEpochState ShelleyEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateShelley shelley -> Just (shelleyLedgerState shelley)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState AllegraEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateAllegra allegra -> Just (shelleyLedgerState allegra)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState MaryEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateMary mary -> Just (shelleyLedgerState mary)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState AlonzoEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateAlonzo alonzo -> Just (shelleyLedgerState alonzo)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState BabbageEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateBabbage babbage -> Just (shelleyLedgerState babbage)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* fn id
        :* Nil

instance HasNewEpochState ConwayEra where
  getNewEpochState st = case ledgerState st of
    LedgerStateConway conway -> Just (shelleyLedgerState conway)
    _ -> Nothing

  applyNewEpochState st =
    hApplyExtLedgerState $
      fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn id
        :* fn (applyNewEpochState' st)
        :* fn id
        :* Nil

hApplyExtLedgerState ::
  NP (Flip LedgerState mk -.-> Flip LedgerState mk) (CardanoShelleyEras StandardCrypto) ->
  ExtLedgerState CardanoBlock mk ->
  ExtLedgerState CardanoBlock mk
hApplyExtLedgerState f ledger =
  case ledgerState ledger of
    HardForkLedgerState hfState ->
      let newHfState = hap (fn id :* f) hfState
       in updateLedgerState $ HardForkLedgerState newHfState
  where
    updateLedgerState st = ledger {ledgerState = st}

applyNewEpochState' ::
  NewEpochState era ->
  Flip LedgerState mk (ShelleyBlock proto era) ->
  Flip LedgerState mk (ShelleyBlock proto era)
applyNewEpochState' newEpochState' ledger =
  Flip $ updateNewEpochState (unFlip ledger)
  where
    updateNewEpochState l = l {shelleyLedgerState = newEpochState'}

-- | A @Traversal@ that targets the @NewEpochState@ from the extended ledger state
newEpochStateT ::
  HasNewEpochState era =>
  Traversal' (ExtLedgerState CardanoBlock mk) (NewEpochState era)
newEpochStateT f ledger =
  case getNewEpochState ledger of
    Just newEpochState' -> flip applyNewEpochState ledger <$> f newEpochState'
    Nothing -> pure ledger
