{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.ParamProposal
  ( ParamProposal (..)
  , convertParamProposal
  ) where

import           Cardano.Prelude

import           Cardano.DbSync.Era.Shelley.Generic.Util (unKeyHashRaw)
import           Cardano.DbSync.Era.Shelley.Generic.Witness (Witness (..))

import           Cardano.Ledger.Coin (Coin)
import           Cardano.Ledger.Shelley (ShelleyEra)
import qualified Cardano.Ledger.ShelleyMA as ShelleyMA

import           Cardano.Slotting.Slot (EpochNo (..))

import qualified Data.Map.Strict as Map

import           Shelley.Spec.Ledger.BaseTypes (UnitInterval, strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

data ParamProposal = ParamProposal
  { pppEpochNo :: !EpochNo
  , pppKey :: !ByteString
  , pppMinFeeA :: !(Maybe Natural)
  , pppMinFeeB :: !(Maybe Natural)
  , pppMaxBlockSize :: !(Maybe Natural)
  , pppMaxTxSize :: !(Maybe Natural)
  , pppMaxBhSize :: !(Maybe Natural)
  , pppKeyDeposit :: !(Maybe Coin)
  , pppPoolDeposit :: !(Maybe Coin)
  , pppMaxEpoch :: !(Maybe EpochNo)
  , pppOptimalPoolCount :: !(Maybe Natural)
  , pppInfluence :: !(Maybe Rational)
  , pppMonetaryExpandRate :: !(Maybe UnitInterval)
  , pppTreasuryGrowthRate :: !(Maybe UnitInterval)
  , pppDecentralisation :: !(Maybe UnitInterval)
  , pppEntropy :: !(Maybe Shelley.Nonce)
  , pppProtocolVersion :: !(Maybe Shelley.ProtVer)
  , pppMinUtxoValue :: !(Maybe Coin)
  , pppMinPoolCost :: !(Maybe Coin)
  }

convertParamProposal :: Witness era -> Shelley.Update era -> [ParamProposal]
convertParamProposal witness (Shelley.Update pp epoch) =
  case witness of
    Shelley _ -> shelleyParamProposal epoch pp
    Allegra _ -> allegraOrMaryParamProposal epoch pp
    Mary _ -> allegraOrMaryParamProposal epoch pp
    -- Alonzo _ -> panic "convertParamProposal: Alonzo"

-- -------------------------------------------------------------------------------------------------

allegraOrMaryParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (ShelleyMA.ShelleyMAEra a c) -> [ParamProposal]
allegraOrMaryParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (mkParamProposal epochNo) $ Map.toList umap

shelleyParamProposal :: EpochNo -> Shelley.ProposedPPUpdates (ShelleyEra c) -> [ParamProposal]
shelleyParamProposal epochNo (Shelley.ProposedPPUpdates umap) =
    map (mkParamProposal epochNo) $ Map.toList umap

mkParamProposal :: EpochNo -> (Shelley.KeyHash genesis crypto, Shelley.PParams' Shelley.StrictMaybe era) -> ParamProposal
mkParamProposal epochNo (key, pmap) =
  ParamProposal
    { pppEpochNo = epochNo
    , pppKey = unKeyHashRaw key
    , pppMinFeeA = strictMaybeToMaybe (Shelley._minfeeA pmap)
    , pppMinFeeB = strictMaybeToMaybe (Shelley._minfeeB pmap)
    , pppMaxBlockSize = strictMaybeToMaybe (Shelley._maxBBSize pmap)
    , pppMaxTxSize = strictMaybeToMaybe (Shelley._maxTxSize pmap)
    , pppMaxBhSize = strictMaybeToMaybe (Shelley._maxBHSize pmap)
    , pppKeyDeposit = strictMaybeToMaybe (Shelley._keyDeposit pmap)
    , pppPoolDeposit = strictMaybeToMaybe (Shelley._poolDeposit pmap)
    , pppMaxEpoch = strictMaybeToMaybe (Shelley._eMax pmap)
    , pppOptimalPoolCount = strictMaybeToMaybe (Shelley._nOpt pmap)
    , pppInfluence = strictMaybeToMaybe (Shelley._a0 pmap)
    , pppMonetaryExpandRate = strictMaybeToMaybe (Shelley._rho pmap)
    , pppTreasuryGrowthRate = strictMaybeToMaybe (Shelley._tau pmap)
    , pppDecentralisation = strictMaybeToMaybe (Shelley._d pmap)
    , pppEntropy = strictMaybeToMaybe (Shelley._extraEntropy pmap)
    , pppProtocolVersion = strictMaybeToMaybe (Shelley._protocolVersion pmap)
    , pppMinUtxoValue = strictMaybeToMaybe (Shelley._minUTxOValue pmap)
    , pppMinPoolCost = strictMaybeToMaybe (Shelley._minPoolCost pmap)
    }
