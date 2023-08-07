{-# LANGUAGE GADTs #-}

module Cardano.DbSync.Era.Shelley.Generic.Witness (
  Evidence (..),
  Witness (..),
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)

-- import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)

-- Cargo culted from ledger-specs. Written by Tim Sheard and PRed in
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2173
-- Even once it is merged, will need to wait until the node moves to a
-- version that this feature.

-- | Evidence that a valid (predefined) crypto exists
data Evidence c where
  Standard :: Evidence StandardCrypto

-- Test :: Evidence TestCrypto

instance Show (Evidence c) where
  show Standard = "Standard"

-- show Test = "Test"-- | Witness of a valid (predefined) era

data Witness era where
  Shelley :: Evidence c -> Witness (ShelleyEra c)
  Allegra :: Evidence c -> Witness (AllegraEra c)
  Mary :: Evidence c -> Witness (MaryEra c)
  Alonzo :: Evidence c -> Witness (AlonzoEra c)
  Babbage :: Evidence c -> Witness (BabbageEra c)

--  Conway :: Evidence c -> Witness (ConwayEra c)

instance Show (Witness e) where
  show (Shelley c) = "Shelley " ++ show c
  show (Allegra c) = "Allegra " ++ show c
  show (Mary c) = "Mary " ++ show c
  show (Alonzo c) = "Alonzo " ++ show c
  show (Babbage c) = "Babbage " ++ show c

--  show (Conway c) = "Conway " ++ show c
