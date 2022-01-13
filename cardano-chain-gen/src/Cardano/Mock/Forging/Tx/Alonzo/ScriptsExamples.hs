{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples where

import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra)

import           Cardano.Ledger.Address
import           Cardano.Ledger.Alonzo
import           Cardano.Ledger.Alonzo.Data
import           Cardano.Ledger.Alonzo.Language
import           Cardano.Ledger.Alonzo.Scripts
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Era
import           Cardano.Ledger.Hashes

import           Cardano.PlutusExample.AlwaysSucceeds

import qualified PlutusCore.Data as Plutus

alwaysSuccedsScript :: Script (AlonzoEra StandardCrypto)
alwaysSuccedsScript = PlutusScript PlutusV1 alwaysSucceedsScriptShortBs

alwaysSucceedsScriptHash :: ScriptHash StandardCrypto
alwaysSucceedsScriptHash = hashScript @(AlonzoEra StandardCrypto) alwaysSuccedsScript

-- addr_test1wpnlxv2xv9a9ucvnvzqakwepzl9ltx7jzgm53av2e9ncv4sysemm8
alwaysSuccedsScriptAddr :: Addr StandardCrypto
alwaysSuccedsScriptAddr = Addr Testnet (ScriptHashObj alwaysSucceedsScriptHash) StakeRefNull


alwaysFailsScript :: Script (AlonzoEra StandardCrypto)
alwaysFailsScript = PlutusScript PlutusV1 alwaysSucceedsScriptShortBs

alwaysFailsScriptHash :: ScriptHash StandardCrypto
alwaysFailsScriptHash = hashScript @(AlonzoEra StandardCrypto) alwaysFailsScript

-- addr_test1wrqvvu0m5jpkgxn3hwfd829hc5kfp0cuq83tsvgk44752dsz4mvrk
alwaysFailsScriptAddr :: Addr StandardCrypto
alwaysFailsScriptAddr = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) StakeRefNull

plutusDataList :: Data (AlonzoEra StandardCrypto)
plutusDataList = Data $ Plutus.List []
