{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples where

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
import           Cardano.Ledger.Mary.Value

import qualified Plutus.V1.Ledger.Examples as Plutus
import qualified PlutusCore.Data as Plutus

alwaysSucceedsScript :: Script (AlonzoEra StandardCrypto)
alwaysSucceedsScript = PlutusScript PlutusV1 (Plutus.alwaysSucceedingNAryFunction 0)

alwaysSucceedsScriptHash :: ScriptHash StandardCrypto
alwaysSucceedsScriptHash = hashScript @(AlonzoEra StandardCrypto) alwaysSucceedsScript

alwaysSucceedsScriptAddr :: Addr StandardCrypto
alwaysSucceedsScriptAddr = Addr Testnet (ScriptHashObj alwaysSucceedsScriptHash) StakeRefNull

alwaysSucceedsScriptStake :: StakeCredential StandardCrypto
alwaysSucceedsScriptStake = ScriptHashObj alwaysSucceedsScriptHash


alwaysFailsScript :: Script (AlonzoEra StandardCrypto)
alwaysFailsScript = PlutusScript PlutusV1 (Plutus.alwaysFailingNAryFunction 0)

alwaysFailsScriptHash :: ScriptHash StandardCrypto
alwaysFailsScriptHash = hashScript @(AlonzoEra StandardCrypto) alwaysFailsScript

-- addr_test1wrqvvu0m5jpkgxn3hwfd829hc5kfp0cuq83tsvgk44752dsz4mvrk
alwaysFailsScriptAddr :: Addr StandardCrypto
alwaysFailsScriptAddr = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) StakeRefNull

alwaysFailsScriptStake :: StakeCredential StandardCrypto
alwaysFailsScriptStake = ScriptHashObj alwaysFailsScriptHash


plutusDataList :: Data (AlonzoEra StandardCrypto)
plutusDataList = Data $ Plutus.List []


alwaysMintScript :: Script (AlonzoEra StandardCrypto)
alwaysMintScript = PlutusScript PlutusV1 (Plutus.alwaysFailingNAryFunction 1)

alwaysMintScriptHash :: ScriptHash StandardCrypto
alwaysMintScriptHash = hashScript @(AlonzoEra StandardCrypto) alwaysMintScript

alwaysMintScriptAddr :: Addr StandardCrypto
alwaysMintScriptAddr = Addr Testnet (ScriptHashObj alwaysMintScriptHash) StakeRefNull

alwaysMintScriptStake :: StakeCredential StandardCrypto
alwaysMintScriptStake = ScriptHashObj alwaysMintScriptHash


assetNames :: [AssetName]
assetNames =
  [ AssetName "abc"
  , AssetName "degwte"
  , AssetName "w4yt4230\\0"
  ]
