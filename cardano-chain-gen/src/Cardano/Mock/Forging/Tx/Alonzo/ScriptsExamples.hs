{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples (
  alwaysSucceedsScript,
  alwaysSucceedsScriptHash,
  alwaysSucceedsScriptAddr,
  alwaysSucceedsScriptStake,
  alwaysFailsScript,
  alwaysFailsScriptHash,
  alwaysFailsScriptAddr,
  alwaysFailsScriptStake,
  plutusDataList,
  alwaysMintScript,
  alwaysMintScriptHash,
  alwaysMintScriptAddr,
  alwaysMintScriptStake,
  scriptHash,
  assetNames,
  plutusData2,
  plutusDataEncLen,
  plutusDataEncIndef,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.Language
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era
import Cardano.Ledger.Hashes
import Cardano.Ledger.Mary.Value
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise
import Codec.Serialise.Encoding
import Data.ByteString.Short
import Ouroboros.Consensus.Cardano.Block (StandardAlonzo)
import qualified Plutus.V1.Ledger.Examples as Plutus
import qualified PlutusCore.Data as Plutus

alwaysSucceedsScript :: forall era. AlonzoScript era
alwaysSucceedsScript = PlutusScript PlutusV1 (Plutus.alwaysSucceedingNAryFunction 0)

alwaysSucceedsScriptHash :: ScriptHash StandardCrypto
alwaysSucceedsScriptHash = scriptHash @StandardAlonzo alwaysSucceedsScript

alwaysSucceedsScriptAddr :: Addr StandardCrypto
alwaysSucceedsScriptAddr = Addr Testnet (ScriptHashObj alwaysSucceedsScriptHash) StakeRefNull

alwaysSucceedsScriptStake :: StakeCredential StandardCrypto
alwaysSucceedsScriptStake = ScriptHashObj alwaysSucceedsScriptHash

alwaysFailsScript :: forall era. AlonzoScript era
alwaysFailsScript = PlutusScript PlutusV1 (Plutus.alwaysFailingNAryFunction 0)

alwaysFailsScriptHash :: ScriptHash StandardCrypto
alwaysFailsScriptHash = scriptHash @StandardAlonzo alwaysFailsScript

-- addr_test1wrqvvu0m5jpkgxn3hwfd829hc5kfp0cuq83tsvgk44752dsz4mvrk
alwaysFailsScriptAddr :: Addr StandardCrypto
alwaysFailsScriptAddr = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) StakeRefNull

alwaysFailsScriptStake :: StakeCredential StandardCrypto
alwaysFailsScriptStake = ScriptHashObj alwaysFailsScriptHash

plutusDataList :: forall era. Data era
plutusDataList = Data $ Plutus.List []

alwaysMintScript :: forall era. AlonzoScript era
alwaysMintScript = PlutusScript PlutusV1 (Plutus.alwaysFailingNAryFunction 1)

alwaysMintScriptHash :: ScriptHash StandardCrypto
alwaysMintScriptHash = scriptHash @StandardAlonzo alwaysMintScript

alwaysMintScriptAddr :: Addr StandardCrypto
alwaysMintScriptAddr = Addr Testnet (ScriptHashObj alwaysMintScriptHash) StakeRefNull

alwaysMintScriptStake :: StakeCredential StandardCrypto
alwaysMintScriptStake = ScriptHashObj alwaysMintScriptHash

scriptHash ::
  forall era.
  ( Crypto era ~ StandardCrypto
  , Core.Script era ~ AlonzoScript era
  , Core.EraScript era
  ) =>
  AlonzoScript era ->
  ScriptHash StandardCrypto
scriptHash = Core.hashScript @era

assetNames :: [AssetName]
assetNames =
  [ AssetName "abc"
  , AssetName "degwte"
  , AssetName "w4yt4230\\0"
  ]

plutusData2 :: [Plutus.Data]
plutusData2 = [Plutus.I 0, Plutus.I 1]

plutusDataEncLen :: ShortByteString
plutusDataEncLen = toShort $ toStrictByteString $ mconcat (encodeListLen 2 : (encode <$> plutusData2))

plutusDataEncIndef :: ShortByteString
plutusDataEncIndef = toShort $ toStrictByteString $ encodeList plutusData2
