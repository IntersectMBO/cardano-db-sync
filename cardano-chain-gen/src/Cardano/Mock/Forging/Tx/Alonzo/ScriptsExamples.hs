{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples (
  alwaysSucceedsPlutusBinary,
  alwaysSucceedsScript,
  alwaysSucceedsScriptHash,
  alwaysSucceedsScriptAddr,
  alwaysSucceedsScriptStake,
  alwaysFailsPlutusBinary,
  alwaysFailsScript,
  alwaysFailsScriptHash,
  alwaysFailsScriptAddr,
  alwaysFailsScriptStake,
  plutusDataList,
  alwaysMintPlutusBinary,
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
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era
import Cardano.Ledger.Hashes
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.Plutus.Language
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise
import Codec.Serialise.Encoding
import Data.ByteString.Short
import Data.Maybe
import Ouroboros.Consensus.Cardano.Block (StandardAlonzo)
import qualified PlutusCore.Data as Plutus
import qualified PlutusLedgerApi.Test.Examples as Plutus

alwaysSucceedsPlutusBinary :: PlutusBinary
alwaysSucceedsPlutusBinary = PlutusBinary $ Plutus.alwaysSucceedingNAryFunction 0

alwaysSucceedsScript :: AlonzoEraScript era => AlonzoScript era
alwaysSucceedsScript = mkPlutusScriptEra alwaysSucceedsPlutusBinary

alwaysSucceedsScriptHash :: ScriptHash StandardCrypto
alwaysSucceedsScriptHash = scriptHash @StandardAlonzo alwaysSucceedsScript

alwaysSucceedsScriptAddr :: Addr StandardCrypto
alwaysSucceedsScriptAddr = Addr Testnet (ScriptHashObj alwaysSucceedsScriptHash) StakeRefNull

alwaysSucceedsScriptStake :: StakeCredential StandardCrypto
alwaysSucceedsScriptStake = ScriptHashObj alwaysSucceedsScriptHash

alwaysFailsPlutusBinary :: PlutusBinary
alwaysFailsPlutusBinary = PlutusBinary $ Plutus.alwaysFailingNAryFunction 0

alwaysFailsScript :: AlonzoEraScript era => AlonzoScript era
alwaysFailsScript = mkPlutusScriptEra alwaysFailsPlutusBinary

alwaysFailsScriptHash :: ScriptHash StandardCrypto
alwaysFailsScriptHash = scriptHash @StandardAlonzo alwaysFailsScript

-- addr_test1wrqvvu0m5jpkgxn3hwfd829hc5kfp0cuq83tsvgk44752dsz4mvrk
alwaysFailsScriptAddr :: Addr StandardCrypto
alwaysFailsScriptAddr = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) StakeRefNull

alwaysFailsScriptStake :: StakeCredential StandardCrypto
alwaysFailsScriptStake = ScriptHashObj alwaysFailsScriptHash

plutusDataList :: forall era. Era era => Data era
plutusDataList = Data $ Plutus.List []

alwaysMintPlutusBinary :: PlutusBinary
alwaysMintPlutusBinary = PlutusBinary $ Plutus.alwaysFailingNAryFunction 1

alwaysMintScript :: AlonzoEraScript era => AlonzoScript era
alwaysMintScript = mkPlutusScriptEra alwaysMintPlutusBinary

alwaysMintScriptHash :: ScriptHash StandardCrypto
alwaysMintScriptHash = scriptHash @StandardAlonzo alwaysMintScript

alwaysMintScriptAddr :: Addr StandardCrypto
alwaysMintScriptAddr = Addr Testnet (ScriptHashObj alwaysMintScriptHash) StakeRefNull

alwaysMintScriptStake :: StakeCredential StandardCrypto
alwaysMintScriptStake = ScriptHashObj alwaysMintScriptHash

mkPlutusScriptEra :: AlonzoEraScript era => PlutusBinary -> AlonzoScript era
mkPlutusScriptEra sh = PlutusScript $ fromJust $ mkBinaryPlutusScript PlutusV1 sh

scriptHash ::
  forall era.
  ( EraCrypto era ~ StandardCrypto
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
