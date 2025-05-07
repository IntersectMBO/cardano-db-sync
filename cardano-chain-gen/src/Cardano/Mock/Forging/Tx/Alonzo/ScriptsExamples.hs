{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoEraScript (..), mkBinaryPlutusScript)
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Core (Era, Script)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential, StakeReference (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Mary.Value (AssetName (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (Language (..), PlutusBinary (..))
import Codec.CBOR.Write (toStrictByteString)
import Codec.Serialise (encode, encodeList)
import Codec.Serialise.Encoding (encodeListLen)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Maybe (fromJust)
import Ouroboros.Consensus.Cardano.Block (AlonzoEra)
import qualified PlutusCore.Data as Plutus
import qualified PlutusLedgerApi.Test.Examples as Plutus

alwaysSucceedsPlutusBinary :: PlutusBinary
alwaysSucceedsPlutusBinary = PlutusBinary $ Plutus.alwaysSucceedingNAryFunction 0

alwaysSucceedsScript :: AlonzoEraScript era => Script era
alwaysSucceedsScript = mkPlutusV1ScriptEra alwaysSucceedsPlutusBinary

alwaysSucceedsScriptHash :: ScriptHash
alwaysSucceedsScriptHash = scriptHash @AlonzoEra alwaysSucceedsScript

alwaysSucceedsScriptAddr :: Addr
alwaysSucceedsScriptAddr = Addr Testnet (ScriptHashObj alwaysSucceedsScriptHash) StakeRefNull

alwaysSucceedsScriptStake :: StakeCredential
alwaysSucceedsScriptStake = ScriptHashObj alwaysSucceedsScriptHash

alwaysFailsPlutusBinary :: PlutusBinary
alwaysFailsPlutusBinary = PlutusBinary $ Plutus.alwaysFailingNAryFunction 0

alwaysFailsScript :: AlonzoEraScript era => Script era
alwaysFailsScript = mkPlutusV1ScriptEra alwaysFailsPlutusBinary

alwaysFailsScriptHash :: ScriptHash
alwaysFailsScriptHash = scriptHash @AlonzoEra alwaysFailsScript

-- addr_test1wrqvvu0m5jpkgxn3hwfd829hc5kfp0cuq83tsvgk44752dsz4mvrk
alwaysFailsScriptAddr :: Addr
alwaysFailsScriptAddr = Addr Testnet (ScriptHashObj alwaysFailsScriptHash) StakeRefNull

alwaysFailsScriptStake :: StakeCredential
alwaysFailsScriptStake = ScriptHashObj alwaysFailsScriptHash

plutusDataList :: forall era. Era era => Data era
plutusDataList = Data $ Plutus.List []

alwaysMintPlutusBinary :: PlutusBinary
alwaysMintPlutusBinary = PlutusBinary $ Plutus.alwaysFailingNAryFunction 1

alwaysMintScript :: AlonzoEraScript era => Script era
alwaysMintScript = mkPlutusV1ScriptEra alwaysMintPlutusBinary

alwaysMintScriptHash :: ScriptHash
alwaysMintScriptHash = scriptHash @AlonzoEra alwaysMintScript

alwaysMintScriptAddr :: Addr
alwaysMintScriptAddr = Addr Testnet (ScriptHashObj alwaysMintScriptHash) StakeRefNull

alwaysMintScriptStake :: StakeCredential
alwaysMintScriptStake = ScriptHashObj alwaysMintScriptHash

mkPlutusV1ScriptEra :: AlonzoEraScript era => PlutusBinary -> Script era
mkPlutusV1ScriptEra sh = fromPlutusScript $ fromJust $ mkBinaryPlutusScript PlutusV1 sh

scriptHash ::
  forall era.
  Core.EraScript era =>
  Script era ->
  ScriptHash
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
