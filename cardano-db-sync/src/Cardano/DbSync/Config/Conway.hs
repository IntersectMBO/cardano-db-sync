{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.DbSync.Config.Conway (
  ConwayGenesisError (..),
  readConwayGenesisConfig,
  readGenesis,
) where

import Cardano.Crypto.Hash (hashToBytes, hashWith)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Conway.PParams (UpgradeConwayPParams (..))
import Cardano.Ledger.Plutus.CostModels (mkCostModel)
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Base16 as Base16
import Data.Default.Class (def)
import qualified Data.Text as Text
import Prelude (error)

type ExceptIO e = ExceptT e IO

data ConwayGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashConway !GenesisHashConway -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Eq, Show)

readConwayGenesisConfig ::
  SyncNodeConfig ->
  ExceptIO SyncNodeError ConwayGenesis
readConwayGenesisConfig SyncNodeConfig {..} =
  case dncConwayGenesisFile of
    Just file -> readConwayGenesisConfig' file dncConwayGenesisHash
    Nothing -> pure (ConwayGenesis defaultUpgradeConwayPParams def def mempty mempty)
  where
    readConwayGenesisConfig' file hash =
      firstExceptT (SNErrConwayConfig (unGenesisFile file) . renderConwayGenesisError) $
        readGenesis file hash
    defaultUpgradeConwayPParams :: UpgradeConwayPParams Identity
    defaultUpgradeConwayPParams =
      UpgradeConwayPParams
        { ucppPoolVotingThresholds = def
        , ucppDRepVotingThresholds = def
        , ucppCommitteeMinSize = 0
        , ucppCommitteeMaxTermLength = EpochInterval 0
        , ucppGovActionLifetime = EpochInterval 0
        , ucppGovActionDeposit = Coin 0
        , ucppDRepDeposit = Coin 0
        , ucppDRepActivity = EpochInterval 0
        , ucppMinFeeRefScriptCostPerByte = minBound
        , ucppPlutusV3CostModel = case mkCostModel PlutusV3 (replicate 233 0) of
            Left err -> error $ "Failed to create default Plutus V3 CostModel " <> show err
            Right cm -> cm
        }

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashConway ->
  ExceptIO ConwayGenesisError ConwayGenesis
readGenesis (GenesisFile file) expectedHash = do
  content <- readFile' file
  checkExpectedGenesisHash expectedHash content
  decodeGenesis (GenesisDecodeError file) content

readFile' :: FilePath -> ExceptIO ConwayGenesisError ByteString
readFile' file =
  handleIOExceptT
    (GenesisReadError file . show)
    (ByteString.readFile file)

decodeGenesis :: (Text -> ConwayGenesisError) -> ByteString -> ExceptIO ConwayGenesisError ConwayGenesis
decodeGenesis f =
  firstExceptT (f . Text.pack)
    . hoistEither
    . Aeson.eitherDecodeStrict'

checkExpectedGenesisHash ::
  Maybe GenesisHashConway ->
  ByteString ->
  ExceptIO ConwayGenesisError ()
checkExpectedGenesisHash Nothing _ = pure ()
checkExpectedGenesisHash (Just expected) content
  | actualHash == expected = pure ()
  | otherwise = left (GenesisHashMismatch actualHash expected)
  where
    actualHash = GenesisHashConway $ hashWith identity content

renderConwayGenesisError :: ConwayGenesisError -> Text
renderConwayGenesisError = \case
  GenesisReadError fp err ->
    mconcat
      [ "There was an error reading the genesis file: "
      , Text.pack fp
      , " Error: "
      , err
      ]
  GenesisHashMismatch actual expected ->
    mconcat
      [ "Wrong Conway genesis file: the actual hash is "
      , renderHash actual
      , ", but the expected Conway genesis hash given in the node "
      , "configuration file is "
      , renderHash expected
      , "."
      ]
  GenesisDecodeError fp err ->
    mconcat
      [ "There was an error parsing the genesis file: "
      , Text.pack fp
      , " Error: "
      , err
      ]

renderHash :: GenesisHashConway -> Text
renderHash = decodeUtf8 . Base16.encode . hashToBytes . unGenesisHashConway
