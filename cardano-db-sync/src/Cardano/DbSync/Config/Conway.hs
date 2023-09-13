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
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Base16 as Base16
import Data.Default.Class (def)
import qualified Data.Text as Text
import Prelude ()

type ExceptIO e = ExceptT e IO

data ConwayGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashConway !GenesisHashConway -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Eq, Show)

readConwayGenesisConfig ::
  SyncNodeConfig ->
  ExceptIO SyncNodeError (ConwayGenesis StandardCrypto)
readConwayGenesisConfig SyncNodeConfig {..} =
  case dncConwayGenesisFile of
    Just file -> readConwayGenesisConfig' file dncConwayGenesisHash
    Nothing -> pure (ConwayGenesis def)
  where
    readConwayGenesisConfig' file hash =
      firstExceptT (SNErrConwayConfig (unGenesisFile file) . renderConwayGenesisError) $
        readGenesis file hash

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashConway ->
  ExceptIO ConwayGenesisError (ConwayGenesis StandardCrypto)
readGenesis (GenesisFile file) expectedHash = do
  content <- readFile' file
  checkExpectedGenesisHash expectedHash content
  decodeGenesis (GenesisDecodeError file) content

readFile' :: FilePath -> ExceptIO ConwayGenesisError ByteString
readFile' file =
  handleIOExceptT
    (GenesisReadError file . show)
    (ByteString.readFile file)

decodeGenesis :: (Text -> ConwayGenesisError) -> ByteString -> ExceptIO ConwayGenesisError (ConwayGenesis StandardCrypto)
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
