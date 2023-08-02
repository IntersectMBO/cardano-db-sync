{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Config.Alonzo (
  readAlonzoGenesisConfig,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (textShow)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Ledger.Alonzo.Genesis
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

readAlonzoGenesisConfig ::
  SyncNodeConfig ->
  ExceptT SyncNodeError IO AlonzoGenesis
readAlonzoGenesisConfig enc = do
  let file = unGenesisFile $ dncAlonzoGenesisFile enc
  firstExceptT (SNErrAlonzoConfig file . renderAlonzoGenesisError) $
    readGenesis (GenesisFile file) Nothing

data AlonzoGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Show)

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashShelley ->
  ExceptT AlonzoGenesisError IO AlonzoGenesis
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
  content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
  let genesisHash = GenesisHashShelley (Crypto.hashWith id content)
  checkExpectedGenesisHash genesisHash
  firstExceptT (GenesisDecodeError file . Text.pack)
    . hoistEither
    $ Aeson.eitherDecodeStrict' content
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT AlonzoGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected
          | actual /= expected ->
              left (GenesisHashMismatch actual expected)
        _ -> pure ()

renderAlonzoGenesisError :: AlonzoGenesisError -> Text
renderAlonzoGenesisError age =
  case age of
    GenesisReadError fp err ->
      mconcat
        [ "There was an error reading the genesis file: "
        , Text.pack fp
        , " Error: "
        , err
        ]
    GenesisHashMismatch actual expected ->
      mconcat
        [ "Wrong Alonzo genesis file: the actual hash is "
        , renderHash actual
        , ", but the expected Alonzo genesis hash given in the node "
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
  where
    renderHash :: GenesisHashShelley -> Text
    renderHash (GenesisHashShelley h) = Text.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)
