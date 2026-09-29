{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Config.Dijkstra (
  DijkstraGenesisError (..),
  readDijkstraGenesisConfig,
) where

import Cardano.Crypto.Hash (hashToBytes, hashWith)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.Ledger.Dijkstra.Genesis (DijkstraGenesis)
import Cardano.Node.Protocol.Dijkstra (emptyDijkstraGenesis)
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString.Base16 as Base16
import qualified Data.Text as Text

type ExceptIO e = ExceptT e IO

data DijkstraGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashDijkstra !GenesisHashDijkstra -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Eq, Show)

-- | The Dijkstra genesis is optional: when no @DijkstraGenesisFile@ is configured
-- we fall back to 'emptyDijkstraGenesis' (the same placeholder the node uses when
-- the field is absent).
readDijkstraGenesisConfig ::
  SyncNodeConfig ->
  ExceptIO SyncNodeError DijkstraGenesis
readDijkstraGenesisConfig SyncNodeConfig {..} =
  case dncDijkstraGenesisFile of
    Nothing -> pure emptyDijkstraGenesis
    Just file ->
      firstExceptT (SNErrDijkstraConfig (unGenesisFile file) . renderDijkstraGenesisError) $
        readGenesis file dncDijkstraGenesisHash

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashDijkstra ->
  ExceptIO DijkstraGenesisError DijkstraGenesis
readGenesis (GenesisFile file) expectedHash = do
  content <- readFile' file
  checkExpectedGenesisHash expectedHash content
  decodeGenesis (GenesisDecodeError file) content

readFile' :: FilePath -> ExceptIO DijkstraGenesisError ByteString
readFile' file =
  handleIOExceptT
    (GenesisReadError file . show)
    (ByteString.readFile file)

decodeGenesis :: (Text -> DijkstraGenesisError) -> ByteString -> ExceptIO DijkstraGenesisError DijkstraGenesis
decodeGenesis f =
  firstExceptT (f . Text.pack)
    . hoistEither
    . Aeson.eitherDecodeStrict'

checkExpectedGenesisHash ::
  Maybe GenesisHashDijkstra ->
  ByteString ->
  ExceptIO DijkstraGenesisError ()
checkExpectedGenesisHash Nothing _ = pure ()
checkExpectedGenesisHash (Just expected) content
  | actualHash == expected = pure ()
  | otherwise = left (GenesisHashMismatch actualHash expected)
  where
    actualHash = GenesisHashDijkstra $ hashWith identity content

renderDijkstraGenesisError :: DijkstraGenesisError -> Text
renderDijkstraGenesisError = \case
  GenesisReadError fp err ->
    mconcat
      [ "There was an error reading the genesis file: "
      , Text.pack fp
      , " Error: "
      , err
      ]
  GenesisHashMismatch actual expected ->
    mconcat
      [ "Wrong Dijkstra genesis file: the actual hash is "
      , renderHash actual
      , ", but the expected Dijkstra genesis hash given in the node "
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

renderHash :: GenesisHashDijkstra -> Text
renderHash = decodeUtf8 . Base16.encode . hashToBytes . unGenesisHashDijkstra
