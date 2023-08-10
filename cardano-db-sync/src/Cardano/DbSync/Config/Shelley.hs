{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DbSync.Config.Shelley (
  ShelleyConfig (..),
  readShelleyGenesisConfig,
) where

import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Db (textShow)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger.Block ()
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

-- Everything in this file should be in ouroboros-consensus so that both 'node' and 'db-=sync'
-- can use it.

data ShelleyConfig = ShelleyConfig
  { scConfig :: !(ShelleyGenesis StandardCrypto)
  , scGenesisHash :: !GenesisHashShelley
  }

readShelleyGenesisConfig ::
  SyncNodeConfig ->
  ExceptT SyncNodeError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ dncShelleyGenesisFile enc
  firstExceptT (SNErrShelleyConfig file . renderShelleyGenesisError) $
    readGenesis (GenesisFile file) Nothing

-- -------------------------------------------------------------------------------------------------

data ShelleyGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Show)

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashShelley ->
  ExceptT ShelleyGenesisError IO ShelleyConfig
readGenesis (GenesisFile file) mbExpectedGenesisHash = do
  content <- handleIOExceptT (GenesisReadError file . textShow) $ BS.readFile file
  let genesisHash = GenesisHashShelley (Crypto.hashWith id content)
  checkExpectedGenesisHash genesisHash
  genesis <-
    firstExceptT (GenesisDecodeError file . Text.pack)
      . hoistEither
      $ Aeson.eitherDecodeStrict' content
  pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected
          | actual /= expected ->
              left (GenesisHashMismatch actual expected)
        _ -> pure ()

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
  case sge of
    GenesisReadError fp err ->
      mconcat
        [ "There was an error reading the genesis file: "
        , Text.pack fp
        , " Error: "
        , err
        ]
    GenesisHashMismatch actual expected ->
      mconcat
        [ "Wrong Shelley genesis file: the actual hash is "
        , renderHash actual
        , ", but the expected Shelley genesis hash given in the node "
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
