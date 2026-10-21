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
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error
import Cardano.Prelude (textShow)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.ListMap as ListMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Ouroboros.Consensus.Shelley.Ledger.Block ()
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

-- Everything in this file should be in ouroboros-consensus so that both 'node' and 'db-=sync'
-- can use it.

data ShelleyConfig = ShelleyConfig
  { scConfig :: !ShelleyGenesis
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
      $ Aeson.eitherDecodeStrict' content >>= parseEither parseGenesisWithInitialFunds
  pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected
          | actual /= expected ->
              left (GenesisHashMismatch actual expected)
        _ -> pure ()

-- | Newer tooling embeds funds in extraConfig.initialFunds.data. The pinned
-- ledger only understands the legacy field, so populate it before constructing
-- ledger state and inserting the genesis distribution. Hash the original bytes.
parseGenesisWithInitialFunds :: Aeson.Value -> Parser ShelleyGenesis
parseGenesisWithInitialFunds value = do
  genesis <- Aeson.parseJSON value
  Aeson.withObject
    "Shelley genesis"
    ( \obj -> do
        extra <- obj Aeson..:? "extraConfig"
        case extra of
          Nothing -> pure genesis
          Just extraValue ->
            Aeson.withObject
              "extraConfig"
              ( \extraObj -> do
                  funds <- extraObj Aeson..:? "initialFunds"
                  case funds of
                    Nothing -> pure genesis
                    Just fundsValue ->
                      Aeson.withObject
                        "initialFunds"
                        ( \fundsObj -> do
                            file <- fundsObj Aeson..:? "file" :: Parser (Maybe Aeson.Value)
                            case file of
                              Just _ -> fail "File-based initialFunds injection is not supported by db-sync"
                              Nothing -> do
                                embedded <- fundsObj Aeson..:? "data"
                                case embedded of
                                  Nothing -> pure genesis
                                  Just initialFunds
                                    | null (ListMap.unListMap (sgInitialFunds genesis)) ->
                                        pure genesis {sgInitialFunds = initialFunds}
                                    | otherwise -> fail "Conflicting legacy and extraConfig initialFunds"
                        )
                        fundsValue
              )
              extraValue
    )
    value

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
