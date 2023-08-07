{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.Script (
  MultiSigScript (..),
  Ledger.KeyHash (),
  fromMultiSig,
) where

import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Core (EraCrypto ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Era (Era ())
import qualified Cardano.Ledger.Shelley.API.Types as Ledger
import Cardano.Prelude
import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser ())
import Prelude ()

data MultiSigScript
  = RequireSignature !(Ledger.KeyHash 'Ledger.Payment StandardCrypto)
  | RequireAllOf ![MultiSigScript]
  | RequireAnyOf ![MultiSigScript]
  | RequireMOf !Int ![MultiSigScript]
  deriving (Eq, Show)

instance Aeson.ToJSON MultiSigScript where
  toJSON (RequireMOf required scripts) =
    Aeson.object
      [ "type" .= Aeson.String "atLeast"
      , "required" .= required
      , "scripts" .= map toJSON scripts
      ]
  toJSON (RequireAnyOf scripts) =
    Aeson.object
      [ "type" .= Aeson.String "any"
      , "scripts" .= map toJSON scripts
      ]
  toJSON (RequireAllOf scripts) =
    Aeson.object
      [ "type" .= Aeson.String "all"
      , "scripts" .= map toJSON scripts
      ]
  toJSON (RequireSignature (Ledger.KeyHash sig)) =
    Aeson.object
      [ "type" .= Aeson.String "sig"
      , "keyHash" .= Aeson.String (hashToTextAsHex sig)
      ]

instance FromJSON MultiSigScript where
  parseJSON v =
    parseScriptSig v
      <|> parseScriptAll v
      <|> parseScriptAny v
      <|> parseScriptMOf v

parseScriptSig ::
  Aeson.Value ->
  Parser MultiSigScript
parseScriptSig = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "sig" -> do
      keyHash <- obj .: "keyHash"
      RequireSignature <$> parseKeyHash keyHash
    _ -> fail "\"sig\" script value not found"

parseScriptAll ::
  Aeson.Value ->
  Parser MultiSigScript
parseScriptAll = Aeson.withObject "all" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "all" -> do
      scripts <- obj .: "scripts"
      pure $ RequireAllOf scripts
    _ -> fail "\"all\" script value not found"

parseScriptAny ::
  Aeson.Value ->
  Parser MultiSigScript
parseScriptAny = Aeson.withObject "any" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "any" -> do
      scripts <- obj .: "scripts"
      pure $ RequireAnyOf scripts
    _ -> fail "\"any\" script value not found"

parseScriptMOf ::
  Aeson.Value ->
  Parser MultiSigScript
parseScriptMOf = Aeson.withObject "atLeast" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "atLeast" -> do
      scripts <- obj .: "scripts"
      req <- obj .: "required"
      when (req > length scripts) $
        reqMismatchedFailure req scripts

      pure $ RequireMOf req scripts
    _ -> fail "\"atLeast\" script value not found"
  where
    reqMismatchedFailure req scripts =
      fail $
        "Required number of script signature exceeds the number of scripts."
          <> " Required: "
          <> show req
          <> " Scripts: "
          <> show (length scripts)

parseKeyHash ::
  Text ->
  Parser (Ledger.KeyHash 'Ledger.Payment StandardCrypto)
parseKeyHash v = do
  let maybeHash = hashFromBytesAsHex $ encodeUtf8 v
      maybeKeyHash = Ledger.KeyHash <$> maybeHash
  case maybeKeyHash of
    Just res -> pure res
    Nothing -> fail $ "Error deserialising payment key hash: " <> show v

fromMultiSig ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  Ledger.MultiSig era ->
  MultiSigScript
fromMultiSig (Ledger.RequireSignature sig) = RequireSignature (Ledger.coerceKeyRole sig)
fromMultiSig (Ledger.RequireAllOf scripts) = RequireAllOf $ map fromMultiSig scripts
fromMultiSig (Ledger.RequireAnyOf scripts) = RequireAnyOf $ map fromMultiSig scripts
fromMultiSig (Ledger.RequireMOf req scripts) = RequireMOf req $ map fromMultiSig scripts
