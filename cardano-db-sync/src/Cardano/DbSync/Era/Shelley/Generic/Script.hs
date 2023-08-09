{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Generic.Script (
  MultiSigScript (..),
  TimelockScript (..),
  KeyHash (..),
  fromMultiSig,
  toMultiSig,
  fromTimelock,
  toTimelock,
) where

import Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import Cardano.Ledger.Core (EraCrypto ())
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Era (Era ())
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), coerceKeyRole)
import qualified Cardano.Ledger.Shelley.API.Types as Shelley
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Prelude
import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser ())
import Data.Sequence.Strict (fromList)
import Prelude ()

-- | Shelley multi signature scripts
data MultiSigScript
  = MSRequireSignature !(KeyHash 'Payment StandardCrypto)
  | MSRequireAllOf ![MultiSigScript]
  | MSRequireAnyOf ![MultiSigScript]
  | MSRequireMOf !Int ![MultiSigScript]
  deriving (Eq, Show)

-- | Allegra time lock scripts
data TimelockScript
  = TSRequireSignature !(KeyHash 'Payment StandardCrypto)
  | TSRequireAllOf ![TimelockScript]
  | TSRequireAnyOf ![TimelockScript]
  | TSRequireMOf !Int ![TimelockScript]
  | TSRequireTimeExpire !SlotNo
  | TSRequireTimeStart !SlotNo
  deriving (Eq, Show)

fromMultiSig ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  Shelley.MultiSig era ->
  MultiSigScript
fromMultiSig (Shelley.RequireSignature sig) = MSRequireSignature (coerceKeyRole sig)
fromMultiSig (Shelley.RequireAllOf scripts) = MSRequireAllOf $ map fromMultiSig scripts
fromMultiSig (Shelley.RequireAnyOf scripts) = MSRequireAnyOf $ map fromMultiSig scripts
fromMultiSig (Shelley.RequireMOf req scripts) = MSRequireMOf req $ map fromMultiSig scripts

toMultiSig ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  MultiSigScript ->
  Shelley.MultiSig era
toMultiSig (MSRequireSignature sig) = Shelley.RequireSignature (coerceKeyRole sig)
toMultiSig (MSRequireAllOf scripts) = Shelley.RequireAllOf $ map toMultiSig scripts
toMultiSig (MSRequireAnyOf scripts) = Shelley.RequireAnyOf $ map toMultiSig scripts
toMultiSig (MSRequireMOf req scripts) = Shelley.RequireMOf req $ map toMultiSig scripts

fromTimelock ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  Allegra.Timelock era ->
  TimelockScript
fromTimelock (Allegra.RequireSignature sig) = TSRequireSignature (coerceKeyRole sig)
fromTimelock (Allegra.RequireTimeExpire slot) = TSRequireTimeExpire slot
fromTimelock (Allegra.RequireTimeStart slot) = TSRequireTimeStart slot
fromTimelock (Allegra.RequireAllOf scripts) =
  TSRequireAllOf $ map fromTimelock (toList scripts)
fromTimelock (Allegra.RequireAnyOf scripts) =
  TSRequireAnyOf $ map fromTimelock (toList scripts)
fromTimelock (Allegra.RequireMOf req scripts) =
  TSRequireMOf req $ map fromTimelock (toList scripts)

toTimelock ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  TimelockScript ->
  Allegra.Timelock era
toTimelock (TSRequireSignature sig) = Allegra.RequireSignature (coerceKeyRole sig)
toTimelock (TSRequireTimeExpire slot) = Allegra.RequireTimeExpire slot
toTimelock (TSRequireTimeStart slot) = Allegra.RequireTimeStart slot
toTimelock (TSRequireAllOf scripts) =
  Allegra.RequireAllOf $ map toTimelock (fromList scripts)
toTimelock (TSRequireAnyOf scripts) =
  Allegra.RequireAnyOf $ map toTimelock (fromList scripts)
toTimelock (TSRequireMOf req scripts) =
  Allegra.RequireMOf req $ map toTimelock (fromList scripts)

instance Aeson.ToJSON MultiSigScript where
  toJSON (MSRequireMOf required scripts) = requireMOfToJSON required scripts
  toJSON (MSRequireAnyOf scripts) = requireAnyOfToJSON scripts
  toJSON (MSRequireAllOf scripts) = requireAllOfToJSON scripts
  toJSON (MSRequireSignature sig) = requireSignatureToJSON sig

instance FromJSON MultiSigScript where
  parseJSON v =
    (MSRequireSignature <$> parseScriptSig v)
      <|> (MSRequireAllOf <$> parseScriptAll v)
      <|> (MSRequireAnyOf <$> parseScriptAny v)
      <|> (uncurry MSRequireMOf <$> parseScriptMOf v)

instance ToJSON TimelockScript where
  toJSON (TSRequireMOf req scripts) = requireMOfToJSON req scripts
  toJSON (TSRequireAnyOf scripts) = requireAnyOfToJSON scripts
  toJSON (TSRequireAllOf scripts) = requireAllOfToJSON scripts
  toJSON (TSRequireSignature sig) = requireSignatureToJSON sig
  toJSON (TSRequireTimeExpire slot) = requireTimeExpireToJSON slot
  toJSON (TSRequireTimeStart slot) = requireTimeStartToJSON slot

instance FromJSON TimelockScript where
  parseJSON v =
    (TSRequireSignature <$> parseScriptSig v)
      <|> (TSRequireAllOf <$> parseScriptAll v)
      <|> (TSRequireAnyOf <$> parseScriptAny v)
      <|> (uncurry TSRequireMOf <$> parseScriptMOf v)
      <|> parseTimelockStart v
      <|> parseTimelockExpire v

requireSignatureToJSON :: KeyHash discr c -> Aeson.Value
requireSignatureToJSON (KeyHash sig) =
  Aeson.object
    [ "type" .= Aeson.String "sig"
    , "keyHash" .= Aeson.String (hashToTextAsHex sig)
    ]

requireAllOfToJSON :: ToJSON a => [a] -> Aeson.Value
requireAllOfToJSON scripts =
  Aeson.object
    [ "type" .= Aeson.String "all"
    , "scripts" .= map toJSON scripts
    ]

requireAnyOfToJSON :: ToJSON a => [a] -> Aeson.Value
requireAnyOfToJSON scripts =
  Aeson.object
    [ "type" .= Aeson.String "any"
    , "scripts" .= map toJSON scripts
    ]

requireMOfToJSON :: ToJSON a => Int -> [a] -> Aeson.Value
requireMOfToJSON req scripts =
  Aeson.object
    [ "type" .= Aeson.String "atLeast"
    , "required" .= req
    , "scripts" .= map toJSON scripts
    ]

requireTimeExpireToJSON :: SlotNo -> Aeson.Value
requireTimeExpireToJSON slot =
  Aeson.object
    [ "type" .= Aeson.String "before"
    , "slot" .= slot
    ]

requireTimeStartToJSON :: SlotNo -> Aeson.Value
requireTimeStartToJSON slot =
  Aeson.object
    [ "type" .= Aeson.String "after"
    , "slot" .= slot
    ]

parseScriptSig ::
  Aeson.Value ->
  Parser (KeyHash 'Payment StandardCrypto)
parseScriptSig = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "sig" -> do
      keyHash <- obj .: "keyHash"
      parseKeyHash keyHash
    _ -> fail "\"sig\" script value not found"

parseKeyHash ::
  Text ->
  Parser (KeyHash 'Payment StandardCrypto)
parseKeyHash v = do
  let maybeHash = hashFromBytesAsHex $ encodeUtf8 v
      maybeKeyHash = KeyHash <$> maybeHash
  case maybeKeyHash of
    Just res -> pure res
    Nothing -> fail $ "Error deserialising payment key hash: " <> show v

parseScriptAll ::
  FromJSON script =>
  Aeson.Value ->
  Parser [script]
parseScriptAll = Aeson.withObject "all" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "all" -> obj .: "scripts"
    _ -> fail "\"all\" script value not found"

parseScriptAny ::
  FromJSON script =>
  Aeson.Value ->
  Parser [script]
parseScriptAny = Aeson.withObject "any" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "any" -> obj .: "scripts"
    _ -> fail "\"any\" script value not found"

parseScriptMOf ::
  (FromJSON script) =>
  Aeson.Value ->
  Parser (Int, [script])
parseScriptMOf = Aeson.withObject "atLeast" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "atLeast" -> do
      scripts <- obj .: "scripts"
      req <- obj .: "required"
      when (req > length scripts) $
        reqMismatchedFailure req scripts

      pure (req, scripts)
    _ -> fail "\"atLeast\" script value not found"
  where
    reqMismatchedFailure req scripts =
      fail $
        "Required number of script signature exceeds the number of scripts."
          <> " Required: "
          <> show req
          <> " Scripts: "
          <> show (length scripts)

parseTimelockExpire ::
  Aeson.Value ->
  Parser TimelockScript
parseTimelockExpire = Aeson.withObject "before" $ \obj -> do
  v <- obj .: "type"

  case (v :: Text) of
    "before" -> TSRequireTimeExpire <$> (obj .: "slot")
    _ -> fail "\"before\" script value not found"

parseTimelockStart ::
  Aeson.Value ->
  Parser TimelockScript
parseTimelockStart =
  Aeson.withObject "after" $ \obj -> do
    v <- obj .: "type"

    case (v :: Text) of
      "after" -> TSRequireTimeStart <$> (obj .: "slot")
      _ -> fail "\"after\" script value not found"
