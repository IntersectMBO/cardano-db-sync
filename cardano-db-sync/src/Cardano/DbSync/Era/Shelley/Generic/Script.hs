{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
import Cardano.Ledger.Crypto
import Cardano.Ledger.Era (Era ())
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), coerceKeyRole)
import qualified Cardano.Ledger.Shelley.API.Types as Shelley
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Prelude hiding (show)
import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser ())
import Data.Sequence.Strict (fromList)
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Text.Show (Show (..))
import Prelude ()

-- | Shelley multi signature scripts
newtype MultiSigScript era = MultiSigScript {unMultiSigScript :: Shelley.MultiSig era}
  deriving (Eq)

instance Era era => Show (MultiSigScript era) where
  show = show . unMultiSigScript

-- | Allegra time lock scripts
newtype TimelockScript era = TimelockScript {unTimelockScript :: Allegra.Timelock era}
  deriving (Eq)

instance Era era => Show (TimelockScript era) where
  show = show . unTimelockScript

fromMultiSig ::
  Shelley.MultiSig era ->
  MultiSigScript era
fromMultiSig = MultiSigScript

toMultiSig ::
  MultiSigScript era ->
  Shelley.MultiSig era
toMultiSig = unMultiSigScript

fromTimelock :: Allegra.Timelock era -> TimelockScript era
fromTimelock = TimelockScript

toTimelock :: TimelockScript era -> Allegra.Timelock era
toTimelock = unTimelockScript

instance Era era => Aeson.ToJSON (MultiSigScript era) where
  toJSON (MultiSigScript script) = multiSigToJSON script
    where
      multiSigToJSON :: Shelley.MultiSig era -> Aeson.Value
      multiSigToJSON (Shelley.RequireMOf req scripts) =
        requireMOfToJSON req (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireAnyOf scripts) =
        requireAnyOfToJSON (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireAllOf scripts) =
        requireAllOfToJSON (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireSignature sig) =
        requireSignatureToJSON sig

instance (Era era, EraCrypto era ~ StandardCrypto) => FromJSON (MultiSigScript era) where
  parseJSON v = MultiSigScript <$> parseMultiSig v

instance Era era => ToJSON (TimelockScript era) where
  toJSON (TimelockScript script) = timelockToJSON script
    where
      timelockToJSON (Allegra.RequireMOf req scripts) =
        requireMOfToJSON req (map timelockToJSON $ toList scripts)
      timelockToJSON (Allegra.RequireAnyOf scripts) =
        requireAnyOfToJSON (map timelockToJSON $ toList scripts)
      timelockToJSON (Allegra.RequireAllOf scripts) =
        requireAllOfToJSON (map timelockToJSON $ toList scripts)
      timelockToJSON (Allegra.RequireSignature sig) = requireSignatureToJSON sig
      timelockToJSON (Allegra.RequireTimeStart slot) = requireTimeStartToJSON slot
      timelockToJSON (Allegra.RequireTimeExpire slot) = requireTimeExpireToJSON slot

instance (Era era, EraCrypto era ~ StandardCrypto) => FromJSON (TimelockScript era) where
  parseJSON v = TimelockScript <$> parseTimelock v

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

parseMultiSig ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  Aeson.Value ->
  Parser (Shelley.MultiSig era)
parseMultiSig v =
  (Shelley.RequireSignature <$> parseScriptSig')
    <|> (Shelley.RequireAllOf <$> parseScriptAll')
    <|> (Shelley.RequireAnyOf <$> parseScriptAny')
    <|> (uncurry Shelley.RequireMOf <$> parseScriptMOf')
  where
    parseScriptSig' = coerceKeyRole <$> parseScriptSig v
    parseScriptAll' = map unMultiSigScript <$> parseScriptAll v
    parseScriptAny' = map unMultiSigScript <$> parseScriptAny v
    parseScriptMOf' = second (map unMultiSigScript) <$> parseScriptMOf v

parseTimelock ::
  (Era era, EraCrypto era ~ StandardCrypto) =>
  Aeson.Value ->
  Parser (Allegra.Timelock era)
parseTimelock v =
  (Allegra.RequireSignature <$> parseScriptSig')
    <|> (Allegra.RequireAllOf <$> parseScriptAll')
    <|> (Allegra.RequireAnyOf <$> parseScriptAny')
    <|> (uncurry Allegra.RequireMOf <$> parseScriptMOf')
    <|> parseTimelockExpire v
    <|> parseTimelockStart v
  where
    parseScriptSig' = coerceKeyRole <$> parseScriptSig v
    parseScriptAll' = fromList . map unTimelockScript <$> parseScriptAll v
    parseScriptAny' = fromList . map unTimelockScript <$> parseScriptAny v
    parseScriptMOf' = second (fromList . map unTimelockScript) <$> parseScriptMOf v

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

parseTimelockExpire :: Era era => Aeson.Value -> Parser (Allegra.Timelock era)
parseTimelockExpire = Aeson.withObject "before" $ \obj -> do
  v <- obj .: "type"

  case (v :: Text) of
    "before" -> Allegra.RequireTimeExpire <$> (obj .: "slot")
    _ -> fail "\"before\" script value not found"

parseTimelockStart :: Era era => Aeson.Value -> Parser (Allegra.Timelock era)
parseTimelockStart =
  Aeson.withObject "after" $ \obj -> do
    v <- obj .: "type"

    case (v :: Text) of
      "after" -> Allegra.RequireTimeStart <$> (obj .: "slot")
      _ -> fail "\"after\" script value not found"
