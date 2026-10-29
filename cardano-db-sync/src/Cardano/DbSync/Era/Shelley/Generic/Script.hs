{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.DbSync.Era.Shelley.Generic.Script (
  MultiSigScript (..),
  TimelockScript (..),
  DijkstraScript (..),
  KeyHash (..),
  fromMultiSig,
  fromTimelock,
  fromDijkstraNativeScript,
) where

import Cardano.Crypto.Hash.Class
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import Cardano.Ledger.Core (Era (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import qualified Cardano.Ledger.Dijkstra.Scripts as Dijkstra
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), coerceKeyRole)
import qualified Cardano.Ledger.Shelley.API.Types as Shelley
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Prelude hiding (show)
import Control.Monad (MonadFail (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser ())
import Data.Sequence.Strict (fromList)
import Text.Show (Show (..))
import Prelude (error)

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

-- | Dijkstra native scripts: a timelock script extended with the guard requirement.
newtype DijkstraScript era = DijkstraScript {unDijkstraScript :: Dijkstra.DijkstraNativeScript era}
  deriving (Eq)

instance Era era => Show (DijkstraScript era) where
  show = show . unDijkstraScript

fromMultiSig ::
  Shelley.MultiSig era ->
  MultiSigScript era
fromMultiSig = MultiSigScript

fromTimelock :: Allegra.Timelock era -> TimelockScript era
fromTimelock = TimelockScript

fromDijkstraNativeScript :: Dijkstra.DijkstraNativeScript era -> DijkstraScript era
fromDijkstraNativeScript = DijkstraScript

instance (Era era, Shelley.ShelleyEraScript era, Core.NativeScript era ~ Shelley.MultiSig era) => Aeson.ToJSON (MultiSigScript era) where
  toJSON (MultiSigScript script) = multiSigToJSON script
    where
      multiSigToJSON :: Shelley.MultiSig era -> Aeson.Value
      multiSigToJSON (Shelley.RequireMOf req scripts) =
        requireMOfToJSON req $ toList (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireAnyOf scripts) =
        requireAnyOfToJSON $ toList (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireAllOf scripts) =
        requireAllOfToJSON $ toList (map multiSigToJSON scripts)
      multiSigToJSON (Shelley.RequireSignature sig) =
        requireSignatureToJSON sig
      multiSigToJSON _ = Aeson.Null -- This can never happen

instance (Era era, Shelley.ShelleyEraScript era, Core.NativeScript era ~ Shelley.MultiSig era) => FromJSON (MultiSigScript era) where
  parseJSON v = MultiSigScript <$> parseMultiSig v

instance (Era era, Allegra.AllegraEraScript era, Core.NativeScript era ~ Allegra.Timelock era) => ToJSON (TimelockScript era) where
  toJSON (TimelockScript script) = timelockToJSON script
    where
      timelockToJSON (Shelley.RequireMOf req scripts) =
        requireMOfToJSON req (map timelockToJSON $ toList scripts)
      timelockToJSON (Shelley.RequireAnyOf scripts) =
        requireAnyOfToJSON (map timelockToJSON $ toList scripts)
      timelockToJSON (Shelley.RequireAllOf scripts) =
        requireAllOfToJSON (map timelockToJSON $ toList scripts)
      timelockToJSON (Shelley.RequireSignature sig) = requireSignatureToJSON sig
      timelockToJSON (Allegra.RequireTimeStart slot) = requireTimeStartToJSON slot
      timelockToJSON (Allegra.RequireTimeExpire slot) = requireTimeExpireToJSON slot
      timelockToJSON _ = error "Impossible: All NativeScripts should have been accounted for"

instance (Era era, Allegra.AllegraEraScript era, Core.NativeScript era ~ Allegra.Timelock era) => FromJSON (TimelockScript era) where
  parseJSON v = TimelockScript <$> parseTimelock v

-- | A Dijkstra native script is a timelock script plus the @guard@ requirement, so it
-- serialises to the same JSON as a timelock with one extra script type.
instance
  ( Era era
  , Dijkstra.DijkstraEraScript era
  , Core.NativeScript era ~ Dijkstra.DijkstraNativeScript era
  ) =>
  ToJSON (DijkstraScript era)
  where
  toJSON (DijkstraScript script) = dijkstraToJSON script
    where
      dijkstraToJSON (Shelley.RequireMOf req scripts) =
        requireMOfToJSON req (map dijkstraToJSON $ toList scripts)
      dijkstraToJSON (Shelley.RequireAnyOf scripts) =
        requireAnyOfToJSON (map dijkstraToJSON $ toList scripts)
      dijkstraToJSON (Shelley.RequireAllOf scripts) =
        requireAllOfToJSON (map dijkstraToJSON $ toList scripts)
      dijkstraToJSON (Shelley.RequireSignature sig) = requireSignatureToJSON sig
      dijkstraToJSON (Allegra.RequireTimeStart slot) = requireTimeStartToJSON slot
      dijkstraToJSON (Allegra.RequireTimeExpire slot) = requireTimeExpireToJSON slot
      dijkstraToJSON (Dijkstra.RequireGuard cred) = requireGuardToJSON cred
      dijkstraToJSON _ = error "Impossible: All NativeScripts should have been accounted for"

instance
  ( Era era
  , Dijkstra.DijkstraEraScript era
  , Core.NativeScript era ~ Dijkstra.DijkstraNativeScript era
  ) =>
  FromJSON (DijkstraScript era)
  where
  parseJSON v = DijkstraScript <$> parseDijkstraNativeScript v

requireSignatureToJSON :: KeyHash discr -> Aeson.Value
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

-- | A guard requirement names the credential that must be guarded: a key hash whose key
-- has to sign, or a script hash whose script has to run. The field name says which.
requireGuardToJSON :: Credential Guard -> Aeson.Value
requireGuardToJSON cred =
  Aeson.object $ ("type" .= Aeson.String "guard") : [credentialField]
  where
    credentialField =
      case cred of
        KeyHashObj (KeyHash kh) -> "keyHash" .= Aeson.String (hashToTextAsHex kh)
        ScriptHashObj (ScriptHash sh) -> "scriptHash" .= Aeson.String (hashToTextAsHex sh)

parseMultiSig ::
  (Shelley.ShelleyEraScript era, Core.NativeScript era ~ Shelley.MultiSig era) =>
  Aeson.Value ->
  Parser (Shelley.MultiSig era)
parseMultiSig v =
  (Shelley.RequireSignature <$> parseScriptSig')
    <|> (Shelley.RequireAllOf <$> parseScriptAll')
    <|> (Shelley.RequireAnyOf <$> parseScriptAny')
    <|> (uncurry Shelley.RequireMOf <$> parseScriptMOf')
  where
    parseScriptSig' = coerceKeyRole <$> parseScriptSig v
    parseScriptAll' = fromList . map unMultiSigScript <$> parseScriptAll v
    parseScriptAny' = fromList . map unMultiSigScript <$> parseScriptAny v
    parseScriptMOf' = second (fromList . map unMultiSigScript) <$> parseScriptMOf v

parseTimelock ::
  (Allegra.AllegraEraScript era, Core.NativeScript era ~ Allegra.Timelock era) =>
  Aeson.Value ->
  Parser (Allegra.Timelock era)
parseTimelock v =
  (Shelley.RequireSignature <$> parseScriptSig')
    <|> (Shelley.RequireAllOf <$> parseScriptAll')
    <|> (Shelley.RequireAnyOf <$> parseScriptAny')
    <|> (uncurry Shelley.RequireMOf <$> parseScriptMOf')
    <|> parseTimelockExpire v
    <|> parseTimelockStart v
  where
    parseScriptSig' = coerceKeyRole <$> parseScriptSig v
    parseScriptAll' = fromList . map unTimelockScript <$> parseScriptAll v
    parseScriptAny' = fromList . map unTimelockScript <$> parseScriptAny v
    parseScriptMOf' = second (fromList . map unTimelockScript) <$> parseScriptMOf v

parseDijkstraNativeScript ::
  (Dijkstra.DijkstraEraScript era, Core.NativeScript era ~ Dijkstra.DijkstraNativeScript era) =>
  Aeson.Value ->
  Parser (Dijkstra.DijkstraNativeScript era)
parseDijkstraNativeScript v =
  (Shelley.RequireSignature <$> parseScriptSig')
    <|> (Shelley.RequireAllOf <$> parseScriptAll')
    <|> (Shelley.RequireAnyOf <$> parseScriptAny')
    <|> (uncurry Shelley.RequireMOf <$> parseScriptMOf')
    <|> parseTimelockExpire v
    <|> parseTimelockStart v
    <|> parseGuard v
  where
    parseScriptSig' = coerceKeyRole <$> parseScriptSig v
    parseScriptAll' = fromList . map unDijkstraScript <$> parseScriptAll v
    parseScriptAny' = fromList . map unDijkstraScript <$> parseScriptAny v
    parseScriptMOf' = second (fromList . map unDijkstraScript) <$> parseScriptMOf v

parseGuard ::
  (Dijkstra.DijkstraEraScript era, Core.NativeScript era ~ Dijkstra.DijkstraNativeScript era) =>
  Aeson.Value ->
  Parser (Dijkstra.DijkstraNativeScript era)
parseGuard = Aeson.withObject "guard" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "guard" -> Dijkstra.mkRequireGuard <$> parseGuardCredential obj
    _ -> fail "\"guard\" script value not found"

parseGuardCredential :: Aeson.Object -> Parser (Credential Guard)
parseGuardCredential obj =
  (KeyHashObj . coerceKeyRole <$> (obj .: "keyHash" >>= parseKeyHash))
    <|> (ScriptHashObj <$> (obj .: "scriptHash" >>= parseScriptHash))

parseScriptHash :: Text -> Parser ScriptHash
parseScriptHash v =
  case ScriptHash <$> hashFromBytesAsHex (encodeUtf8 v) of
    Just res -> pure res
    Nothing -> fail $ "Error deserialising script hash: " <> show v

parseScriptSig ::
  Aeson.Value ->
  Parser (KeyHash Payment)
parseScriptSig = Aeson.withObject "sig" $ \obj -> do
  v <- obj .: "type"
  case (v :: Text) of
    "sig" -> do
      keyHash <- obj .: "keyHash"
      parseKeyHash keyHash
    _ -> fail "\"sig\" script value not found"

parseKeyHash ::
  Text ->
  Parser (KeyHash Payment)
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
  FromJSON script =>
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

parseTimelockExpire :: Allegra.AllegraEraScript era => Aeson.Value -> Parser (Core.NativeScript era)
parseTimelockExpire = Aeson.withObject "before" $ \obj -> do
  v <- obj .: "type"

  case (v :: Text) of
    "before" -> Allegra.RequireTimeExpire <$> (obj .: "slot")
    _ -> fail "\"before\" script value not found"

parseTimelockStart :: Allegra.AllegraEraScript era => Aeson.Value -> Parser (Core.NativeScript era)
parseTimelockStart =
  Aeson.withObject "after" $ \obj -> do
    v <- obj .: "type"

    case (v :: Text) of
      "after" -> Allegra.RequireTimeStart <$> (obj .: "slot")
      _ -> fail "\"after\" script value not found"
