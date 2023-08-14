{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Era.Shelley.Generic.ScriptData (
  ScriptData (..),
) where

import Cardano.Ledger.Alonzo.Scripts.Data (Data (..), getPlutusData)
import Cardano.Ledger.Binary (Annotator (..), DecCBOR (..), ToCBOR (..), fromPlainDecoder)
import Cardano.Ledger.Binary.Encoding (EncCBOR (..))
import Cardano.Ledger.Era (Era (..))
import Cardano.Prelude hiding (show)
import Codec.Serialise (Serialise (..))
import Control.Monad (fail)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Types (Parser ())
import Data.Bifoldable
import qualified Data.ByteString.Base16 as Base16
import Data.Scientific (Scientific (..), toBoundedInteger)
import Data.Vector (fromList)
import qualified PlutusLedgerApi.V1 as Plutus
import Text.Show (Show (..))
import Prelude ()

newtype ScriptData era = ScriptData {unScriptData :: Data era}
  deriving (Eq, Generic, EncCBOR, ToCBOR)

instance Era era => Show (ScriptData era) where
  show (ScriptData (Data d)) = show d

instance Era era => DecCBOR (Annotator (ScriptData era)) where
  decCBOR = pure <$> fromPlainDecoder decode

instance Era era => Serialise (ScriptData era) where
  encode (ScriptData (Data d)) = encode d
  decode = ScriptData . Data <$> decode

instance ToJSON (ScriptData era) where
  toJSON (ScriptData ds) = plutusDataToJSON $ getPlutusData ds

instance Era era => FromJSON (ScriptData era) where
  parseJSON = fmap fromPlutusData . parsePlutusData

fromPlutusData :: Era era => Plutus.Data -> ScriptData era
fromPlutusData = ScriptData . Data

parsePlutusData :: Aeson.Value -> Parser Plutus.Data
parsePlutusData = Aeson.withObject "toplevel" parseTopLevel

parseTopLevel :: Aeson.Object -> Parser Plutus.Data
parseTopLevel obj = case Aeson.toList obj of
  [ ("constructor", Aeson.Number n)
    , ("fields", Aeson.Array fs)
    ] -> parseConstr n (toList fs)
  [("map", Aeson.Array kvs)] -> parseMap $ toList kvs
  [("list", Aeson.Array ls)] -> parseList $ toList ls
  [("int", Aeson.Number n)] -> parseInteger n
  [("bytes", Aeson.String bs)] -> parseBytes bs
  _ -> fail "bad json object"

parseConstr :: Scientific -> [Aeson.Value] -> Parser Plutus.Data
parseConstr constr fields = Plutus.Constr <$> constr' <*> fields'
  where
    constr' =
      maybe
        (fail $ "Constr not an integer: " <> show constr)
        (pure . fromIntegral)
        (toBoundedInteger @Int constr)

    fields' = mapM parsePlutusData fields

parseMap :: [Aeson.Value] -> Parser Plutus.Data
parseMap kvs = Plutus.Map <$> mapM parseKV kvs
  where
    parseKV (Aeson.Object ms) = do
      let maybeKV = (,) <$> Aeson.lookup "k" ms <*> Aeson.lookup "v" ms
      case maybeKV of
        Just (k, v) -> (,) <$> parsePlutusData k <*> parsePlutusData v
        Nothing -> fail "incorrect map pair"
    parseKV _ = fail "not a map"

parseList :: [Aeson.Value] -> Parser Plutus.Data
parseList ls = Plutus.List <$> mapM parsePlutusData ls

parseBytes :: Text -> Parser Plutus.Data
parseBytes bs = bifoldMap fail (pure . Plutus.B) bytes
  where
    bytes = Base16.decode (encodeUtf8 bs)

parseInteger :: Scientific -> Parser Plutus.Data
parseInteger n = Plutus.I . fromIntegral <$> parseInt n

parseInt :: Scientific -> Parser Int
parseInt n = maybe (fail errMsg) pure (toBoundedInteger @Int n)
  where
    errMsg = "Number is not an integer: " <> show n

plutusDataToJSON :: Plutus.Data -> Aeson.Value
plutusDataToJSON (Plutus.Constr n xs) = constrToJSON n xs
plutusDataToJSON (Plutus.Map ms) = mapToJSON ms
plutusDataToJSON (Plutus.List xs) = listToJSON xs
plutusDataToJSON (Plutus.I n) = intToJSON n
plutusDataToJSON (Plutus.B bs) = bytesToJSON bs

mapToJSON :: [(Plutus.Data, Plutus.Data)] -> Aeson.Value
mapToJSON ms = Aeson.object [("map", jsonMaps)]
  where
    jsonMaps = Aeson.Array $ fromList (map mapKv ms)
    mapKv (k, v) =
      Aeson.object
        [ ("k", plutusDataToJSON k)
        , ("v", plutusDataToJSON v)
        ]

constrToJSON :: Integer -> [Plutus.Data] -> Aeson.Value
constrToJSON n fields =
  Aeson.object
    [ ("fields", toJSONArray $ map plutusDataToJSON fields)
    , ("constructor", Aeson.Number $ fromInteger n)
    ]

listToJSON :: [Plutus.Data] -> Aeson.Value
listToJSON xs =
  Aeson.object [("list", toJSONArray $ map plutusDataToJSON xs)]

intToJSON :: Integer -> Aeson.Value
intToJSON i = Aeson.object [("int", Aeson.Number $ fromInteger i)]

bytesToJSON :: ByteString -> Aeson.Value
bytesToJSON bs = Aeson.object [("bytes", bytes' bs)]
  where
    bytes' = Aeson.String . decodeUtf8 . Base16.encode

toJSONArray :: [Aeson.Value] -> Aeson.Value
toJSONArray = Aeson.Array . fromList
