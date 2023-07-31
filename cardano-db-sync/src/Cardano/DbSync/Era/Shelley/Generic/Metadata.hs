{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Generic.Metadata (
  TxMetadataValue (..),
  fromAllegraMetadata,
  fromAlonzoMetadata,
  fromShelleyMetadata,
  fromMaryMetadata,
  metadataValueToJsonNoSchema,
  fromMetadatum,
  toMetadatum,
) where

import qualified Cardano.Ledger.Allegra.TxAuxData as Allegra
import qualified Cardano.Ledger.Alonzo.TxAuxData as Alonzo
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Shelley.TxAuxData as Shelley
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Tuple.Extra (both)
import qualified Data.Vector as Vector
import Ouroboros.Consensus.Cardano.Block (StandardAllegra, StandardMary, StandardShelley)

data TxMetadataValue
  = TxMetaMap ![(TxMetadataValue, TxMetadataValue)]
  | TxMetaList ![TxMetadataValue]
  | TxMetaNumber !Integer -- -2^64 .. 2^64-1
  | TxMetaBytes !ByteString
  | TxMetaText !Text
  deriving (Eq, Ord, Show)

fromAllegraMetadata :: Allegra.AllegraTxAuxData StandardAllegra -> Map Word64 TxMetadataValue
fromAllegraMetadata (Allegra.AllegraTxAuxData mdMap _scripts) =
  Map.map fromMetadatum mdMap

fromAlonzoMetadata :: Era era => Alonzo.AlonzoTxAuxData era -> Map Word64 TxMetadataValue
fromAlonzoMetadata aux =
  Map.map fromMetadatum $ Alonzo.atadMetadata aux

fromShelleyMetadata :: Shelley.ShelleyTxAuxData StandardShelley -> Map Word64 TxMetadataValue
fromShelleyMetadata (Shelley.ShelleyTxAuxData mdMap) =
  Map.map fromMetadatum mdMap

fromMaryMetadata :: Allegra.AllegraTxAuxData StandardMary -> Map Word64 TxMetadataValue
fromMaryMetadata (Allegra.AllegraTxAuxData mdMap _scripts) =
  Map.map fromMetadatum mdMap

metadataValueToJsonNoSchema :: TxMetadataValue -> Aeson.Value
metadataValueToJsonNoSchema = conv
  where
    conv :: TxMetadataValue -> Aeson.Value
    conv (TxMetaNumber n) = Aeson.Number (fromInteger n)
    conv (TxMetaBytes bs) =
      Aeson.String
        ( bytesPrefix
            <> Text.decodeLatin1 (Base16.encode bs)
        )
    conv (TxMetaText txt) = Aeson.String txt
    conv (TxMetaList vs) = Aeson.Array (Vector.fromList (map conv vs))
    conv (TxMetaMap kvs) =
      Aeson.object
        [ (Aeson.fromText $ convKey k, conv v)
        | (k, v) <- kvs
        ]

    -- Metadata allows any value as a key, not just string as JSON does.
    -- For simple types we just convert them to string dirctly.
    -- For structured keys we render them as JSON and use that as the string.
    convKey :: TxMetadataValue -> Text
    convKey (TxMetaNumber n) = Text.pack (show n)
    convKey (TxMetaBytes bs) =
      bytesPrefix
        <> Text.decodeLatin1 (Base16.encode bs)
    convKey (TxMetaText txt) = txt
    convKey v =
      Text.Lazy.toStrict
        . Aeson.Text.encodeToLazyText
        $ conv v

-- -------------------------------------------------------------------------------------------------

-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"

fromMetadatum :: Shelley.Metadatum -> TxMetadataValue
fromMetadatum = \case
  Shelley.I x -> TxMetaNumber x
  Shelley.B x -> TxMetaBytes x
  Shelley.S x -> TxMetaText x
  Shelley.List xs -> TxMetaList $ map fromMetadatum xs
  Shelley.Map xs -> TxMetaMap $ map (both fromMetadatum) xs

toMetadatum :: TxMetadataValue -> Shelley.Metadatum
toMetadatum = \case
  TxMetaNumber n -> Shelley.I n
  TxMetaBytes b -> Shelley.B b
  TxMetaText s -> Shelley.S s
  TxMetaList xs -> Shelley.List $ map toMetadatum xs
  TxMetaMap ms -> Shelley.Map $ map (both toMetadatum) ms
