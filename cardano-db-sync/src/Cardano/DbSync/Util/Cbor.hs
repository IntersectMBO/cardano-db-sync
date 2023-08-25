module Cardano.DbSync.Util.Cbor (
  deserialiseTxMetadataFromCbor,
  serialiseTxMetadataToCbor,
) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..))
import Cardano.Ledger.Binary.Decoding (DecoderError (..), decodeFull')
import Cardano.Ledger.Binary.Encoding (serialize')
import Cardano.Ledger.Binary.Version (shelleyProtVer)
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import Cardano.Prelude
import Prelude ()

serialiseTxMetadataToCbor :: Map Word64 TxMetadataValue -> ByteString
serialiseTxMetadataToCbor = serialize' shelleyProtVer . map toShelleyMetadatum
  where
    toShelleyMetadatum :: TxMetadataValue -> Metadatum
    toShelleyMetadatum (TxMetaNumber n) = I n
    toShelleyMetadatum (TxMetaBytes b) = B b
    toShelleyMetadatum (TxMetaText s) = S s
    toShelleyMetadatum (TxMetaList xs) = List $ map toShelleyMetadatum xs
    toShelleyMetadatum (TxMetaMap ms) =
      Map $
        map (bimapBoth toShelleyMetadatum) ms

deserialiseTxMetadataFromCbor :: ByteString -> Either DecoderError (Map Word64 TxMetadataValue)
deserialiseTxMetadataFromCbor =
  (map fromShelleyMetadatum <$>) . decodeFull' shelleyProtVer
  where
    fromShelleyMetadatum :: Metadatum -> TxMetadataValue
    fromShelleyMetadatum (I n) = TxMetaNumber n
    fromShelleyMetadatum (B b) = TxMetaBytes b
    fromShelleyMetadatum (S s) = TxMetaText s
    fromShelleyMetadatum (List xs) = TxMetaList $ map fromShelleyMetadatum xs
    fromShelleyMetadatum (Map ms) =
      TxMetaMap $
        map (bimapBoth fromShelleyMetadatum) ms

bimapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
bimapBoth f = bimap f f
