{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Util.Bech32 (
  serialiseToBech32,
  deserialiseFromBech32,
  serialiseVerKeyVrfToBech32,
  deserialiseVerKeyVrfFromBech32,
  serialiseStakePoolKeyHashToBech32,
  deserialiseStakePoolKeyHashFromBech32,
) where

import Cardano.Crypto.Hash.Class (hashFromBytes, hashToBytes)
import Cardano.Crypto.VRF.Class (rawDeserialiseVerKeyVRF, rawSerialiseVerKeyVRF)
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..), VerKeyVRF ())
import Cardano.Prelude
import Codec.Binary.Bech32
import Prelude (id)

-- | Wrap Bech32 deserialisation errors
data DecodeError
  = -- | Error decoding bech32 to text
    Bech32DecodingError !DecodingError
  | -- | Error extracting bytes from text
    DataPartToBytesError
  | -- | Error decoding raw bytes
    DecodeFromRawBytesError
  deriving (Eq, Show)

instance Exception DecodeError

-- | Serialise a ByteString to a bech32 address with the given human-readable prefix
serialiseToBech32 :: Text -> ByteString -> Text
serialiseToBech32 prefix bytes = encodeLenient humanReadablePart dataPart
  where
    humanReadablePart =
      either (panic . show) id $
        humanReadablePartFromText prefix
    dataPart = dataPartFromBytes bytes

-- | Deserialise a bech32 address to a ByteString
deserialiseFromBech32 :: Text -> Either DecodeError ByteString
deserialiseFromBech32 s = decodeLenient' s >>= dataPartToBytes'
  where
    decodeLenient' = bimap Bech32DecodingError snd . decodeLenient
    dataPartToBytes' d = maybeToEither DataPartToBytesError $ dataPartToBytes d

-- | Serialise a Verification Key to bech32 address
serialiseVerKeyVrfToBech32 :: VerKeyVRF StandardCrypto -> Text
serialiseVerKeyVrfToBech32 =
  serialiseToBech32 "vrf_vk" . rawSerialiseVerKeyVRF

-- | Deserialise a bech32 address to a Verification Key
deserialiseVerKeyVrfFromBech32 :: Text -> Either DecodeError (VerKeyVRF StandardCrypto)
deserialiseVerKeyVrfFromBech32 text =
  deserialiseFromBech32 text >>= deserialiseFromRawBytes'
  where
    deserialiseFromRawBytes' :: ByteString -> Either DecodeError (VerKeyVRF StandardCrypto)
    deserialiseFromRawBytes' =
      maybeToRight DecodeFromRawBytesError . rawDeserialiseVerKeyVRF

-- | Serialise stake pool key hash to a bech32 address
serialiseStakePoolKeyHashToBech32 :: KeyHash 'StakePool StandardCrypto -> Text
serialiseStakePoolKeyHashToBech32 (KeyHash hash) =
  serialiseToBech32 "pool" $ hashToBytes hash

-- | Deserialise a bech32 address to a stake pool key hash
deserialiseStakePoolKeyHashFromBech32 ::
  Text ->
  Either DecodeError (KeyHash 'StakePool StandardCrypto)
deserialiseStakePoolKeyHashFromBech32 text =
  deserialiseFromBech32 text >>= deserialiseFromRawBytes'
  where
    deserialiseFromRawBytes' ::
      ByteString ->
      Either DecodeError (KeyHash 'StakePool StandardCrypto)
    deserialiseFromRawBytes' bytes = maybeToRight DecodeFromRawBytesError $ hashFromBytes' bytes

    hashFromBytes' :: ByteString -> Maybe (KeyHash 'StakePool StandardCrypto)
    hashFromBytes' bytes = KeyHash <$> hashFromBytes bytes
