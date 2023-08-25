{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Util.CborTest (tests) where

import Cardano.DbSync.Era.Shelley.Generic.Metadata (TxMetadataValue (..))
import Cardano.DbSync.Util.Cbor
import Cardano.Prelude
import qualified Data.ByteString as SByteString
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import qualified Data.Text as Text
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Util.CborTest"
      [ ("serialiseTxMetadataToCbor simple", prop_serialiseTxMetadataToCbor)
      , ("serialiseTxMetadataToCbor roundtrip", prop_serialiseTxMetadataToCbor_roundtrip)
      ]

prop_serialiseTxMetadataToCbor :: Property
prop_serialiseTxMetadataToCbor = property $ do
  (txData, cbor) <- forAll $ Gen.element knownTxMetadata
  cbor === serialiseTxMetadataToCbor' txData
  where
    serialiseTxMetadataToCbor' = serialiseCborToBase16 . serialiseTxMetadataToCbor

prop_serialiseTxMetadataToCbor_roundtrip :: Property
prop_serialiseTxMetadataToCbor_roundtrip = property $ do
  txData <- forAll genTxMetadata

  cover 10 "number" (any isNumber txData)
  cover 10 "bytes" (any isBytes txData)
  cover 10 "text" (any isText txData)
  cover 10 "list" (any isList txData)
  cover 10 "map" (any isMap txData)

  tripping txData serialiseTxMetadataToCbor deserialiseTxMetadataFromCbor

knownTxMetadata :: [(Map Word64 TxMetadataValue, Text)]
knownTxMetadata =
  [
    ( Map.singleton 1 (TxMetaText "tx metadata value")
    , "a101717478206d657461646174612076616c7565"
    )
  ,
    ( Map.singleton
        674
        ( TxMetaMap
            [
              ( TxMetaText "msg"
              , TxMetaList
                  [TxMetaText "Minswap: Swap Exact Out Order"]
              )
            ]
        )
    , "a11902a2a1636d736781781d4d696e737761703a2053776170204578616374204f7574204f72646572"
    )
  ,
    ( Map.singleton
        1000
        ( TxMetaBytes . Base16.decodeLenient . encodeUtf8 $
            "01da32e76ec731be1a80444acae242f9122971a077f01aa691d1ec89c8da042223c75772ad8f7a48b3068833af202a6500ab22f763dd4ef83d"
        )
    , "a11903e8583901da32e76ec731be1a80444acae242f9122971a077f01aa691d1ec89c8da042223c75772ad8f7a48b3068833af202a6500ab22f763dd4ef83d"
    )
  ,
    ( Map.singleton 1005 (TxMetaNumber 2650000)
    , "a11903ed1a00286f90"
    )
  ,
    ( Map.singleton
        41
        ( TxMetaList
            [ TxMetaMap
                [
                  ( TxMetaNumber 53
                  , TxMetaText "yes"
                  )
                ]
            ]
        )
    , "a1182981a1183563796573"
    )
  ]

genTxMetadata :: Gen (Map Word64 TxMetadataValue)
genTxMetadata = Gen.sized $ \(Size size) ->
  Map.fromList <$> Gen.list (range size) generator
  where
    range = Range.linear 0
    generator =
      (,)
        <$> Gen.word64 Range.constantBounded
        <*> genTxMetadataValue

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue = Gen.sized $ \(Size size) -> do
  Gen.frequency
    [ (1, TxMetaNumber <$> genTxMetaNumber)
    , (1, TxMetaBytes <$> genTxMetaBytes)
    , (1, TxMetaText <$> genTxMetaText)
    , (signum size, TxMetaList <$> Gen.scale (`div` 2) genTxMetaList)
    , (signum size, TxMetaMap <$> Gen.scale (`div` 2) genTxMetaMap)
    ]
  where
    genTxMetaNumber :: Gen Integer
    genTxMetaNumber =
      Gen.integral
        ( Range.linear
            (-fromIntegral (maxBound @Word64))
            (fromIntegral (maxBound @Word64))
        )

    genTxMetaBytes :: Gen SByteString.ByteString
    genTxMetaBytes =
      SByteString.pack <$> Gen.list (Range.linear 0 64) (Gen.word8 Range.constantBounded)

    genTxMetaText :: Gen Text
    genTxMetaText = Text.pack <$> Gen.list (Range.linear 0 64) Gen.alphaNum

    genTxMetaList :: Gen [TxMetadataValue]
    genTxMetaList = Gen.sized $ \(Size size) ->
      Gen.list (Range.linear 0 size) genTxMetadataValue

    genTxMetaMap :: Gen [(TxMetadataValue, TxMetadataValue)]
    genTxMetaMap = Gen.sized $ \(Size size) ->
      Gen.list (Range.linear 0 size) $
        (,) <$> genTxMetadataValue <*> genTxMetadataValue

isNumber :: TxMetadataValue -> Bool
isNumber (TxMetaNumber _) = True
isNumber _ = False

isBytes :: TxMetadataValue -> Bool
isBytes (TxMetaBytes _) = True
isBytes _ = False

isText :: TxMetadataValue -> Bool
isText (TxMetaText _) = True
isText _ = False

isList :: TxMetadataValue -> Bool
isList (TxMetaList _) = True
isList _ = False

isMap :: TxMetadataValue -> Bool
isMap (TxMetaMap _) = True
isMap _ = False

serialiseCborToBase16 :: ByteString -> Text
serialiseCborToBase16 = decodeUtf8 . Base16.encode
