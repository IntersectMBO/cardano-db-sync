{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.UtilTest (tests) where

import Cardano.Db (sanitiseUrl)
import Cardano.DbSync.Era.Util (containsUnicodeNul)
import Cardano.DbSync.Util (
  jsonHasNul,
  jsonNumberUnstorable,
  jsonbNestingExceeded,
  jsonbNestingLimit,
  stripJsonNuls,
  unstorableAsJsonb,
 )
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude ()

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.Util"
      [ ("sanitiseUrl encodes NUL as %00", prop_sanitiseUrl_encodes)
      , ("sanitiseUrl leaves NUL-free text unchanged", prop_sanitiseUrl_clean_unchanged)
      , ("sanitiseUrl output never contains NUL", prop_sanitiseUrl_no_nul)
      , ("sanitiseUrl preserves distinctness of NUL-bearing URLs", prop_sanitiseUrl_distinct)
      , ("sanitiseUrl collides a raw NUL with a literal %00", prop_sanitiseUrl_known_collision)
      , ("jsonHasNul finds NUL in strings, keys and nested values", prop_jsonHasNul_finds)
      , ("jsonHasNul is False for NUL-free JSON", prop_jsonHasNul_clean)
      , ("stripJsonNuls removes every NUL", prop_stripJsonNuls_no_nul)
      , ("stripJsonNuls leaves NUL-free JSON unchanged", prop_stripJsonNuls_clean_unchanged)
      , ("encoded stripJsonNuls has no \\u0000 escape", prop_stripJsonNuls_encoding_safe)
      , ("encoding a NUL does produce the escape Postgres rejects", prop_nul_encodes_to_escape)
      , ("containsUnicodeNul matches only the NUL escape", prop_containsUnicodeNul_only_nul)
      , ("jsonbNestingExceeded accepts a document at the limit", prop_nesting_at_limit)
      , ("jsonbNestingExceeded rejects one level past the limit", prop_nesting_past_limit)
      , ("jsonbNestingExceeded ignores brackets inside strings", prop_nesting_ignores_strings)
      , ("jsonbNestingExceeded accepts ordinary JSON", prop_nesting_clean)
      , ("jsonNumberUnstorable follows Postgres numeric range", prop_number_range)
      , ("jsonNumberUnstorable is False for ordinary numbers", prop_number_clean)
      , ("unstorableAsJsonb names the limit that was hit", prop_unstorable_reasons)
      , ("unstorableAsJsonb accepts storable JSON", prop_unstorable_clean)
      , ("stripJsonNuls removes the reason a NUL is refused", prop_strip_then_storable)
      ]

-- -----------------------------------------------------------------------------
-- sanitiseUrl

prop_sanitiseUrl_encodes :: Property
prop_sanitiseUrl_encodes = withTests 1 $
  property $ do
    sanitiseUrl "http://example.com/a\0b" === "http://example.com/a%00b"
    sanitiseUrl "\0" === "%00"
    sanitiseUrl "\0\0" === "%00%00"
    sanitiseUrl "" === ""
    -- An already percent-encoded NUL is left alone, so it is not double encoded.
    sanitiseUrl "http://example.com/a%00b" === "http://example.com/a%00b"

prop_sanitiseUrl_clean_unchanged :: Property
prop_sanitiseUrl_clean_unchanged = property $ do
  txt <- forAll genCleanText
  sanitiseUrl txt === txt

prop_sanitiseUrl_no_nul :: Property
prop_sanitiseUrl_no_nul = property $ do
  txt <- forAll genNulText
  assert . not . Text.any (== '\0') $ sanitiseUrl txt

-- | Why percent encoding rather than stripping: stripping would map every
-- NUL-bearing URL onto its NUL-free twin, collapsing distinct rows under
-- @unique_voting_anchor (data_hash, url, type)@. Encoding keeps them apart.
prop_sanitiseUrl_distinct :: Property
prop_sanitiseUrl_distinct = property $ do
  prefix <- forAll genCleanText
  suffix <- forAll genCleanText
  let withNul = prefix <> "\0" <> suffix
      without = prefix <> suffix
  -- Stripping would make these equal; encoding must not.
  sanitiseUrl withNul /== sanitiseUrl without

-- | The one collision percent encoding does accept, recorded so it is a known
-- limitation rather than an assumed property: a URL containing a literal @%00@
-- is indistinguishable from one containing a raw NUL. Reaching it needs a
-- crafted pair sharing @data_hash@ and @type@, and the outcome is a shared
-- anchor row rather than a failed insert.
prop_sanitiseUrl_known_collision :: Property
prop_sanitiseUrl_known_collision =
  withTests 1 $
    property $
      sanitiseUrl "http://example.com/a\0b" === sanitiseUrl "http://example.com/a%00b"

-- -----------------------------------------------------------------------------
-- jsonHasNul

prop_jsonHasNul_finds :: Property
prop_jsonHasNul_finds = withTests 1 $
  property $ do
    assert . jsonHasNul $ Aeson.String "a\0b"
    assert . jsonHasNul $ Aeson.toJSON [Aeson.String "a\0b"]
    assert . jsonHasNul $ Aeson.object [("k", Aeson.String "a\0b")]
    -- A NUL in a key must be caught too, not just in a value.
    assert . jsonHasNul $ Aeson.object [(Key.fromText "k\0", Aeson.String "clean")]
    -- Nested several levels down.
    assert . jsonHasNul $
      Aeson.object [("outer", Aeson.toJSON [Aeson.object [("inner", Aeson.String "\0")]])]
    -- Non-string leaves cannot carry a NUL.
    assert . not . jsonHasNul $ Aeson.object [("k", Aeson.Number 1)]
    assert . not . jsonHasNul $ Aeson.Null

prop_jsonHasNul_clean :: Property
prop_jsonHasNul_clean = property $ do
  val <- forAll genCleanValue
  assert . not $ jsonHasNul val

-- -----------------------------------------------------------------------------
-- stripJsonNuls

prop_stripJsonNuls_no_nul :: Property
prop_stripJsonNuls_no_nul = property $ do
  val <- forAll genNulValue
  assert . not . jsonHasNul $ stripJsonNuls val

prop_stripJsonNuls_clean_unchanged :: Property
prop_stripJsonNuls_clean_unchanged = property $ do
  val <- forAll genCleanValue
  stripJsonNuls val === val

-- | The invariant that actually matters: Postgres @jsonb@ rejects @\\u0000@ even
-- as an escape sequence, so the encoded form must not contain it. Checked on the
-- encoding rather than via 'jsonHasNul' so the two are not each other's oracle.
prop_stripJsonNuls_encoding_safe :: Property
prop_stripJsonNuls_encoding_safe = property $ do
  val <- forAll genNulValue
  let encoded = encodeToText (stripJsonNuls val)
  annotateShow encoded
  assert . not $ Text.isInfixOf "\\u0000" encoded

-- | Guards the premise of the fix: without stripping, a NUL really does encode
-- to the escape that Postgres refuses. If aeson ever stopped doing this, the
-- test above would pass for the wrong reason.
prop_nul_encodes_to_escape :: Property
prop_nul_encodes_to_escape =
  withTests 1 $
    property $
      assert . Text.isInfixOf "\\u0000" . encodeToText $
        Aeson.String "a\0b"

-- -----------------------------------------------------------------------------
-- jsonb storability

prop_containsUnicodeNul_only_nul :: Property
prop_containsUnicodeNul_only_nul = withTests 1 $ property $ do
  assert $ containsUnicodeNul "{\"a\":\"\\u0000\"}"
  assert . not $ containsUnicodeNul "{\"a\":\"\\u0001\"}"
  assert . not $ containsUnicodeNul "{\"a\":\"\\u000F\"}"
  assert . not $ containsUnicodeNul "{\"a\":\"\\u000a\"}"
  assert . not $ containsUnicodeNul "{\"a\":\"plain\"}"

-- | Nested arrays at exactly @jsonbNestingLimit@ levels.
nestedArrays :: Int -> ByteString
nestedArrays n = TextEnc.encodeUtf8 (Text.replicate n "[" <> "1" <> Text.replicate n "]")

prop_nesting_at_limit :: Property
prop_nesting_at_limit = withTests 1 $ property $ do
  assert . not $ jsonbNestingExceeded (nestedArrays jsonbNestingLimit)

prop_nesting_past_limit :: Property
prop_nesting_past_limit = withTests 1 $ property $ do
  assert $ jsonbNestingExceeded (nestedArrays (jsonbNestingLimit + 1))
  -- and well past it, as a hostile datum would be
  assert $ jsonbNestingExceeded (nestedArrays (jsonbNestingLimit * 10))

-- | A bracket inside a string is data, not nesting; an escaped quote does not end it.
prop_nesting_ignores_strings :: Property
prop_nesting_ignores_strings = withTests 1 $ property $ do
  let brackets = TextEnc.encodeUtf8 ("[\"" <> Text.replicate (jsonbNestingLimit * 2) "[" <> "\"]")
  assert . not $ jsonbNestingExceeded brackets
  let escapedQuote = TextEnc.encodeUtf8 ("[\"\\\"" <> Text.replicate (jsonbNestingLimit * 2) "[" <> "\"]")
  assert . not $ jsonbNestingExceeded escapedQuote

prop_nesting_clean :: Property
prop_nesting_clean = property $ do
  val <- forAll genCleanValue
  assert . not . jsonbNestingExceeded . LBS.toStrict $ Aeson.encode val

-- | Postgres @numeric@ holds at most 131072 digits before the point and 16383 after.
-- Built with 'Sci.scientific', never written as @1e131073@: a decimal literal goes
-- through 'fromRational', which expands the power of ten.
prop_number_range :: Property
prop_number_range = withTests 1 $ property $ do
  assert $ jsonNumberUnstorable (Sci.scientific 1 131073)
  assert . not $ jsonNumberUnstorable (Sci.scientific 1 131071)
  assert $ jsonNumberUnstorable (Sci.scientific 1 1000000000)
  assert $ jsonNumberUnstorable (Sci.scientific 1 (-16384))
  assert . not $ jsonNumberUnstorable (Sci.scientific 1 (-16383))
  -- A negative exponent must not excuse the weight limit: these have 131073, 131072
  -- and 114690 digits before the point respectively.
  assert $ jsonNumberUnstorable (Sci.scientific bigCoefficient (-1))
  assert . not $ jsonNumberUnstorable (Sci.scientific bigCoefficient (-2))
  assert $ jsonNumberUnstorable (Sci.scientific bigCoefficient (-16384))
  where
    -- 131074 ones. Not divisible by 10, so no exact representation of a value
    -- scaled from it can normalise the exponent back to zero.
    bigCoefficient :: Integer
    bigCoefficient = (10 ^ (131074 :: Int) - 1) `div` 9

prop_number_clean :: Property
prop_number_clean = property $ do
  n <- forAll (Gen.integral (Range.linearFrom 0 (-1000000) 1000000))
  assert . not $ jsonNumberUnstorable (fromIntegral (n :: Int))

prop_unstorable_reasons :: Property
prop_unstorable_reasons = withTests 1 $ property $ do
  let reasonFor = unstorableAsJsonb
  reasonHas "NUL" (reasonFor (Aeson.String "a\0b"))
  reasonHas "numeric" (reasonFor (Aeson.Number (Sci.scientific 1 131073)))
  reasonHas "Nests deeper" (reasonFor (nestedValue (jsonbNestingLimit + 1)))
  where
    reasonHas needle mReason =
      case mReason of
        Nothing -> annotate ("expected a reason mentioning " <> Text.unpack needle) >> failure
        Just reason -> do
          annotate (Text.unpack reason)
          assert $ Text.isInfixOf needle reason

prop_unstorable_clean :: Property
prop_unstorable_clean = property $ do
  val <- forAll genCleanValue
  unstorableAsJsonb val === Nothing

-- | What 'safeEncodeToJson' relies on: stripping removes the NUL reason.
prop_strip_then_storable :: Property
prop_strip_then_storable = property $ do
  val <- forAll genNulValue
  case unstorableAsJsonb (stripJsonNuls val) of
    Nothing -> success
    Just reason -> do
      annotate (Text.unpack reason)
      assert . not $ Text.isInfixOf "NUL" reason

-- | A 'Aeson.Value' nested @n@ levels deep.
nestedValue :: Int -> Aeson.Value
nestedValue n = foldr (\_ acc -> Aeson.toJSON [acc]) (Aeson.Number 1) [1 .. n]

-- -----------------------------------------------------------------------------
-- Helpers

encodeToText :: Aeson.Value -> Text
encodeToText = TextEnc.decodeUtf8 . LBS.toStrict . Aeson.encode

-- | Text that never contains a NUL.
genCleanText :: Gen Text
genCleanText = Gen.text (Range.linear 0 24) (Gen.filter (/= '\0') Gen.unicode)

-- | Text that is biased towards containing at least one NUL.
genNulText :: Gen Text
genNulText =
  Gen.text (Range.linear 1 24) $
    Gen.frequency [(1, pure '\0'), (3, Gen.filter (/= '\0') Gen.unicode)]

genCleanValue :: Gen Aeson.Value
genCleanValue = genValue genCleanText

genNulValue :: Gen Aeson.Value
genNulValue = genValue genNulText

genValue :: Gen Text -> Gen Aeson.Value
genValue genStr =
  Gen.recursive
    Gen.choice
    [ Aeson.String <$> genStr
    , pure Aeson.Null
    , Aeson.Bool <$> Gen.bool
    ]
    [ Aeson.toJSON <$> Gen.list (Range.linear 0 3) (genValue genStr)
    , Aeson.object <$> Gen.list (Range.linear 0 3) genPair
    ]
  where
    genPair = (,) . Key.fromText <$> genStr <*> genValue genStr
