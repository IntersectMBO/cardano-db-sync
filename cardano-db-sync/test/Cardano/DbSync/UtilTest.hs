{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.UtilTest (tests) where

import Cardano.Db (sanitiseUrl)
import Cardano.DbSync.Util (jsonHasNul, stripJsonNuls)
import Cardano.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
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
    genPair = (,) <$> (Key.fromText <$> genStr) <*> genValue genStr
