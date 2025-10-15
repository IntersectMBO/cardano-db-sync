{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.OffChain.VoteTest (tests) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Error (runOrThrowIO)
import Cardano.DbSync.OffChain.Http (parseAndValidateVoteData)
import Cardano.Prelude hiding ((%))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Hedgehog

tests :: IO Bool
tests =
  checkParallel $
    Group
      "Cardano.DbSync.OffChain.Vote"
      [ ("parseAndValidateVoteData handles invalid CIP format", prop_parseInvalidCIPFormat)
      , ("parseAndValidateVoteData handles valid JSON but invalid structure", prop_parseValidJsonInvalidStructure)
      ]

-- | Test that we can parse JSON with incorrect field types (e.g., doNotList as string instead of bool)
-- This is based on the issue https://github.com/IntersectMBO/cardano-db-sync/issues/1995
prop_parseInvalidCIPFormat :: Property
prop_parseInvalidCIPFormat = withTests 1 $ property $ do
  -- Read the test file with invalid doNotList field (string instead of bool)
  fileContent <- liftIO $ BS.readFile "../cardano-chain-gen/test/testfiles/invalid-vote-doNotList.jsonld"
  let lbsContent = LBS.fromStrict fileContent

  -- Run the parser
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData fileContent lbsContent Nothing DB.DrepAnchor Nothing

  let (mocvd, val, _hash, _warning) = result

  -- Should succeed in parsing generic JSON
  annotate "Successfully parsed as generic JSON"

  -- Should fail to parse into strongly-typed OffChainVoteData
  assert $ isNothing mocvd

  -- But should have valid Aeson.Value
  case Aeson.toJSON val of
    Aeson.Object _obj -> do
      annotate "Has valid JSON object"
      success
    _ -> do
      annotate "Expected JSON object"
      failure

-- | Test with completely valid JSON but not matching the CIP schema
prop_parseValidJsonInvalidStructure :: Property
prop_parseValidJsonInvalidStructure = property $ do
  -- Create a valid JSON that doesn't match CIP schema at all
  let invalidJson = "{\"randomField\": \"value\", \"number\": 42}"
      bs = encodeUtf8 invalidJson
      lbs = LBS.fromStrict bs

  -- This should succeed because it's valid JSON, just not matching the schema
  result <- liftIO $ runOrThrowIO $ runExceptT $ parseAndValidateVoteData bs lbs Nothing DB.DrepAnchor Nothing

  let (mocvd, _val, _hash, _warning) = result

  annotate "Successfully parsed generic JSON"
  -- Should not parse into OffChainVoteData
  assert $ isNothing mocvd
