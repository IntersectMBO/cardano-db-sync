{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Util (
  containsUnicodeNul,
  safeDecodeUtf8,
  safeDecodeToJson,
  safeEncodeToJson,
) where

import Control.Concurrent.Class.MonadSTM.Strict (modifyTVar)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Cardano.Db.Log (logWarning)
import Cardano.Prelude

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..), UnicodeNullSource)
import Cardano.DbSync.Util (jsonbNestingExceeded, stripJsonNuls, unstorableAsJsonb)

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
  | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
  | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

-- | Whether the encoded JSON carries an escaped NUL, which Postgres rejects as @jsonb@.
containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u0000"

safeDecodeToJson :: MonadIO m => SyncEnv -> UnicodeNullSource -> DB.TxId -> ByteString -> m (Maybe Text)
safeDecodeToJson syncEnv source txId jsonBs = do
  ejson <- liftIO $ safeDecodeUtf8 jsonBs
  case ejson of
    Left err -> do
      liftIO . logWarning (getTrace syncEnv) $
        mconcat
          [show source, ": Could not decode to UTF8: ", textShow err]
      pure Nothing
    Right json
      -- See https://github.com/IntersectMBO/cardano-db-sync/issues/297
      | containsUnicodeNul json -> do
          addUnicodeNullToStats syncEnv source txId
          pure Nothing
      | jsonbNestingExceeded jsonBs -> pure Nothing
      | otherwise -> pure $ Just json

-- | Encode a value db-sync renders itself, for a jsonb column that cannot be NULL.
-- A NUL is stripped; anything still refused becomes the JSON @null@ literal.
safeEncodeToJson :: MonadIO m => SyncEnv -> Text -> Aeson.Value -> m Text
safeEncodeToJson syncEnv column value
  | Just reason <- unstorableAsJsonb stripped = do
      liftIO . logWarning (getTrace syncEnv) $
        mconcat [column, ": ", reason, ". Storing the JSON null literal in its place."]
      pure "null"
  | otherwise = pure (encodeToText stripped)
  where
    stripped = stripJsonNuls value
    encodeToText = Text.decodeUtf8 . LBS.toStrict . Aeson.encode

-- | Add a Unicode null character to the epoch statistics.
addUnicodeNullToStats :: MonadIO m => SyncEnv -> UnicodeNullSource -> DB.TxId -> m ()
addUnicodeNullToStats syncEnv source txId = liftIO $ do
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats
      { elsUnicodeNull =
          Map.insertWith (++) source [txId] (elsUnicodeNull epochStats)
      }
