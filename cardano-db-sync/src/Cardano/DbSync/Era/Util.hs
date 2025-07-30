{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Util (
  containsUnicodeNul,
  safeDecodeUtf8,
  safeDecodeToJson,
) where

import Control.Concurrent.Class.MonadSTM.Strict (modifyTVar)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Cardano.BM.Trace (logWarning)
import Cardano.Prelude

import qualified Cardano.Db as DB
import Cardano.DbSync.Api (getTrace)
import Cardano.DbSync.Api.Types (EpochStatistics (..), SyncEnv (..), UnicodeNullSource)

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
  | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
  | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u000"

safeDecodeToJson :: MonadIO m => SyncEnv -> UnicodeNullSource -> DB.TxId -> ByteString -> m (Maybe Text)
safeDecodeToJson syncEnv source txId jsonBs = do
  ejson <- liftIO $ safeDecodeUtf8 jsonBs
  case ejson of
    Left err -> do
      liftIO . logWarning (getTrace syncEnv) $
        mconcat
          [show source, ": Could not decode to UTF8: ", textShow err]
      pure Nothing
    Right json ->
      if containsUnicodeNul json
        then do
          -- See https://github.com/IntersectMBO/cardano-db-sync/issues/297
          addUnicodeNullToStats syncEnv source txId
          pure Nothing
        else pure $ Just json

-- | Add a Unicode null character to the epoch statistics.
addUnicodeNullToStats :: MonadIO m => SyncEnv -> UnicodeNullSource -> DB.TxId -> m ()
addUnicodeNullToStats syncEnv source txId = liftIO $ do
  atomically $ modifyTVar (envEpochStatistics syncEnv) $ \epochStats ->
    epochStats
      { elsUnicodeNull =
          Map.insertWith (++) source [txId] (elsUnicodeNull epochStats)
      }
