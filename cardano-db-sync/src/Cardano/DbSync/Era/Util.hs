{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Util (
  liftLookupFail,
  containsUnicodeNul,
  safeDecodeUtf8,
  safeDecodeToJson,
) where

import Cardano.BM.Trace (logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.AppT (App, askTrace)
import Cardano.DbSync.Error.Types (SyncNodeError (..))
import Cardano.Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

-- liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT SyncNodeError m a
-- liftLookupFail loc =
--   firstExceptT (\lf -> SNErrDefault $ mconcat [loc, " ", show lf]) . newExceptT

liftLookupFail :: Text -> App (Either DB.LookupFail a) -> App a
liftLookupFail loc action = do
  result <- action
  case result of
    Right val -> pure val
    Left lf -> throwError . SNErrDefault $ mconcat [loc, " ", show lf]

safeDecodeUtf8 :: ByteString -> App (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
  | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
  | otherwise = liftIO $ try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u000"

safeDecodeToJson :: Text -> ByteString -> App (Maybe Text)
safeDecodeToJson tracePrefix x = do
  tracer <- askTrace
  ejson <- safeDecodeUtf8 x
  case ejson of
    Left err -> do
      liftIO . logWarning tracer $
        mconcat
          [tracePrefix, ": Could not decode to UTF8: ", DB.textShow err]
      -- We have to insert
      pure Nothing
    Right json ->
      -- See https://github.com/IntersectMBO/cardano-db-sync/issues/297
      if containsUnicodeNul json
        then do
          liftIO $ logWarning tracer $ tracePrefix <> "was recorded as null, due to a Unicode NUL character found when trying to parse the json."
          pure Nothing
        else pure $ Just json
