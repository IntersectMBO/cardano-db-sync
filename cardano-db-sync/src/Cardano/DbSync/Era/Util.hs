{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Util (
  liftLookupFail,
  containsUnicodeNul,
  safeDecodeUtf8,
  safeDecodeToJson,
) where

import Cardano.BM.Trace (Trace, logWarning)
import qualified Cardano.Db as DB
import Cardano.DbSync.Error
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT SyncNodeError m a
liftLookupFail loc =
  firstExceptT (\lf -> SNErrDefault $ mconcat [loc, " ", show lf]) . newExceptT

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
  | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
  | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0

containsUnicodeNul :: Text -> Bool
containsUnicodeNul = Text.isInfixOf "\\u000"

safeDecodeToJson :: MonadIO m => Trace IO Text -> Text -> ByteString -> m (Maybe Text)
safeDecodeToJson tracer tracePrefix x = do
  ejson <- liftIO $ safeDecodeUtf8 x
  case ejson of
    Left err -> do
      liftIO . logWarning tracer $
        mconcat
          [tracePrefix, ": Could not decode to UTF8: ", DB.textShow err]
      -- We have to insert
      pure Nothing
    Right json ->
      -- See https://github.com/input-output-hk/cardano-db-sync/issues/297
      if containsUnicodeNul json
        then do
          liftIO $ logWarning tracer $ tracePrefix <> ": dropped due to a Unicode NUL character."
          pure Nothing
        else pure $ Just json
