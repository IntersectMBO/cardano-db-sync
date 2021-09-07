{-# LANGUAGE NoImplicitPrelude #-}
module Cardano.DbSync.Era.Util
    ( liftLookupFail
    , safeDecodeUtf8
    ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import qualified Cardano.Db as DB

import           Cardano.Sync.Error

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

liftLookupFail :: Monad m => Text -> m (Either DB.LookupFail a) -> ExceptT SyncNodeError m a
liftLookupFail loc =
  firstExceptT (\lf -> NEError $ loc <> DB.renderLookupFail lf) . newExceptT

safeDecodeUtf8 :: ByteString -> IO (Either Text.UnicodeException Text)
safeDecodeUtf8 bs
    | BS.any isNullChar bs = pure $ Left (Text.DecodeError (BS.unpack bs) (Just 0))
    | otherwise = try $ evaluate (Text.decodeUtf8With Text.strictDecode bs)
  where
    isNullChar :: Char -> Bool
    isNullChar ch = ord ch == 0
