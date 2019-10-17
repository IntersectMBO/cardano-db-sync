{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Server.Util
  ( bsBase16Encode
  , decodeTextAddress
  , k
  , runQuery
  , slotsPerEpoch
  , textBase16Decode
  , textShow
  ) where

import           Cardano.Chain.Common (Address, fromCBORTextAddress)

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word64)

import           Database.Persist.Sql (SqlBackend, runSqlConn)

import           Explorer.Web.Error (ExplorerError (..))

-- | bsBase16Encode : Convert a raw ByteString to Base16 and then encode it as Text.
bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt


decodeTextAddress :: Text -> Either ExplorerError Address
decodeTextAddress txt =
  first (const . Internal $ "Unable to decode address " <> txt <> ".")
    $ fromCBORTextAddress txt

-- TODO, get this from the config somehow
k :: Word64
k = 2160

runQuery :: MonadIO m => SqlBackend -> ReaderT SqlBackend IO a -> m a
runQuery backend query =
  liftIO $ runSqlConn query backend

slotsPerEpoch :: Word64
slotsPerEpoch = k * 10

textBase16Decode :: Text -> Either ExplorerError ByteString
textBase16Decode text = do
  case Base16.decode (Text.encodeUtf8 text) of
    (bs, "") -> Right bs
    _ -> Left $ Internal (Text.pack $ "Unable to Base16.decode " ++ show text ++ ".")

textShow :: Show a => a -> Text
textShow = Text.pack . show
