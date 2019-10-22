{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Server.Util
  ( blockPosixTime
  , bsBase16Encode
  , decodeTextAddress
  , defaultPageSize
  , divRoundUp
  , k
  , runQuery
  , slotsPerEpoch
  , textBase16Decode
  , textShow
  , toPageSize
  , unflattenSlotNo
  ) where

import           Cardano.Chain.Common (Address, fromCBORTextAddress)

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word (Word16, Word64)

import           Database.Persist.Sql (SqlBackend, runSqlConn)

import           Explorer.DB (Block (..))
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Server.Types (PageSize (..))


blockPosixTime :: Block -> POSIXTime
blockPosixTime = utcTimeToPOSIXSeconds . blockTime

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


defaultPageSize :: PageSize
defaultPageSize = PageSize 10

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = (a + b - 1) `div` b

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

toPageSize :: Maybe PageSize -> PageSize
toPageSize = fromMaybe defaultPageSize

unflattenSlotNo :: Word64 -> Word16
unflattenSlotNo w = fromIntegral (w `mod` slotsPerEpoch)
