{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.Util
  ( blockPosixTime
  , bsBase16Encode
  , collapseTxGroup
  , decodeTextAddress
  , defaultPageSize
  , divRoundUp
  , genesisDistributionTxHash
  , k
  , runQuery
  , slotsPerEpoch
  , textBase16Decode
  , textShow
  , toPageSize
  , unflattenSlotNo
  , zipTxBrief
  ) where

import           Cardano.Chain.Common (Address, fromCBORTextAddress)

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word (Word16, Word64)

import           Database.Persist.Sql (IsolationLevel (..), SqlBackend, runSqlConnWithIsolation)

import           Explorer.DB (Block (..), TxId)

import           Explorer.Web.Api.Legacy.Types (PageSize (..))
import           Explorer.Web.ClientTypes (CCoin (..), CHash (..), CTxAddressBrief (..),
                    CTxBrief (..), CTxHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))

blockPosixTime :: Block -> POSIXTime
blockPosixTime = utcTimeToPOSIXSeconds . blockTime

-- | bsBase16Encode : Convert a raw ByteString to Base16 and then encode it as Text.
bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ show bs
    Right txt -> txt

collapseTxGroup :: [(TxId, a)] -> (TxId, [a])
collapseTxGroup xs =
  case xs of
    [] -> error "collapseTxGroup: groupOn produced [] on non-empty list (impossible)"
    (x:_) -> (fst x, map snd xs)

decodeTextAddress :: Text -> Either ExplorerError Address
decodeTextAddress txt =
  first (const . Internal $ "Unable to decode address " <> txt <> ".")
    $ fromCBORTextAddress txt


defaultPageSize :: PageSize
defaultPageSize = PageSize 10

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = (a + b - 1) `div` b

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

genesisDistributionTxHash :: CTxHash
genesisDistributionTxHash = CTxHash (CHash "Genesis Distribution")

-- TODO, get this from the config somehow
k :: Word64
k = 2160

runQuery :: MonadIO m => SqlBackend -> ReaderT SqlBackend IO a -> m a
runQuery backend query =
  liftIO $ runSqlConnWithIsolation query backend Serializable

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

zipTxBrief :: [(TxId, ByteString, UTCTime)] -> [(TxId, [CTxAddressBrief])] -> [(TxId, [CTxAddressBrief])] -> [CTxBrief]
zipTxBrief xs ins outs =
    mapMaybe build $ map fst3 xs
  where
    idMap :: Map TxId (ByteString, UTCTime)
    idMap = Map.fromList $ map (\(a, b, c) -> (a, (b, c))) xs

    inMap :: Map TxId [CTxAddressBrief]
    inMap = Map.fromList ins

    outMap :: Map TxId [CTxAddressBrief]
    outMap = Map.fromList outs

    build :: TxId -> Maybe CTxBrief
    build txid = do
      (hash, time) <- Map.lookup txid idMap
      inputs <- Map.lookup txid inMap
      outputs <- Map.lookup txid outMap
      inSum <- Just $ sum (map (unCCoin . ctaAmount) inputs)
      outSum <- Just $ sum (map (unCCoin . ctaAmount) outputs)
      pure $ CTxBrief
              { ctbId = CTxHash . CHash $ bsBase16Encode hash
              , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds time
              , ctbInputs = inputs
              , ctbOutputs = outputs
              , ctbInputSum = mkCCoin inSum
              , ctbOutputSum = mkCCoin outSum
              , ctbFees = mkCCoin (inSum - outSum)
              }
