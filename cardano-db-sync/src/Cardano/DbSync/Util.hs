{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Util
  ( cardanoBlockSlotNo
  , fmap3
  , getSyncStatus
  , isSyncedWithinSeconds
  , liftedLogException
  , logException
  , renderByteArray
  , renderSlotList
  , textPrettyShow
  , textShow
  , tipBlockNo
  , traverseMEither
  ) where

import           Cardano.Prelude hiding (catch)

import           Cardano.BM.Trace (Trace, logError)

import           Cardano.DbSync.Types

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Exception.Lifted (SomeException, catch)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time (diffUTCTime)

import           Text.Show.Pretty (ppShow)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.Shelley.Ledger.Block as Shelley

import           Ouroboros.Network.Block (BlockNo (..), Tip, getTipBlockNo)
import           Ouroboros.Network.Point (withOrigin)

import qualified Shelley.Spec.Ledger.BlockChain as Shelley


cardanoBlockSlotNo :: CardanoBlock -> SlotNo
cardanoBlockSlotNo blk =
  case blk of
    BlockByron (ByronBlock _bblk slot _hash) -> slot
    BlockShelley sblk -> Shelley.bheaderSlotNo $
                            Shelley.bhbody (Shelley.bheader $ Shelley.shelleyBlockRaw sblk)

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

getSyncStatus :: SlotDetails -> SyncState
getSyncStatus sd = isSyncedWithinSeconds sd 120

isSyncedWithinSeconds :: SlotDetails -> Word -> SyncState
isSyncedWithinSeconds sd target =
  -- diffUTCTime returns seconds.
  let secDiff = ceiling (diffUTCTime (sdCurrentTime sd) (sdSlotTime sd)) :: Int
  in if fromIntegral (abs secDiff) <= target
        then SyncFollowing
        else SyncLagging

textPrettyShow :: Show a => a -> Text
textPrettyShow = Text.pack . ppShow

textShow :: Show a => a -> Text
textShow = Text.pack . show

tipBlockNo :: Tip blk -> BlockNo
tipBlockNo tip = withOrigin (BlockNo 0) identity (getTipBlockNo tip)

-- | Run a function of type `a -> m (Either e ())` over a list and return
-- the first `e` or `()`.
-- TODO: Is this not just `traverse` ?
traverseMEither :: Monad m => (a -> m (Either e ())) -> [a] -> m (Either e ())
traverseMEither action xs = do
  case xs of
    [] -> pure $ Right ()
    (y:ys) ->
      action y >>= either (pure . Left) (const $ traverseMEither action ys)


-- | Needed when debugging disappearing exceptions.
liftedLogException :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
liftedLogException tracer txt action =
    action `catch` logger
  where
    logger :: MonadIO m => SomeException -> m a
    logger e =
      liftIO $ do
        putStrLn $ "Caught exception: txt " ++ show e
        logError tracer $ txt <> textShow e
        throwIO e

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action =
    action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

renderSlotList :: [SlotNo] -> Text
renderSlotList xs
  | length xs < 10 = textShow (map unSlotNo xs)
  | otherwise =
      mconcat [ "[", textShow (unSlotNo $ List.head xs), "..", textShow (unSlotNo $ List.last xs), "]" ]
