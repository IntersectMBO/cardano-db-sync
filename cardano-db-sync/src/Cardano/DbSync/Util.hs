{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Util (
  cardanoBlockSlotNo,
  fmap3,
  getSyncStatus,
  isSyncedWithinSeconds,
  liftedLogException,
  logActionDuration,
  logException,
  maybeFromStrict,
  maybeToStrict,
  nullMetricSetters,
  plusCoin,
  renderByteArray,
  renderPoint,
  renderSlotList,
  rewardTypeToSource,
  textPrettyShow,
  textShow,
  third,
  thrd3,
  forth4,
  splitLast,
  traverseMEither,
  whenStrictJust,
  whenMaybe,
  mlookup,
  whenRight,
  whenFalseEmpty,
  whenFalseMempty,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import Cardano.Db (RewardSource (..), textShow)
import Cardano.DbSync.Config.Types ()
import Cardano.DbSync.Types
import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import Cardano.Prelude hiding (catch)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Exception.Lifted (catch)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock as Time
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (blockSlot, getPoint)
import qualified Ouroboros.Network.Point as Point
import Text.Show.Pretty (ppShow)

cardanoBlockSlotNo :: CardanoBlock -> SlotNo
cardanoBlockSlotNo = blockSlot

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

getSyncStatus :: SlotDetails -> SyncState
getSyncStatus sd = isSyncedWithinSeconds sd 120

isSyncedWithinSeconds :: SlotDetails -> Word -> SyncState
isSyncedWithinSeconds sd target =
  -- diffUTCTime returns seconds.
  let secDiff = ceiling (Time.diffUTCTime (sdCurrentTime sd) (sdSlotTime sd)) :: Int
   in if fromIntegral (abs secDiff) <= target
        then SyncFollowing
        else SyncLagging

textPrettyShow :: Show a => a -> Text
textPrettyShow = Text.pack . ppShow

-- | Run a function of type `a -> m (Either e ())` over a list and return
-- the first `e` or `()`.
-- TODO: Is this not just `traverse` ?
traverseMEither :: Monad m => (a -> m (Either e ())) -> [a] -> m (Either e ())
traverseMEither action xs = do
  case xs of
    [] -> pure $ Right ()
    (y : ys) ->
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

-- | Log the runtime duration of an action. Mainly for debugging.
logActionDuration :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
logActionDuration tracer label action = do
  before <- liftIO Time.getCurrentTime
  a <- action
  after <- liftIO Time.getCurrentTime
  liftIO . logInfo tracer $ mconcat [label, ": duration ", textShow (Time.diffUTCTime after before)]
  pure a

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

-- | Eequired for testing or when disabling the metrics.
nullMetricSetters :: MetricSetters
nullMetricSetters =
  MetricSetters
    { metricsSetNodeBlockHeight = const $ pure ()
    , metricsSetDbQueueLength = const $ pure ()
    , metricsSetDbBlockHeight = const $ pure ()
    , metricsSetDbSlotHeight = const $ pure ()
    }

plusCoin :: Coin -> Coin -> Coin
plusCoin (Coin a) (Coin b) = Coin (a + b)

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

renderPoint :: CardanoPoint -> Text
renderPoint point =
  case getPoint point of
    Origin -> "genesis"
    At blk ->
      mconcat
        [ "slot "
        , textShow (unSlotNo $ Point.blockPointSlot blk)
        , ", hash "
        , renderByteArray $ toRawHash (Proxy @CardanoBlock) (Point.blockPointHash blk)
        ]

renderSlotList :: [SlotNo] -> Text
renderSlotList xs
  | length xs < 10 = textShow (map unSlotNo xs)
  | otherwise =
      mconcat ["[", textShow (unSlotNo $ List.head xs), "..", textShow (unSlotNo $ List.last xs), "]"]

rewardTypeToSource :: Shelley.RewardType -> RewardSource
rewardTypeToSource rt =
  case rt of
    Shelley.LeaderReward -> RwdLeader
    Shelley.MemberReward -> RwdMember

maybeFromStrict :: Strict.Maybe a -> Maybe a
maybeFromStrict Strict.Nothing = Nothing
maybeFromStrict (Strict.Just a) = Just a

maybeToStrict :: Maybe a -> Strict.Maybe a
maybeToStrict Nothing = Strict.Nothing
maybeToStrict (Just a) = Strict.Just a

whenStrictJust :: Applicative m => Strict.Maybe a -> (a -> m ()) -> m ()
whenStrictJust ma f =
  case ma of
    Strict.Nothing -> pure ()
    Strict.Just a -> f a

whenMaybe :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenMaybe (Just a) f = Just <$> f a
whenMaybe Nothing _f = pure Nothing

third :: (a, b, c) -> c
third (_, _, c) = c

thrd3 :: (a, b, c, d) -> c
thrd3 (_, _, c, _) = c

forth4 :: (a, b, c, d) -> d
forth4 (_, _, _, d) = d

splitLast :: [(a, b, c, d)] -> ([(a, b, c)], [d])
splitLast = unzip . fmap (\(a, b, c, d) -> ((a, b, c), d))

mlookup :: Ord k => Maybe k -> Map k a -> Maybe a
mlookup mKey mp = (`Map.lookup` mp) =<< mKey

whenRight :: Applicative m => Either e a -> (a -> m ()) -> m ()
whenRight ma f =
  case ma of
    Right a -> f a
    Left _ -> pure ()

whenFalseEmpty :: Applicative m => Bool -> a -> m a -> m a
whenFalseEmpty flag a mkAs =
  if flag then mkAs else pure a

whenFalseMempty :: (Monoid a, Applicative m) => Bool -> m a -> m a
whenFalseMempty flag mkAs =
  if flag then mkAs else pure mempty
