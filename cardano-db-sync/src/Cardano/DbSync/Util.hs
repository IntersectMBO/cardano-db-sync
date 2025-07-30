{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Cardano.DbSync.Util (
  maxBulkSize,
  cardanoBlockSlotNo,
  getSyncStatus,
  isSyncedWithinSeconds,
  isSyncedWithintwoMinutes,
  logException,
  maybeFromStrict,
  maybeToStrict,
  renderByteArray,
  renderPoint,
  rewardTypeToSource,
  textShow,
  forth4,
  splitLast,
  whenStrictJust,
  whenStrictJustDefault,
  whenDefault,
  whenMaybe,
  mlookup,
  whenFalseEmpty,
  whenFalseMempty,
) where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Db (RewardSource (..))
import Cardano.DbSync.Config.Types ()
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import Cardano.Prelude hiding (catch)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Exception.Lifted (catch)
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text.Encoding as Text
import qualified Data.Time.Clock as Time
import Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import Ouroboros.Consensus.Cardano.CanHardFork ()
import Ouroboros.Consensus.Protocol.Praos ()
import Ouroboros.Consensus.Shelley.HFEras ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (blockSlot, getPoint)
import qualified Ouroboros.Network.Point as Point

maxBulkSize :: Int
maxBulkSize = 40000

cardanoBlockSlotNo :: Consensus.CardanoBlock StandardCrypto -> SlotNo
cardanoBlockSlotNo = blockSlot

isSyncedWithinSeconds :: SlotDetails -> Word -> SyncState
isSyncedWithinSeconds sd target =
  -- diffUTCTime returns seconds.
  let secDiff = ceiling (Time.diffUTCTime (sdCurrentTime sd) (sdSlotTime sd)) :: Int
   in if fromIntegral (abs secDiff) <= target
        then SyncFollowing
        else SyncLagging

getSyncStatus :: SlotDetails -> SyncState
getSyncStatus sd = isSyncedWithinSeconds sd 120

isSyncedWithintwoMinutes :: SlotDetails -> Bool
isSyncedWithintwoMinutes sd = isSyncedWithinSeconds sd 120 == SyncFollowing

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

whenStrictJustDefault :: Applicative m => b -> Strict.Maybe a -> (a -> m b) -> m b
whenStrictJustDefault b ma f =
  case ma of
    Strict.Nothing -> pure b
    Strict.Just a -> f a

whenDefault :: Applicative m => a -> Bool -> m a -> m a
whenDefault a bl ma = if bl then ma else pure a

whenMaybe :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenMaybe (Just a) f = Just <$> f a
whenMaybe Nothing _f = pure Nothing

forth4 :: (a, b, c, d) -> d
forth4 (_, _, _, d) = d

splitLast :: [(a, b, c, d)] -> ([(a, b, c)], [d])
splitLast = unzip . fmap (\(a, b, c, d) -> ((a, b, c), d))

mlookup :: Ord k => Maybe k -> Map k a -> Maybe a
mlookup mKey mp = (`Map.lookup` mp) =<< mKey

whenFalseEmpty :: Applicative m => Bool -> a -> m a -> m a
whenFalseEmpty flag a mkAs =
  if flag then mkAs else pure a

whenFalseMempty :: (Monoid a, Applicative m) => Bool -> m a -> m a
whenFalseMempty flag mkAs =
  if flag then mkAs else pure mempty
