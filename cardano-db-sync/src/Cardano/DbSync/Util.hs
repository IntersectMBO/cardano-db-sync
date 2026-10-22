{-# LANGUAGE BangPatterns #-}
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
  cardanoBlockSlotNo,
  getSyncStatus,
  isSyncedWithinSeconds,
  isSyncedWithintwoMinutes,
  jsonHasNul,
  jsonbNestingLimit,
  minMaxStackDepthKb,
  jsonbNestingExceeded,
  jsonNumberUnstorable,
  unstorableAsJsonb,
  logException,
  maybeFromStrict,
  maybeToStrict,
  renderByteArray,
  renderPoint,
  rewardTypeToSource,
  stripJsonNuls,
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

import Cardano.Db (RewardSource (..))
import Cardano.Db.Log (LogMessage, logError)
import Cardano.DbSync.Config.Types ()
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Shelley.Rewards as Shelley
import Cardano.Logging (Trace)
import Cardano.Prelude hiding (catch)
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Exception.Lifted (catch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, base10Exponent, coefficient)
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
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

-- | ouroboros-network catches 'SomeException' and if a 'mempty' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO LogMessage -> Text -> IO a -> IO a
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

jsonHasNul :: Aeson.Value -> Bool
jsonHasNul v =
  case v of
    Aeson.String t -> Text.any (== '\0') t
    Aeson.Array xs -> any jsonHasNul xs
    Aeson.Object o -> any entryHasNul (KeyMap.toList o)
    _otherwise -> False
  where
    entryHasNul (k, val) = Text.any (== '\0') (Key.toText k) || jsonHasNul val

stripJsonNuls :: Aeson.Value -> Aeson.Value
stripJsonNuls v =
  case v of
    Aeson.String t -> Aeson.String (stripNuls t)
    Aeson.Array xs -> Aeson.Array (stripJsonNuls <$> xs)
    Aeson.Object o -> Aeson.Object (KeyMap.fromList (stripEntry <$> KeyMap.toList o))
    _otherwise -> v
  where
    stripEntry (k, val) = (Key.fromText (stripNuls (Key.toText k)), stripJsonNuls val)
    stripNuls = Text.filter (/= '\0')

-- | How deeply nested a JSON document may be before db-sync refuses to store it as
-- @jsonb@.
jsonbNestingLimit :: Int
jsonbNestingLimit = 2000

-- | The @max_stack_depth@, in kB, a server needs to parse a value nested to
-- 'jsonbNestingLimit'. Measured at around 300 kB, rounded up for margin.
minMaxStackDepthKb :: Int
minMaxStackDepthKb = 512

-- | Whether encoded JSON nests deeper than 'jsonbNestingLimit', one level per array
-- or object, as Postgres counts it.
jsonbNestingExceeded :: ByteString -> Bool
jsonbNestingExceeded bs =
  go 0 False False 0
  where
    len = BS.length bs

    go :: Int -> Bool -> Bool -> Int -> Bool
    go !level !inString !escaped !i
      | i >= len = False
      | inString =
          if escaped
            then go level True False (i + 1)
            else case BS.index bs i of
              '\\' -> go level True True (i + 1)
              '"' -> go level False False (i + 1)
              _otherwise -> go level True False (i + 1)
      | otherwise = case BS.index bs i of
          '"' -> go level True False (i + 1)
          '{' -> deeper
          '[' -> deeper
          '}' -> go (level - 1) False False (i + 1)
          ']' -> go (level - 1) False False (i + 1)
          _otherwise -> go level False False (i + 1)
      where
        deeper
          | level + 1 > jsonbNestingLimit = True
          | otherwise = go (level + 1) False False (i + 1)

-- | Whether Postgres would reject this number as out of @numeric@ range: more than
-- 131072 digits before the decimal point, or 16383 after it.
jsonNumberUnstorable :: Scientific -> Bool
jsonNumberUnstorable sci
  | expo >= 0 = integerDigits + expo > 131072
  | otherwise = negate expo - trailingZeros > 16383
  where
    expo = base10Exponent sci
    digits = show (abs (coefficient sci)) :: [Char]
    integerDigits = length digits
    trailingZeros = length (takeWhile (== '0') (reverse digits))

-- | Why Postgres would refuse to store this value as @jsonb@, or 'Nothing' if it will
-- take it.
unstorableAsJsonb :: Aeson.Value -> Maybe Text
unstorableAsJsonb value
  | jsonHasNul value = Just "Contains a Unicode NUL character, which Postgres cannot store"
  | hasUnstorableNumber value = Just "Contains a number outside the range Postgres can store as numeric"
  | tooDeep 0 value = Just ("Nests deeper than " <> textShow jsonbNestingLimit <> " levels, which Postgres cannot parse as jsonb")
  | otherwise = Nothing
  where
    hasUnstorableNumber v =
      case v of
        Aeson.Number sci -> jsonNumberUnstorable sci
        Aeson.Array xs -> any hasUnstorableNumber xs
        Aeson.Object o -> any hasUnstorableNumber (KeyMap.elems o)
        _otherwise -> False

    tooDeep !level v
      | level > jsonbNestingLimit = True
      | otherwise =
          case v of
            Aeson.Array xs -> any (tooDeep (level + 1)) xs
            Aeson.Object o -> any (tooDeep (level + 1)) (KeyMap.elems o)
            _otherwise -> False

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
