{-# LANGUAGE OverloadedStrings #-}

-- | Multi-asset (native token) support shared by the reports.
module Cardano.DbTool.Report.Asset (
  AssetFilter (..),
  AssetQuantity (..),
  filterAssets,
  hasKnownDecimals,
  includesAssets,
  knownAssets,
  queryAssetBalances,
  rawQuantityNote,
  renderAssetName,
  renderAssetQuantity,
  sortAssetsByAmount,
) where

import Cardano.Db (TxOutVariantType)
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display (shortenBech32)
import Cardano.Prelude (textShow)
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Which multi-assets (native tokens) a report includes.
data AssetFilter
  = -- | No multi-assets, only ADA.
    NoAssets
  | -- | Only the tokens listed in 'knownAssets'.
    KnownAssets
  | -- | All multi-assets.
    AllAssets
  deriving (Eq)

-- | Whether a report includes any multi-assets (and so an 'asset' column).
includesAssets :: AssetFilter -> Bool
includesAssets assetFilter = assetFilter /= NoAssets

-- | Keep the asset quantities selected by the filter.
filterAssets :: AssetFilter -> [AssetQuantity] -> [AssetQuantity]
filterAssets assetFilter =
  case assetFilter of
    NoAssets -> const []
    KnownAssets -> filter hasKnownDecimals
    AllAssets -> id

-- | A quantity of a multi-asset: the net movement in a transaction (positive if received by the
-- stake address, negative if sent from it) or the balance held by a stake address.
data AssetQuantity = AssetQuantity
  { aqFingerprint :: !Text
  , aqName :: !ByteString
  , aqQuantity :: !Integer
  }
  deriving (Eq)

-- | The tokens of interest, keyed by asset fingerprint, with the name to display for each and
-- its number of decimal places. Token decimals are not recorded on chain (they are published
-- off-chain by the token issuer), and the on-chain asset names are not always the names the
-- tokens are known by, so both are listed here. Other tokens are shown with their on-chain name
-- and raw on-chain quantity.
knownAssets :: Map Text (Text, Int)
knownAssets =
  Map.fromList
    [ ("asset1wd3llgkhsw6etxf2yca6cgk9ssrpva3wf0pq9a", ("NIGHT", 6))
    , ("asset16fq594uun90f2jajmecjcdt4jnsnq7r3jdqsw5", ("USDA", 6))
    , ("asset15f3ymkjafxxeunv5gtdl54g5qs8ty9k84tq94x", ("DJED", 6))
    , ("asset12ffdj8kk2w485sr7a5ekmjjdyecz8ps2cm5zed", ("USDM", 6))
    ]

hasKnownDecimals :: AssetQuantity -> Bool
hasKnownDecimals aq = Map.member (aqFingerprint aq) knownAssets

-- | The number of decimal places of a known token.
knownDecimals :: AssetQuantity -> Maybe Int
knownDecimals aq = snd <$> Map.lookup (aqFingerprint aq) knownAssets

-- | The note to print under a table that shows asset quantities without known decimals.
rawQuantityNote :: Text
rawQuantityNote =
  "Asset quantities are raw on-chain amounts (token decimals are not applied) unless the token's decimals are known."

-- | The quantity, with the token's decimal places applied if they are known, eg 1123456 is
-- rendered as "1.123456" for a token with 6 decimal places.
renderAssetQuantity :: AssetQuantity -> Text
renderAssetQuantity aq =
  sign
    <> case knownDecimals aq of
      Just decimals
        | decimals > 0 ->
            let (whole, frac) = quantity `divMod` (10 ^ decimals)
             in textShow whole <> "." <> Text.justifyRight decimals '0' (textShow frac)
      _otherwise -> textShow quantity
  where
    quantity = abs (aqQuantity aq)
    sign = if aqQuantity aq < 0 then "-" else ""

-- | Sort asset quantities with the known tokens (those in 'knownAssets') first, and within
-- each group from the biggest amount to the smallest. Known token amounts are compared with their
-- decimal places applied (as they are displayed), unknown token amounts as raw quantities.
sortAssetsByAmount :: [AssetQuantity] -> [AssetQuantity]
sortAssetsByAmount =
  List.sortOn (\aq -> (not (hasKnownDecimals aq), Down (displayedAmount aq), aqFingerprint aq))
  where
    displayedAmount :: AssetQuantity -> Rational
    displayedAmount aq =
      fromIntegral (aqQuantity aq)
        / 10 ^ fromMaybe 0 (knownDecimals aq)

-- | The shortened asset fingerprint, followed by the asset name: the name listed in
-- 'knownAssets' for a known token, otherwise the on-chain name if it is printable text.
renderAssetName :: AssetQuantity -> Text
renderAssetName aq =
  case Map.lookup (aqFingerprint aq) knownAssets of
    Just (name, _decimals) -> fingerprint <> " " <> name
    Nothing ->
      case Text.decodeUtf8' (aqName aq) of
        Right name
          | not (Text.null name) && Text.all Char.isPrint name -> fingerprint <> " " <> name
        _otherwise -> fingerprint
  where
    fingerprint = shortenBech32 (aqFingerprint aq)

-- | Query the multi-asset balances of a stake address: the quantity of each asset received
-- less the quantity spent, ordered by fingerprint. Assets with a zero balance are omitted.
queryAssetBalances :: TxOutVariantType -> DB.StakeAddressId -> DB.DbM [AssetQuantity]
queryAssetBalances txOutVariantType saId = do
  received <- DB.queryReceivedAssetTransactions txOutVariantType saId
  spent <- DB.querySpentAssetTransactions txOutVariantType saId
  let balances =
        Map.fromListWith (+) $
          map (toEntry id) received ++ map (toEntry negate) spent
  pure
    [ AssetQuantity fingerprint name quantity
    | ((fingerprint, name), quantity) <- Map.toList balances
    , quantity /= 0
    ]
  where
    toEntry ::
      (Integer -> Integer) ->
      (ByteString, Text, ByteString, Integer) ->
      ((Text, ByteString), Integer)
    toEntry sign (_hash, fingerprint, name, quantity) = ((fingerprint, name), sign quantity)
