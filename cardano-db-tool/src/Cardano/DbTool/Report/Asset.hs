{-# LANGUAGE OverloadedStrings #-}

-- | Multi-asset (native token) support shared by the reports.
module Cardano.DbTool.Report.Asset (
  AssetQuantity (..),
  hasKnownDecimals,
  knownAssetDecimals,
  queryAssetBalances,
  rawQuantityNote,
  renderAssetName,
  renderAssetQuantity,
) where

import Cardano.Db (TxOutVariantType)
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display (shortenBech32)
import Cardano.Prelude (textShow)
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | A quantity of a multi-asset: the net movement in a transaction (positive if received by the
-- stake address, negative if sent from it) or the balance held by a stake address.
data AssetQuantity = AssetQuantity
  { aqFingerprint :: !Text
  , aqName :: !ByteString
  , aqQuantity :: !Integer
  }
  deriving (Eq)

-- | The number of decimal places of known tokens, keyed by asset fingerprint. Token decimals are
-- not recorded on chain (they are published off-chain by the token issuer), so they are listed
-- here for the tokens of interest. Quantities of other tokens are shown as raw on-chain amounts.
knownAssetDecimals :: Map Text Int
knownAssetDecimals =
  Map.fromList
    [ ("asset1wd3llgkhsw6etxf2yca6cgk9ssrpva3wf0pq9a", 6) -- NIGHT
    , ("asset16fq594uun90f2jajmecjcdt4jnsnq7r3jdqsw5", 6) -- USDA
    , ("asset15f3ymkjafxxeunv5gtdl54g5qs8ty9k84tq94x", 6) -- DJED
    , ("asset12ffdj8kk2w485sr7a5ekmjjdyecz8ps2cm5zed", 6) -- USDM
    ]

hasKnownDecimals :: AssetQuantity -> Bool
hasKnownDecimals aq = Map.member (aqFingerprint aq) knownAssetDecimals

-- | The note to print under a table that shows asset quantities without known decimals.
rawQuantityNote :: Text
rawQuantityNote =
  "Asset quantities are raw on-chain amounts (token decimals are not applied) unless the token's decimals are known."

-- | The quantity, with the token's decimal places applied if they are known, eg 1123456 is
-- rendered as "1.123456" for a token with 6 decimal places.
renderAssetQuantity :: AssetQuantity -> Text
renderAssetQuantity aq =
  sign
    <> case Map.lookup (aqFingerprint aq) knownAssetDecimals of
      Just decimals
        | decimals > 0 ->
            let (whole, frac) = quantity `divMod` (10 ^ decimals)
             in textShow whole <> "." <> Text.justifyRight decimals '0' (textShow frac)
      _otherwise -> textShow quantity
  where
    quantity = abs (aqQuantity aq)
    sign = if aqQuantity aq < 0 then "-" else ""

-- | The shortened asset fingerprint, followed by the asset name if it is printable text.
renderAssetName :: AssetQuantity -> Text
renderAssetName aq =
  case Text.decodeUtf8' (aqName aq) of
    Right name
      | not (Text.null name) && Text.all Char.isPrint name ->
          shortenBech32 (aqFingerprint aq) <> " " <> name
    _otherwise -> shortenBech32 (aqFingerprint aq)

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
