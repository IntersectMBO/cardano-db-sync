{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.DbTool.Report.Transactions (
  reportTransactions,
) where

import Cardano.Db
import qualified Cardano.Db as DB
import Cardano.DbTool.Report.Display
import Cardano.Prelude (textShow)
import Control.Monad (forM_, unless)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)

{- HLINT ignore "Redundant ^." -}

-- | Report the transactions for each stake address. If 'includeAssets' is set, the net
-- multi-asset movements of each transaction are also reported.
reportTransactions :: TxOutVariantType -> Bool -> [Text] -> IO ()
reportTransactions txOutVariantType includeAssets addrs =
  forM_ addrs $ \saddr -> do
    Text.putStrLn $ "\nTransactions for: " <> saddr <> "\n"
    xs <- runDbStandaloneSilent (queryStakeAddressTransactions txOutVariantType includeAssets saddr)
    renderTransactions includeAssets xs

-- -------------------------------------------------------------------------------------------------
-- This command is designed to emulate the output of the script:
-- https://forum.cardano.org/t/dump-wallet-transactions-with-cardano-cli/40651/6
-- -------------------------------------------------------------------------------------------------

data Direction = Outgoing | Incoming
  deriving (Eq, Ord, Show)

data Transaction = Transaction
  { trHash :: !Text
  , trTime :: !UTCTime
  , trDirection :: !Direction
  , trAmount :: !Ada
  , trAssets :: ![AssetMovement]
  }
  deriving (Eq)

-- | The net movement of a multi-asset in a transaction.
data AssetMovement = AssetMovement
  { amFingerprint :: !Text
  , amName :: !ByteString
  , amQuantity :: !Integer
  -- ^ Positive if received by the stake address, negative if sent from it.
  }
  deriving (Eq)

instance Ord Transaction where
  compare tra trb =
    case compare (trTime tra) (trTime trb) of
      LT -> LT
      GT -> GT
      EQ -> compare (trDirection tra) (trDirection trb)

queryStakeAddressTransactions :: TxOutVariantType -> Bool -> Text -> DB.DbM [Transaction]
queryStakeAddressTransactions txOutVariantType includeAssets address = do
  mSaId <- DB.queryStakeAddressId address
  case mSaId of
    Nothing -> pure []
    Just saId -> queryTransactions saId
  where
    queryTransactions :: DB.StakeAddressId -> DB.DbM [Transaction]
    queryTransactions saId = do
      inputs <- queryInputs txOutVariantType saId
      outputs <- queryOutputs txOutVariantType saId
      let txs = coalesceTxs (inputs ++ outputs)
      if includeAssets
        then do
          assets <- queryAssetMovements txOutVariantType saId
          pure $ map (\tr -> tr {trAssets = Map.findWithDefault [] (trHash tr) assets}) txs
        else pure txs

-- | Query the net multi-asset movements of each transaction, keyed by transaction hash.
-- Assets whose net movement in a transaction is zero (eg returned as change) are omitted.
queryAssetMovements :: TxOutVariantType -> DB.StakeAddressId -> DB.DbM (Map Text [AssetMovement])
queryAssetMovements txOutVariantType saId = do
  received <- DB.queryReceivedAssetTransactions txOutVariantType saId
  spent <- DB.querySpentAssetTransactions txOutVariantType saId
  let netQuantities =
        Map.fromListWith (+) $
          map (toEntry id) received ++ map (toEntry negate) spent
  pure $
    Map.fromListWith
      (flip (++))
      [ (hash, [AssetMovement fingerprint name quantity])
      | ((hash, fingerprint, name), quantity) <- Map.toList netQuantities
      , quantity /= 0
      ]
  where
    toEntry ::
      (Integer -> Integer) ->
      (ByteString, Text, ByteString, Integer) ->
      ((Text, Text, ByteString), Integer)
    toEntry sign (hash, fingerprint, name, quantity) =
      ((renderHash hash, fingerprint, name), sign quantity)

queryInputs :: TxOutVariantType -> DB.StakeAddressId -> DB.DbM [Transaction]
queryInputs txOutVariantType saId = do
  -- Standard UTxO inputs
  res1 <- case txOutVariantType of
    TxOutVariantCore -> DB.queryInputTransactionsCore saId
    TxOutVariantAddress -> DB.queryInputTransactionsAddress saId

  -- Reward withdrawals
  res2 <- DB.queryWithdrawalTransactions saId
  pure $ groupByTxHash (map (convertTx Incoming) res1 ++ map (convertTx Outgoing) res2)
  where
    groupByTxHash :: [Transaction] -> [Transaction]
    groupByTxHash = mapMaybe coalesceInputs . List.groupOn trHash . List.sortOn trHash

    coalesceInputs :: [Transaction] -> Maybe Transaction
    coalesceInputs xs =
      case xs of
        [] -> Nothing
        (x : _) ->
          Just $
            Transaction
              { trHash = trHash x
              , trTime = trTime x
              , trDirection = trDirection x
              , trAmount = sumAmounts xs
              , trAssets = []
              }

queryOutputs :: TxOutVariantType -> DB.StakeAddressId -> DB.DbM [Transaction]
queryOutputs txOutVariantType saId = do
  res <- case txOutVariantType of
    TxOutVariantCore -> DB.queryOutputTransactionsCore saId
    TxOutVariantAddress -> DB.queryOutputTransactionsAddress saId

  pure . groupOutputs $ map (convertTx Outgoing) res
  where
    groupOutputs :: [Transaction] -> [Transaction]
    groupOutputs = mapMaybe coalesceInputs . List.groupOn trHash . List.sortOn trHash

    coalesceInputs :: [Transaction] -> Maybe Transaction
    coalesceInputs xs =
      case xs of
        [] -> Nothing
        (x : _) ->
          Just $
            Transaction
              { trHash = trHash x
              , trTime = trTime x
              , trDirection = trDirection x
              , trAmount = sum $ map trAmount xs
              , trAssets = []
              }

sumAmounts :: [Transaction] -> Ada
sumAmounts =
  List.foldl' func 0
  where
    func :: Ada -> Transaction -> Ada
    func acc tr =
      case trDirection tr of
        Incoming -> acc + trAmount tr
        Outgoing -> acc - trAmount tr

-- | Net the incoming and outgoing entries of each transaction into a single entry, sorted by
-- time. Entries are grouped by transaction hash rather than by adjacency, because all the
-- transactions in a block have the same time and so their entries can be interleaved after
-- sorting. Each transaction has at most one outgoing and one incoming entry.
coalesceTxs :: [Transaction] -> [Transaction]
coalesceTxs =
  List.sort . mapMaybe (coalesce . List.sortOn trDirection) . Map.elems . Map.fromListWith (flip (++)) . map (\tr -> (trHash tr, [tr]))
  where
    coalesce :: [Transaction] -> Maybe Transaction
    coalesce xs =
      case xs of
        [] -> Nothing
        [a] -> Just a
        [a, b] ->
          Just $
            if trAmount a > trAmount b
              then Transaction (trHash a) (trTime a) Outgoing (trAmount a - trAmount b) []
              else Transaction (trHash a) (trTime a) Incoming (trAmount b - trAmount a) []
        _otherwise -> error $ "coalesceTxs: " ++ show (length xs)

convertTx :: Direction -> (ByteString, UTCTime, DbLovelace) -> Transaction
convertTx dir (hash, time, ll) =
  Transaction
    { trHash = renderHash hash
    , trTime = time
    , trDirection = dir
    , trAmount = word64ToAda (unDbLovelace ll)
    , trAssets = []
    }

renderHash :: ByteString -> Text
renderHash = Text.decodeUtf8 . Base16.encode

-- | Render the transactions. If 'includeAssets' is set, an 'asset' column is added and each
-- transaction row is followed by a row for each multi-asset it moved.
renderTransactions :: Bool -> [Transaction] -> IO ()
renderTransactions includeAssets xs = do
  mapM_ Text.putStrLn (renderTable cols (concatMap toRows xs))
  putStrLn ""
  unless (all (all hasKnownDecimals . trAssets) xs) $
    putStrLn "Asset quantities are raw on-chain amounts (token decimals are not applied) unless the token's decimals are known.\n"
  where
    cols :: [(Align, Text)]
    cols =
      [ (AlignLeft, "tx_hash")
      , (AlignLeft, "date/time")
      , (AlignLeft, "direction")
      , (AlignRight, "amount")
      ]
        ++ [(AlignLeft, "asset") | includeAssets]

    -- A row for the transaction's ADA movement, followed by a row for each asset it moved.
    toRows :: Transaction -> [[Text]]
    toRows tr =
      ( [ trHash tr
        , formatReportTime (trTime tr)
        , textShow (trDirection tr)
        , renderAda (trAmount tr)
        ]
          ++ ["ADA" | includeAssets]
      )
        : map assetRow (trAssets tr)

    assetRow :: AssetMovement -> [Text]
    assetRow am =
      [ ""
      , ""
      , textShow (if amQuantity am < 0 then Outgoing else Incoming)
      , renderAssetQuantity am
      , renderAssetName am
      ]

-- | The number of decimal places of known tokens, keyed by asset fingerprint. Token decimals are
-- not recorded on chain (they are published off-chain by the token issuer), so they are listed
-- here for the tokens of interest. Quantities of other tokens are shown as raw on-chain amounts.
knownAssetDecimals :: Map Text Int
knownAssetDecimals =
  Map.fromList
    [ ("asset1wd3llgkhsw6etxf2yca6cgk9ssrpva3wf0pq9a", 6) -- NIGHT
    , ("asset16fq594uun90f2jajmecjcdt4jnsnq7r3jdqsw5", 6) -- USDA
    ]

hasKnownDecimals :: AssetMovement -> Bool
hasKnownDecimals am = Map.member (amFingerprint am) knownAssetDecimals

-- | The absolute quantity of an asset movement, with the token's decimal places applied if they
-- are known, eg 1123456 is rendered as "1.123456" for a token with 6 decimal places.
renderAssetQuantity :: AssetMovement -> Text
renderAssetQuantity am =
  case Map.lookup (amFingerprint am) knownAssetDecimals of
    Just decimals
      | decimals > 0 ->
          let (whole, frac) = quantity `divMod` (10 ^ decimals)
           in textShow whole <> "." <> Text.justifyRight decimals '0' (textShow frac)
    _otherwise -> textShow quantity
  where
    quantity = abs (amQuantity am)

-- | The shortened asset fingerprint, followed by the asset name if it is printable text.
renderAssetName :: AssetMovement -> Text
renderAssetName am =
  case Text.decodeUtf8' (amName am) of
    Right name
      | not (Text.null name) && Text.all Char.isPrint name ->
          shortenBech32 (amFingerprint am) <> " " <> name
    _otherwise -> shortenBech32 (amFingerprint am)
