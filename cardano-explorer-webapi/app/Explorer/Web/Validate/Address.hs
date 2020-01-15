{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate.Address
  ( validateAddressSummary
  , validateRedeemAddressSummary
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.ANSI (green, red)
import qualified Data.Text.IO as Text

import           Database.Persist.Sql (SqlBackend)

import           Explorer.Web (CAddress (..), CAddressSummary (..), CCoin (..), CHash (..),
                    CTxBrief (..), CTxHash (..),
                    queryAddressSummary, runQuery)
import           Explorer.Web.Api.Legacy.Util (decodeTextAddress, textShow)
import           Explorer.Web.Validate.Random (queryRandomAddress, queryRandomRedeemAddress)
import           Explorer.Web.Validate.ErrorHandling (handleLookupFail, handleExplorerError)

import           System.Exit (exitFailure)

-- | Validate that all address have a balance >= 0.
validateAddressSummary :: SqlBackend -> IO ()
validateAddressSummary backend = do
  addrSum <- runQuery backend $ do
                addrTxt <- handleLookupFail =<< queryRandomAddress
                addr <- handleExplorerError $ decodeTextAddress addrTxt
                handleExplorerError =<< queryAddressSummary addrTxt addr
  if unCCoin (caBalance addrSum) >= 0
    then reportAddressSummaryOk addrSum
    else reportAddressSummaryFail addrSum
  validateAddressTotalFees addrSum
  validateTxFeeNonNegative addrSum

-- | Validate that all redeem address have a balance >= 0.
validateRedeemAddressSummary :: SqlBackend -> IO ()
validateRedeemAddressSummary backend = do
  addrSum <- runQuery backend $ do
                addrTxt <- handleLookupFail =<< queryRandomRedeemAddress
                addr <- handleExplorerError $ decodeTextAddress addrTxt
                handleExplorerError =<< queryAddressSummary addrTxt addr
  if unCCoin (caBalance addrSum) >= 0
    then reportAddressSummaryOk addrSum
    else reportAddressSummaryFail addrSum

-- -------------------------------------------------------------------------------------------------

validateAddressTotalFees :: CAddressSummary -> IO ()
validateAddressTotalFees addrSum =
    if unCCoin (caBalance addrSum) >= 0
      then reportAddressFeesOk
      else reportAddressFeesFail
  where
    reportAddressFeesOk :: IO ()
    reportAddressFeesOk = do
      mapM_ Text.putStr
        [ "  Total fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " is non-negative: "
        ]
      Text.putStrLn $ green "ok"

    reportAddressFeesFail :: IO ()
    reportAddressFeesFail = do
      Text.putStrLn $ "  Total fees for address are negative: "
      reportCAddressSummary addrSum
      exitFailure

validateTxFeeNonNegative :: CAddressSummary -> IO ()
validateTxFeeNonNegative addrSum =
    case filter (\x -> unCCoin (ctbFees x) < 0) (caTxList addrSum) of
      [] -> reportAddressSummaryTxFeeOk
      xs -> reportAddressSummaryTxFeeFail xs
  where
    reportAddressSummaryTxFeeOk :: IO ()
    reportAddressSummaryTxFeeOk = do
      mapM_ Text.putStr
        [ "  Individual tx fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " is non-negative: "
        ]
      Text.putStrLn $ green "ok"

    reportAddressSummaryTxFeeFail :: [CTxBrief] -> IO ()
    reportAddressSummaryTxFeeFail xs = do
      mapM_ Text.putStr
        [ "  Individual tx fees for address " , shortenAddress (unCAddress $ caAddress addrSum)
        , " are negative: "
        ]
      mapM_ reportCTxBrief xs
      exitFailure

-- -------------------------------------------------------------------------------------------------

reportAddressSummaryOk :: CAddressSummary -> IO ()
reportAddressSummaryOk addrSum = do
  mapM_ Text.putStr
    [ "  Balance for " , shortenAddress (unCAddress $ caAddress addrSum)
    , " (", textShow (caType addrSum), ") is non-negative: "
    ]
  Text.putStrLn $ green "ok"

reportAddressSummaryFail :: CAddressSummary -> IO ()
reportAddressSummaryFail addrSum = do
  Text.putStrLn $ "  Address balance is negative: "
  reportCAddressSummary addrSum
  exitFailure

reportCAddressSummary :: CAddressSummary -> IO ()
reportCAddressSummary addrSum =
  mapM_ Text.putStrLn
    [ "  Address: " <> unCAddress (caAddress addrSum)
    , "    type: " <> textShow (caType addrSum)
    , "    tx count: " <> textShow (caTxNum addrSum)
    , "    balance: " <>
            let balance = unCCoin (caBalance addrSum) in
            if balance < 0
              then red (textShow balance)
              else green (textShow balance)
    , "    fees: " <>
            let fees = unCCoin (caTotalFee addrSum) in
            if fees < 0
              then red (textShow fees)
              else green (textShow fees)
    , ""
    ]

reportCTxBrief :: CTxBrief -> IO ()
reportCTxBrief tx =
  mapM_ Text.putStrLn
    [ "  Tx: " <> unCTxHash (ctbId tx)
    , "    input count: " <> textShow (length $ ctbInputs tx)
    , "    output count: " <> textShow (length $ ctbOutputs tx)
    , "    fees: " <>
            let fees = unCCoin (ctbFees tx) in
            if fees < 0
              then red (textShow fees)
              else green (textShow fees)
    , ""
    ]

shortenAddress :: Text -> Text
shortenAddress addr =
  mconcat [Text.take 10 addr, "...", Text.drop (Text.length addr - 10) addr]

unCTxHash :: CTxHash -> Text
unCTxHash (CTxHash (CHash txt)) = txt
