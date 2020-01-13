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

import           Explorer.Web (CAddress (..), CAddressSummary (..), CCoin (..),
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
  mapM_ Text.putStrLn
    [ "Address: " <> unCAddress (caAddress addrSum)
    , "  type: " <> textShow (caType addrSum)
    , "  tx count: " <> textShow (caTxNum addrSum)
    , "  balance: " <>
            let balance = unCCoin (caBalance addrSum) in
            if balance < 0
              then red (textShow balance)
              else green (textShow balance)
    , ""
    ]
  exitFailure


shortenAddress :: Text -> Text
shortenAddress addr =
  mconcat [Text.take 10 addr, "...", Text.drop (Text.length addr - 10) addr]
