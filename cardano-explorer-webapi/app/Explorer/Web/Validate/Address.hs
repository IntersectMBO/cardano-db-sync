{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate.Address
  ( validateAddressSummary
  , validateRedeemAddressSummary
  ) where

import           Control.Monad (when)

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
  reportAddressSummary addrSum

-- | Validate that all redeem address have a balance >= 0.
validateRedeemAddressSummary :: SqlBackend -> IO ()
validateRedeemAddressSummary backend = do
  addrSum <- runQuery backend $ do
                addrTxt <- handleLookupFail =<< queryRandomRedeemAddress
                addr <- handleExplorerError $ decodeTextAddress addrTxt
                handleExplorerError =<< queryAddressSummary addrTxt addr
  reportAddressSummary addrSum

reportAddressSummary :: CAddressSummary -> IO ()
reportAddressSummary addrSum = do
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
  when (unCCoin (caBalance addrSum) < 0) $
    exitFailure
