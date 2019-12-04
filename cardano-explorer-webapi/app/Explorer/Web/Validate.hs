{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate
  ( runValidation
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT)

import           Data.Text.ANSI (green, red)
import qualified Data.Text.IO as Text

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (LookupFail, readPGPassFileEnv, renderLookupFail, toConnectionString)

import           Explorer.Web (CAddress (..), CAddressSummary (..), CCoin (..),
                    queryAddressSummary, runQuery)
import           Explorer.Web.Error (ExplorerError (..), renderExplorerError)
import           Explorer.Web.Api.Legacy.Util (decodeTextAddress, textShow)
import           Explorer.Web.Random (queryRandomAddress, queryRandomRedeemAddress)

import           System.Exit (exitFailure)

runValidation :: Word -> IO ()
runValidation count = do
  pgconfig <- readPGPassFileEnv
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      liftIO $ loop backend count
  where
    loop :: SqlBackend -> Word -> IO ()
    loop backend n
      | n == 0 = pure ()
      | otherwise = do
          validate backend
          loop backend (n - 1)

validate :: SqlBackend -> IO ()
validate backend = do
  validateRedeemAddressSummary backend
  validateAddressSummary backend


validateAddressSummary :: SqlBackend -> IO ()
validateAddressSummary backend = do
  addrSum <- runQuery backend $ do
                addrTxt <- handleLookupFail =<< queryRandomAddress
                addr <- handleExplorerError $ decodeTextAddress addrTxt
                handleExplorerError =<< queryAddressSummary addrTxt addr
  reportAddressSummary addrSum

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

handleLookupFail :: MonadIO m => Either LookupFail a -> m a
handleLookupFail ela =
  case ela of
    Left err -> liftIO $ do
                  Text.putStrLn $ renderLookupFail err
                  exitFailure
    Right v -> pure v

handleExplorerError :: MonadIO m => Either ExplorerError a -> m a
handleExplorerError eea =
  case eea of
    Left err -> liftIO $ do
                  Text.putStrLn $ renderExplorerError err
                  exitFailure
    Right v -> pure v
