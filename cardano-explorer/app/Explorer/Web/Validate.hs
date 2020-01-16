{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate
  ( runValidation
  ) where

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
  findWorst backend
  if False
    then validateRedeemAddressSummary backend
    else pure ()
  validateAddressSummary backend


findWorst :: SqlBackend -> IO ()
findWorst backend = do
  addrSum <- runQuery backend $ do
                let addrTxt = "DdzFFzCqrhssKt9voak6J9nQ85vP8PHecMNXtBQFsmzoWaLb9poUF85NCty5nfciRPDYBf6pPCctM9SmkAwu1RyFhDWeVhbrd5RdrNHt"
                addr <- handleExplorerError $ decodeTextAddress addrTxt
                handleExplorerError =<< queryAddressSummary addrTxt addr
  reportAddressSummary addrSum
  loop addrSum
  where
    loop :: CAddressSummary -> IO ()
    loop oldAddrSum = do
      addrSum <- runQuery backend $ do
                    addrTxt <- handleLookupFail =<< queryRandomAddress
                    addr <- handleExplorerError $ decodeTextAddress addrTxt
                    handleExplorerError =<< queryAddressSummary addrTxt addr
      let balance = unCCoin (caBalance addrSum)
      if balance >= 0
        then loop oldAddrSum
        else if caTxNum oldAddrSum < caTxNum addrSum
                then loop oldAddrSum
                else reportAddressSummary addrSum >> loop addrSum

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
reportAddressSummary addrSum =
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
