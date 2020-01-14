{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate
  ( runValidation
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runNoLoggingT)

import           Data.Text.ANSI (yellow)
import qualified Data.Text.IO as Text

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (readPGPassFileEnv, toConnectionString)

import           Explorer.Web.Api.Legacy.Util (textShow)
import           Explorer.Web.Validate.Address (validateAddressSummary, validateRedeemAddressSummary)
import           Explorer.Web.Validate.GenesisAddress (validateGenesisAddressPaging)

runValidation :: Word -> IO ()
runValidation count = do
  pgconfig <- readPGPassFileEnv
  putStrLn ""
  runNoLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      liftIO $ loop backend 1
  where
    loop :: SqlBackend -> Word -> IO ()
    loop backend n
      | n > count = pure ()
      | otherwise = do
          Text.putStrLn $ yellow ("Test #" <> textShow n <> ":")
          validate backend
          loop backend (n + 1)

validate :: SqlBackend -> IO ()
validate backend = do
  validateRedeemAddressSummary backend
  validateAddressSummary backend
  validateGenesisAddressPaging backend
  putStrLn ""
