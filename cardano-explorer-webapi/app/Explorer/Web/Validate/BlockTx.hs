{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Validate.Address
  ( validateBlockTx
  ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Text.ANSI (green, red)
import qualified Data.Text.IO as Text

import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (LookupFail, renderLookupFail)

import           Explorer.Web (CAddress (..), CAddressSummary (..), CCoin (..),
                    queryAddressSummary, runQuery)
import           Explorer.Web.Error (ExplorerError (..), renderExplorerError)
import           Explorer.Web.Api.Legacy.Util (decodeTextAddress, textShow)
import           Explorer.Web.Validate.Random (queryRandomBlockHash)

import           System.Exit (exitFailure)

-- | Validate that the total coin sent agrees when calling the SQL queries behind:
--      /api/blocks/summary/{blkHash}
--      /api/blocks/txs/{blkHash}

validateBlockTx :: SqlBackend -> IO ()
validateBlockTx backend = do
  addrSum <- runQuery backend $ do
                addrTxt <- handleLookupFail =<< queryRandomBlockHash
                handleExplorerError =<< queryAddressSummary addrTxt addr
  reportAddressSummary addrSum
