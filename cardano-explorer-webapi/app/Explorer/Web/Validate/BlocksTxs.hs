{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Explorer.Web.Validate.BlocksTxs
  ( validateBlocksTxs
  ) where

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.ANSI (green, red)
import qualified Data.Text.IO as Text

import           Database.Persist.Sql (SqlBackend)

import           Explorer.Web (CTxBrief (..), CTxAddressBrief (..), CTxBrief (..),
                    queryBlocksTxs, runQuery)
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode)
import           Explorer.Web.Validate.Random (queryRandomBlockHash)
import           Explorer.Web.Validate.ErrorHandling (handleLookupFail, handleExplorerError)

import           System.Exit (exitFailure)

import           Text.Show.Pretty (ppShow)

validateBlocksTxs :: SqlBackend -> IO ()
validateBlocksTxs backend = do
  (blkHash, txs) <- runQuery backend $ do
                      blkHash <- handleLookupFail =<< queryRandomBlockHash
                      (blkHash,) <$> (handleExplorerError =<< queryBlocksTxs blkHash 100 0)

  validateInputsUnique (bsBase16Encode blkHash) $ List.sortOn ctaAddress (concatMap ctbInputs txs)
  validateOutputsUnique (bsBase16Encode blkHash) $ List.sortOn ctaAddress (concatMap ctbOutputs txs)


-- -------------------------------------------------------------------------------------------------

validateInputsUnique :: Text -> [CTxAddressBrief] -> IO ()
validateInputsUnique blkHash tabs = do
  mapM_ Text.putStr [ "  Inputs for block " , shortenTxHash blkHash, " are unique: " ]
  if length tabs == length (List.nub tabs)
    then Text.putStrLn $ green "ok"
    else do
      Text.putStrLn $ red "validateInputsUnique failed"
      exitFailure

-- https://github.com/input-output-hk/cardano-explorer/issues/195
validateOutputsUnique :: Text -> [CTxAddressBrief] -> IO ()
validateOutputsUnique blkHash tabs = do
  mapM_ Text.putStr [ "  Outputs for block " , shortenTxHash blkHash, " are unique: " ]
  if length tabs == length (List.nub tabs)
    then Text.putStrLn $ green "ok"
    else do
      Text.putStrLn $ red "failed\n    Duplicate entries in:"
      mapM_ (\x -> putStrLn $ "      " ++ x) $ lines (ppShow tabs)
      exitFailure

-- -------------------------------------------------------------------------------------------------

shortenTxHash :: Text -> Text
shortenTxHash txh =
  mconcat [Text.take 10 txh, "...", Text.drop (Text.length txh - 10) txh]
