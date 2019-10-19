{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Web.Server.BlockPages
  ( blockPages
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Esqueleto ((^.), countRows, from, select, unValue, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), isJust, listToMaybe)

import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.LegacyApi (PageNumber)
import           Explorer.Web.Server.Util (divRoundUp, runQuery, toPageSize)
import           Explorer.Web.Server.Types (PageSize (..))

import           Servant (Handler)


blockPages
    :: SqlBackend -> Maybe PageSize
    -> Handler (Either ExplorerError PageNumber)
blockPages backend mPageSize =
    runQuery backend $ do
      blockCount <- queryMainBlockCount
      if | blockCount < 1 -> pure $ Left (Internal "There are currently no block to display.")
         | pageSize < 1 -> pure $ Left (Internal "Page size must be greater than 1 if you want to display blocks.")
         | otherwise -> pure $ Right $ divRoundUp blockCount pageSize
  where
    pageSize = unPageSize $ toPageSize mPageSize

queryMainBlockCount :: MonadIO m => ReaderT SqlBackend m Word
queryMainBlockCount = do
  res <- select . from $ \ blk -> do
            where_ (isJust $ blk ^. BlockBlockNo)
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)
