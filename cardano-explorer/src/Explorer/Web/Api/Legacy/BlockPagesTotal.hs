{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Explorer.Web.Api.Legacy.BlockPagesTotal
  ( blockPagesTotal
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (listToMaybe)

import           Database.Esqueleto ((^.), countRows, from, select, unValue, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), isJust)

import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy (PageNumber)
import           Explorer.Web.Api.Legacy.Util (divRoundUp, runQuery, toPageSize)
import           Explorer.Web.Api.Legacy.Types (PageSize (..))

import           Servant (Handler)


blockPagesTotal
    :: SqlBackend -> Maybe PageSize
    -> Handler (Either ExplorerError PageNumber)
blockPagesTotal backend mPageSize =
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
