{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Core.DB.Query
  ( queryBlockCount
  , queryBlockId
  , queryLatestBlocks
  , querySelectCount
  ) where


import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)

import           Database.Esqueleto (From, SqlQuery, (^.), (==.), countRows, desc, entityKey,
                    from, limit, orderBy, select, unValue, val, where_)
import           Database.Persist.Sql (SqlBackend, entityVal)

import           Explorer.Core.DB.Schema

-- If you squint, these Esqueleto queries almost look like SQL queries.



-- | Get the last N blocks.
-- This assumes that the block are inserted into the datebase from oldest to
-- newest which is not exlicitly enforced, but should arise automatically
-- from the way blocks are retrieved and inserted into the database.
queryLatestBlocks :: MonadIO m => Int -> ReaderT SqlBackend m [Block]
queryLatestBlocks limitCount = do
  eblks <- select $ from $ \ blk -> do
              orderBy [desc (blk ^. BlockId)]
              limit $ fromIntegral limitCount
              pure blk
  pure $ map entityVal eblks


-- | Count the number of blocks in the Block table.
queryBlockCount :: (MonadIO m, From Block) => ReaderT SqlBackend m Word
queryBlockCount = do
  res <- select $ from $ \ (_ :: Block ) ->
            pure countRows
  pure $ maybe 0 unValue (listToMaybe res)

-- | Count the number of blocks in the Block table.
queryBlockId :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe BlockId)
queryBlockId hash = do
  res <- select $ from $ \ blk -> do
            where_ (blk ^. BlockHash ==. val hash)
            pure blk
  pure $ fmap entityKey (listToMaybe res)


-- | Count the number of rows that match the select with the supplied predicate.
querySelectCount :: (MonadIO m, From table) => (table -> SqlQuery ()) -> ReaderT SqlBackend m Word
querySelectCount predicate = do
  xs <- select $ from $ \x -> do
            predicate x
            pure countRows
  pure $ maybe 0 unValue (listToMaybe xs)

-- -----------------------------------------------------------------------------

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a
