{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Query where

import           Database.Esqueleto
import           Database.Persist.Sql       (SqlBackend)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString

import           Explorer.DB

queryOneBlock :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Block, Key Block))
queryOneBlock blkHash = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockHash ==. val blkHash
    pure blk
  let
    both :: Entity Block -> (Block, Key Block)
    both e = (entityVal e, entityKey e)
  pure $ fmap both (listToMaybe rows)

queryBlockHash :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe ByteString)
queryBlockHash blkid = do
  rows <- select . from $ \blk -> do
    where_ $ blk ^. BlockId ==. val blkid
    pure $ blk ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryNextBlock :: MonadIO m => Key Block -> ReaderT SqlBackend m (Maybe ByteString)
queryNextBlock blkid = do
  rows <- select . from $ \blk2 -> do
    where_ $ blk2 ^. BlockPrevious ==. val (Just blkid)
    pure $ blk2 ^. BlockHash
  pure (unValue <$> listToMaybe rows)

queryBlockSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Block, Maybe ByteString, Maybe ByteString))
queryBlockSummary blkHash = do
  maybeBlock <- queryOneBlock blkHash
  case maybeBlock of
    Just (blk, blkid) ->
      case blockPrevious blk of
        Just prevblkid -> do
          previousHash <- queryBlockHash prevblkid
          nextHash <- queryNextBlock blkid
          pure $ Just (blk, previousHash, nextHash)
        Nothing -> pure Nothing
    Nothing -> pure Nothing
