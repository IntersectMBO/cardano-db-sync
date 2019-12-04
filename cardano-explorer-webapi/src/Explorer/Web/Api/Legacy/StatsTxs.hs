{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Explorer.Web.Api.Legacy.StatsTxs
  ( statsTxs
  ) where

import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.List as List
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.),
                    asc, desc, from, limit, offset, on, orderBy, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (BlockId, EntityField (..), isJust, queryBlockHeight)

import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.ClientTypes (CHash (..), CTxHash (..))
import           Explorer.Web.Api.Legacy (PageNumber, TxsStats)
import           Explorer.Web.Api.Legacy.Types (PageNo (..))
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, runQuery)

import           Servant (Handler)

-- Example queries:
--
--  /api/stats/txs
--  /api/stats/txs?page=1
--  /api/stats/txs?page=4000
--  /api/stats/txs?page=10000
--  /api/stats/txs?page=100000


-- type TxsStats = (PageNumber, [(CTxHash, Word64)])
statsTxs
    :: SqlBackend -> Maybe PageNo
    -> Handler (Either ExplorerError TxsStats)
statsTxs backend mPageNo = do
    runQuery backend $ do
      blockHeight <- queryBlockHeight
      let currentPageNo = toPageNo blockHeight
      case mPageNo of
        Nothing -> Right . (currentPageNo,) <$> queryLatestBlockTx (calculatePageEntries blockHeight)
        Just (PageNo 0) -> pure $ Left (Internal "Page number must be greater than 0")
        Just pn ->
          if unPageNo pn > currentPageNo
            then pure $ Left (Internal "Number of pages exceeds total page count.")
            else Right . (currentPageNo,) <$> queryBlockTxPageNo pn
  where
    toPageNo :: Word64 -> PageNumber
    toPageNo x =
      case fromIntegral x `divMod` 10 of
        (y, 0) -> y
        (y, _) -> y + 1

    calculatePageEntries :: Word64 -> Int
    calculatePageEntries blockHeight =
      case blockHeight `mod` 10 of
        0 -> 10
        y -> fromIntegral y


queryLatestBlockTx :: MonadIO m => Int -> ReaderT SqlBackend m [(CTxHash, Word64)]
queryLatestBlockTx count = do
  rows <- select . from $ \ blk -> do
          where_ (isJust (blk ^. BlockBlockNo))
          orderBy [desc (blk ^. BlockBlockNo)]
          limit (fromIntegral count)
          pure (blk ^. BlockId)
  concatMapM queryBlockTx rows

queryBlockTxPageNo :: MonadIO m => PageNo -> ReaderT SqlBackend m [(CTxHash, Word64)]
queryBlockTxPageNo (PageNo page) = do
  rows <- select . from $ \ blk -> do
          where_ (isJust (blk ^. BlockBlockNo))
          orderBy [asc (blk ^. BlockBlockNo)]
          offset (fromIntegral $ (page - 1) * 10)
          limit 10
          pure (blk ^. BlockId)
  concatMapM queryBlockTx $ List.reverse rows

queryBlockTx :: MonadIO m => Value BlockId -> ReaderT SqlBackend m [(CTxHash, Word64)]
queryBlockTx (Value blkid) = do
    rows <- select . from $ \ (blk `InnerJoin` tx) -> do
            on (blk ^. BlockId ==. tx ^. TxBlock)
            where_ (blk ^. BlockId ==. val blkid)
            pure (tx ^. TxHash, tx ^. TxSize)
    pure $ map convert rows
  where
    convert :: (Value ByteString, Value Word64) -> (CTxHash, Word64)
    convert (Value h, Value s) = (CTxHash $ CHash (bsBase16Encode h), s)
