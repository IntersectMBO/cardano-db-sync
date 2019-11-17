{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Explorer.Web.Api.Legacy.EpochPage
  ( epochPage
  ) where

import           Cardano.Chain.Slotting (EpochNumber (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString (ByteString)
import           Data.Fixed (Fixed (..), Uni)
import qualified Data.List as List
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Text (Text)
import           Data.Word (Word64)

import           Database.Esqueleto (Entity (..), InnerJoin (..), LeftOuterJoin (..), Value (..),
                    (^.), (?.), (==.), asc, countRows, from, groupBy, just, limit,
                    offset, on, orderBy, select, sum_, val, where_)
import           Database.Persist.Sql (SqlBackend)


import           Explorer.DB (Block (..), EntityField (..))

import           Explorer.Web.ClientTypes (CBlockEntry (..), CHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.Types (PageNo (..))
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, divRoundUp, runQuery,
                    slotsPerEpoch, textShow)

import           Servant (Handler)

-- Example queries:
--
--  /api/epochs/0
--  /api/epochs/1
--  /api/epochs/1?page=1
--  /api/epochs/1?page=2

epochPage
    :: SqlBackend -> EpochNumber -> Maybe PageNo
    -> Handler (Either ExplorerError (Int, [CBlockEntry]))
epochPage backend (EpochNumber epoch) mPageNo = do
    runQuery backend $ do
      mEpochBlocks <- queryEpochBlockCount epoch
      case mEpochBlocks of
        Nothing -> pure $ Left (noBlocksFound epoch mPageNo)
        Just 0 -> pure $ Left (noBlocksFound epoch Nothing)
        Just epochBlocks -> queryEpochBlocks epoch epochBlocks (fromMaybe (PageNo 1) mPageNo)


queryEpochBlockCount :: MonadIO m => Word64 -> ReaderT SqlBackend m (Maybe Int)
queryEpochBlockCount epoch = do
  res <- select . from $ \ blk -> do
          where_ (blk ^. BlockEpochNo ==. just (val epoch))
          pure countRows
  pure $ unValue <$> (listToMaybe res)

queryEpochBlocks
    :: MonadIO m
    => Word64 -> Int -> PageNo
    -> ReaderT SqlBackend m (Either ExplorerError (Int, [CBlockEntry]))
queryEpochBlocks epoch epochBlocks (PageNo page) = do
    rows <- select . from $ \ ((sl `InnerJoin` blk) `LeftOuterJoin` tx) -> do
              on (just (blk ^. BlockId) ==. tx ?. TxBlock)
              on (sl ^. SlotLeaderId ==. blk ^. BlockSlotLeader)
              where_ (blk ^. BlockEpochNo ==. just (val epoch))
              orderBy [asc (blk ^. BlockSlotNo)]
              groupBy (blk ^. BlockId)
              groupBy (sl ^. SlotLeaderId)
              offset (fromIntegral $ (page - 1) * 10)
              limit 10
              pure (blk, sl ^. SlotLeaderHash, sum_ (tx ?. TxOutSum), sum_ (tx ?. TxFee))
    case rows of
      [] -> pure $ Left (noBlocksFound epoch $ Just (PageNo page))
      _ -> pure $ Right (epochBlocks `divRoundUp` 10, map convert $ List.reverse rows)
  where
    convert :: (Entity Block, Value ByteString, Value (Maybe Uni), Value (Maybe Uni)) -> CBlockEntry
    convert (Entity _ blk, Value slh, vmOutSum, vmFee) =
      CBlockEntry
        { cbeEpoch = fromMaybe 0 (blockEpochNo blk)
        , cbeSlot = maybe 0 (\x -> fromIntegral $ x `mod` slotsPerEpoch) (blockSlotNo blk)
        , cbeBlkHeight = maybe 0 fromIntegral (blockBlockNo blk)
        , cbeBlkHash = CHash $ bsBase16Encode (blockHash blk)
        , cbeTimeIssued = Just $ utcTimeToPOSIXSeconds (blockTime blk)
        , cbeTxNum = fromIntegral (blockTxCount blk)
        , cbeTotalSent = mkCCoin $ unTotal vmOutSum
        , cbeSize = blockSize blk
        , cbeBlockLead = Just $ bsBase16Encode slh
        , cbeFees = mkCCoin $ unTotal vmFee
        }

unTotal :: Value (Maybe Uni) -> Integer
unTotal mvi =
  case unValue mvi of
    Just (MkFixed x) -> x
    _ -> 0

noBlocksFound :: Word64 -> Maybe PageNo -> ExplorerError
noBlocksFound epoch mPageNo =
    Internal $ maybe msg (\(PageNo pn) -> msg <> " page #" <> textShow pn) mPageNo
  where
    msg :: Text
    msg = "No blocks found for epoch #" <> textShow epoch

