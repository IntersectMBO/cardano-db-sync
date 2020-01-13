{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Explorer.Web.Validate.Random
  ( queryRandomAddress
  , queryRandomBlockHash
  , queryRandomRedeemAddress
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Maybe (listToMaybe)
import           Data.Text (Text)

import           Database.Esqueleto (Entity (..), InnerJoin (..), Value (..), SqlExpr,
                    (^.), (==.),
                    countRows, from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (BlockId, EntityField (..), LookupFail (..), Key (..),
                    TxOut (..), TxOutId, isJust, maybeToEither)

import           Explorer.Web.Api.Legacy.Util (bsBase16Encode)

import           System.Random (randomRIO)


-- | Get a random address.
queryRandomAddress :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Text)
queryRandomAddress = do
    res <- select . from $ \ (_ :: SqlExpr (Entity TxOut)) ->
              pure countRows
    case listToMaybe res of
      Nothing -> pure $ Left (DbLookupMessage "queryRandomAddress: Empty TxOut table")
      Just (Value txoCount) -> do
        txoid <- liftIO $ randomRIO (1, txoCount - 1)
        res1 <- select . from $ \ txOut -> do
                  where_ (txOut ^. TxOutId ==. val (mkTxOutId txoid))
                  pure (txOut ^. TxOutAddress)
        pure $ maybeToEither errMsg unValue (listToMaybe res1)
  where
    errMsg :: LookupFail
    errMsg = DbLookupMessage "queryRandomAddress: Lookup address by index failed"

queryRandomBlockHash :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Text)
queryRandomBlockHash = do
    res <- select . from $ \ blk -> do
              where_ (isJust (blk ^. BlockBlockNo))
              pure countRows
    case listToMaybe res of
      Nothing -> pure $ Left (DbLookupMessage "queryRandomBlockHash: Empty Block table")
      Just (Value blkCount) -> do
        blkid <- liftIO $ randomRIO (1, blkCount - 1)
        res1 <- select . from $ \ blk -> do
                  where_ (blk ^. BlockId ==. val (mkBlockId blkid))
                  pure (blk ^. BlockHash)
        pure $ maybeToEither errMsg (bsBase16Encode . unValue) (listToMaybe res1)
  where
    errMsg :: LookupFail
    errMsg = DbLookupMessage "queryRandomBlockHash: Lookup block by index failed"

queryRandomRedeemAddress :: MonadIO m => ReaderT SqlBackend m (Either LookupFail Text)
queryRandomRedeemAddress = do
    res <- select . from $ \ (tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              -- Block 1 contains all the TxOuts from the Genesis Distribution.
              where_ (tx ^. TxBlock ==. val (mkBlockId 1))
              pure countRows
    case listToMaybe res of
      Nothing -> pure $ Left (DbLookupMessage "queryRandomAddress: Empty TxOut table")
      Just (Value txoCount) -> do
        txoid <- liftIO $ randomRIO (1, txoCount - 1)
        res1 <- select . from $ \ txOut -> do
                  where_ (txOut ^. TxOutId ==. val (mkTxOutId txoid))
                  pure (txOut ^. TxOutAddress)
        pure $ maybeToEither errMsg unValue (listToMaybe res1)
  where
    errMsg :: LookupFail
    errMsg = DbLookupMessage "queryRandomRedeemAddress: Lookup address by index failed"

mkTxOutId :: Word -> TxOutId
mkTxOutId = TxOutKey . fromIntegral

mkBlockId :: Word -> BlockId
mkBlockId = BlockKey . fromIntegral
