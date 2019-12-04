{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Explorer.Web.Api.Legacy.TxsSummary
  ( txsSummary
  ) where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, left)

import           Data.ByteString (ByteString)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..), (^.), (==.), (&&.),
                    entityVal, from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (Block (..), BlockId, EntityField (..), LookupFail (..), Tx (..),
                    TxId, entityPair, maybeToEither)

import           Explorer.Web.ClientTypes (CAddress (..), CCoin (..), CHash (..),
                    CTxAddressBrief (..), CTxHash (..), CTxSummary (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Api.Legacy.Util

import           Servant (Handler)
-- import           Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)


-- Example queries:
--
--  /api/txs/summary/494714ddea994bd321ce3a850ceb21416aad3f693dbd38ce47daf0269d93c26f
--  /api/txs/summary/8480fe609b742e6fa1dca9d9aff1bdc7a90acb0f1e68105634f07fcef68c2479
--  /api/txs/summary/c8f98baea0b6eb8709398c2b16a17a25d722e1600bed39128a3e9ea4b0d8b5c1

txsSummary
    :: SqlBackend -> CTxHash
    -> Handler (Either ExplorerError CTxSummary)
txsSummary backend (CTxHash (CHash hashTxt)) =
  runExceptT $ do
    txhash <- hoistEither $ textBase16Decode hashTxt
    mTxblk <- runQuery backend $ queryTxSummary txhash
    case mTxblk of
      Nothing -> left $ Internal "tx not found" -- TODO, give the same error as before?
      Just (tx, blk, inputs, outputs) -> do
        case blockSlotNo blk of
          Just slotno -> do
            let (epoch, slot) = slotno `divMod` slotsPerEpoch
            pure $ CTxSummary
              { ctsId              = CTxHash . CHash . bsBase16Encode $ txHash tx
              -- Tx timestamp is the same as block timestamp.
              , ctsTxTimeIssued    = Just $ blockPosixTime blk
              , ctsBlockTimeIssued = Just $ blockPosixTime blk
              , ctsBlockHeight     = fromIntegral <$> blockBlockNo blk
              , ctsBlockEpoch      = Just epoch
              , ctsBlockSlot       = Just $ fromIntegral slot
              , ctsBlockHash       = Just . CHash $ bsBase16Encode (blockHash blk)
              , ctsRelayedBy       = Nothing
              , ctsTotalInput      = mkCCoin . sum $ map (unCCoin . ctaAmount) inputs
              , ctsTotalOutput     = mkCCoin . sum $ map (unCCoin . ctaAmount) outputs
              , ctsFees            = mkCCoin $ fromIntegral (txFee tx)
              , ctsInputs          = inputs
              , ctsOutputs         = outputs
              }
          Nothing -> left $ Internal "cant find slot# of block"

queryTxSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Tx, Block, [CTxAddressBrief], [CTxAddressBrief]))
queryTxSummary txhash = do
  eTx <- queryTx txhash
  case eTx of
    Right (txid, tx) -> do
      mBlock <- queryBlockById (txBlock tx)
      case mBlock of
        Just block -> do
          inputs <- queryTxInputs txid
          outputs <- queryTxOutputs txid
          pure $ Just (tx, block, inputs, outputs)
        Nothing -> pure Nothing
    Left _ -> pure Nothing -- TODO


queryTx :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (TxId, Tx))
queryTx hash = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxHash ==. val hash)
            pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityPair (listToMaybe res)

queryTxOutputs :: MonadIO m => TxId -> ReaderT SqlBackend m [CTxAddressBrief]
queryTxOutputs txid = do
    rows <- select . from $ \ (tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              where_ (txOut ^. TxOutTxId ==. val txid)
              pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
    pure $ map convert rows
  where
    convert :: (Value Text, Value Word64, Value ByteString, Value Word16) -> CTxAddressBrief
    convert (Value addr, Value amount, Value txhash, Value index) =
      CTxAddressBrief
        { ctaAddress = CAddress addr
        , ctaAmount = mkCCoin $ fromIntegral amount
        , ctaTxHash = CTxHash $ CHash (bsBase16Encode txhash)
        , ctaTxIndex = fromIntegral index
        }

queryBlockById :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe Block)
queryBlockById blockid = do
  rows <- select . from $ \blk -> do
      where_ $ blk ^. BlockId ==. val blockid
      pure blk
  pure $ fmap entityVal (listToMaybe rows)

queryTxInputs :: MonadIO m => TxId -> ReaderT SqlBackend m [CTxAddressBrief]
queryTxInputs txid = do
    rows <- select . from $ \(txIn `InnerJoin` txOut  `InnerJoin` tx) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              on ((txIn ^. TxInTxOutId ==. txOut ^. TxOutTxId) &&. (txIn ^. TxInTxOutIndex ==. txOut ^. TxOutIndex))
              where_ (txIn ^. TxInTxInId ==. val txid)
              pure (txOut ^. TxOutAddress, txOut ^. TxOutValue, tx ^. TxHash, txOut ^. TxOutIndex)
    pure $ map convert rows
  where
    convert :: (Value Text, Value Word64, Value ByteString, Value Word16) -> CTxAddressBrief
    convert (Value addr, Value amount, Value txhash, Value index) =
      CTxAddressBrief
        { ctaAddress = CAddress addr
        , ctaAmount = mkCCoin $ fromIntegral amount
        , ctaTxHash = CTxHash $ CHash (bsBase16Encode txhash)
        , ctaTxIndex = fromIntegral index
        }
