{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Explorer.Web.Server.TxsSummary
  ( txsSummary
  ) where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Control.Monad.Trans.Except.Extra (hoistEither)

import           Data.ByteString (ByteString)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Word (Word64)

import           Database.Esqueleto (InnerJoin (..), (^.), (==.), (&&.),
                    entityVal, from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (Block (..), BlockId, EntityField (..), LookupFail (..), Tx (..),
                    TxId, TxOut (..),
                    entityPair, maybeToEither, unValue2)

import           Explorer.Web.ClientTypes (CAddress (..),
                    CHash (..), CCoin, CTxHash (..), CTxSummary (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Server.Util

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
txsSummary backend (CTxHash (CHash hash)) =
  runExceptT $ do
    blob <- hoistEither $ textBase16Decode hash
    mTxblk <- runQuery backend $ queryTxSummary blob
    case mTxblk of
      Nothing -> throwE $ Internal "tx not found" -- TODO, give the same error as before?
      Just (tx, blk, inputs, outputs) -> do
        case blockSlotNo blk of
          Just slotno -> do
            let (epoch, slot) = slotno `divMod` slotsPerEpoch
            pure $ CTxSummary
              { ctsId              = CTxHash . CHash . bsBase16Encode $ txHash tx
              , ctsTxTimeIssued    = Nothing
              , ctsBlockTimeIssued = Just $ blockPosixTime blk
              , ctsBlockHeight     = fromIntegral <$> blockBlockNo blk
              , ctsBlockEpoch      = Just epoch
              , ctsBlockSlot       = Just $ fromIntegral slot
              , ctsBlockHash       = Just . CHash . bsBase16Encode $ blockHash blk
              , ctsRelayedBy       = Nothing
              , ctsTotalInput      = (mkCCoin . sum . map (\(_addr,coin) -> fromIntegral  coin)) inputs
              , ctsTotalOutput     = (mkCCoin . sum . map (fromIntegral . txOutValue)) outputs
              , ctsFees            = mkCCoin $ fromIntegral $ txFee tx
              , ctsInputs          = map convertInput inputs
              , ctsOutputs         = map convertTxOut outputs
              }
          Nothing -> throwE $ Internal "cant find slot# of block"

queryTxSummary :: MonadIO m => ByteString -> ReaderT SqlBackend m (Maybe (Tx, Block, [(Text, Word64)], [TxOut]))
queryTxSummary txhash = do
  eTx <- queryTx txhash
  case eTx of
    Right (txid, tx) -> do
      mBlock <- queryBlockById (txBlock tx)
      case mBlock of
        Just block -> do
          inputs <- queryGetInputOutputs txid
          outputs <- queryOutputsByTxId txid
          pure $ Just (tx, block, inputs, outputs)
        Nothing -> pure Nothing
    Left _ -> pure Nothing -- TODO


-- queryInputBlockTime :: TxId -> ReaderT SqlBackend m (Maybe POSIXTime)
-- queryInputBlockTime txid

queryTx :: MonadIO m => ByteString -> ReaderT SqlBackend m (Either LookupFail (TxId, Tx))
queryTx hash = do
  res <- select . from $ \ tx -> do
            where_ (tx ^. TxHash ==. val hash)
            pure tx
  pure $ maybeToEither (DbLookupTxHash hash) entityPair (listToMaybe res)

queryOutputsByTxId :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOut]
queryOutputsByTxId txid = do
  rows <- select . from $ \txout -> do
    where_ $ txout ^. TxOutTxId ==. val txid
    pure txout
  pure $ map entityVal rows

queryBlockById :: MonadIO m => BlockId -> ReaderT SqlBackend m (Maybe Block)
queryBlockById blockid = do
  rows <- select . from $ \blk -> do
      where_ $ blk ^. BlockId ==. val blockid
      pure blk
  pure $ fmap entityVal (listToMaybe rows)

queryGetInputOutputs :: MonadIO m => TxId -> ReaderT SqlBackend m [(Text, Word64)]
queryGetInputOutputs txid = do
  rows <- select . from $ \(txin `InnerJoin` txout) -> do
    on ((txin ^. TxInTxOutId ==. txout ^. TxOutTxId) &&. (txin ^. TxInTxOutIndex ==. txout ^. TxOutIndex))
    where_ $ txin ^. TxInTxInId ==. val txid
    pure (txout ^. TxOutAddress, txout ^. TxOutValue)
  pure $ map unValue2 rows



convertTxOut :: TxOut -> (CAddress, CCoin)
convertTxOut TxOut{txOutAddress,txOutValue} = (CAddress txOutAddress, mkCCoin $ fromIntegral txOutValue)

convertInput :: (Text, Word64) -> Maybe (CAddress, CCoin)
convertInput (addr, coin) = Just (CAddress $ addr, mkCCoin $ fromIntegral coin)
