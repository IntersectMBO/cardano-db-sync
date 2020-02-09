{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node.Plugin.Default.Insert
  ( insertByronBlock
  ) where

import           Cardano.Binary (serialize')
import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT,
                    runExceptT)

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Explorer DB functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Ledger
import qualified Cardano.Chain.Common as Ledger
import qualified Cardano.Chain.UTxO as Ledger

import qualified Cardano.Crypto as Crypto

import           Cardano.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Explorer.DB as DB
import           Explorer.Node.Error
import           Explorer.Node.Util

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..))

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !Word64
  , vfFee :: !Word64
  }

insertByronBlock
    :: Trace IO Text -> ByronBlock -> BlockNo
    -> ReaderT SqlBackend (NoLoggingT IO) (Either ExplorerNodeError ())
insertByronBlock tracer blk tipBlockNo =
  runExceptT $
    case byronBlockRaw blk of
      Ledger.ABOBBlock ablk -> insertABlock tracer ablk tipBlockNo
      Ledger.ABOBBoundary abblk -> insertABOBBoundary tracer abblk

insertABOBBoundary
    :: MonadIO m
    => Trace IO Text -> Ledger.ABoundaryBlock ByteString
    -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary tracer blk = do
  let prevHash = case Ledger.boundaryPrevHash (Ledger.boundaryHeader blk) of
                    Left gh -> genesisToHeaderHash gh
                    Right hh -> hh
  meta <- liftLookupFail "insertABOBBoundary" DB.queryMeta
  pbid <- liftLookupFail "insertABOBBoundary" $ DB.queryBlockId (unHeaderHash prevHash)
  mle <- liftLookupFail "insertABOBBoundary: " $ DB.queryEpochNo pbid
  slid <- lift . DB.insertSlotLeader $ DB.SlotLeader (BS.replicate 28 '\0') "Epoch boundary slot leader"
  void . lift . DB.insertBlock $
            DB.Block
              { DB.blockHash = unHeaderHash $ Ledger.boundaryHashAnnotated blk
              , DB.blockEpochNo = Just $ maybe 0 (+1) mle
              , DB.blockSlotNo = Nothing -- No slotNo for a boundary block
              , DB.blockBlockNo = Nothing
              , DB.blockPrevious = Just pbid
              , DB.blockMerkelRoot = Nothing -- No merkelRoot for a boundary block
              , DB.blockSlotLeader = slid
              , DB.blockSize = fromIntegral $ Ledger.boundaryBlockLength blk
              , DB.blockTime = DB.epochUtcTime meta (maybe 0 (+1) mle)
              , DB.blockTxCount = 0
              }
  supply <- lift DB.queryTotalSupply
  liftIO $ do
      logInfo tracer $ Text.concat
                [ "Total supply at start of epoch ", textShow (boundaryEpochNumber blk)
                , " is ", DB.renderAda supply, " Ada"
                ]
      logInfo tracer $ Text.concat
                  [ "insertABOBBoundary: epoch "
                  , textShow (Ledger.boundaryEpoch $ Ledger.boundaryHeader blk)
                  , " hash "
                  , renderAbstractHash (Ledger.boundaryHashAnnotated blk)
                  ]

insertABlock
    :: MonadIO m
    => Trace IO Text -> Ledger.ABlock ByteString -> BlockNo
    -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
insertABlock tracer blk (BlockNo tipBlockNo) = do
    meta <- liftLookupFail "insertABlock" DB.queryMeta
    pbid <- liftLookupFail "insertABlock" $ DB.queryBlockId (unHeaderHash $ blockPreviousHash blk)

    let slotsPerEpoch = 10 * DB.metaProtocolConst meta

    slid <- lift . DB.insertSlotLeader $ mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = blockHash blk
                    , DB.blockEpochNo = Just $ slotNumber blk `div` slotsPerEpoch
                    , DB.blockSlotNo = Just $ slotNumber blk
                    , DB.blockBlockNo = Just $ blockNumber blk
                    , DB.blockPrevious = Just pbid
                    , DB.blockMerkelRoot = Just $ unCryptoHash (blockMerkelRoot blk)
                    , DB.blockSlotLeader = slid
                    , DB.blockSize = fromIntegral $ Ledger.blockLength blk
                    , DB.blockTime = DB.slotUtcTime meta (slotNumber blk)
                    , DB.blockTxCount = fromIntegral $ length (blockPayload blk)
                    }

    mapMVExceptT (insertTx tracer blkId) $ blockPayload blk

    liftIO $ do
      when (slotNumber blk > 0 && slotNumber blk `mod` 20 == 0) $
        logger tracer $ "insertABlock: continuing epoch " <> textShow (slotNumber blk `div` slotsPerEpoch)
      logger tracer $ mconcat
        [ "insertABlock: slot ", textShow (slotNumber blk)
        , ", block ", textShow (blockNumber blk)
        , ", hash ", renderAbstractHash (blockHash blk)
        ]
  where
    logger :: Trace IO a -> a -> IO ()
    logger =
      if tipBlockNo - blockNumber blk < 20
        then logInfo
        else logDebug


insertTx
    :: MonadIO m
    => Trace IO Text -> DB.BlockId -> Ledger.TxAux
    -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId tx = do
    valFee <- firstExceptT annotateTx $ newExceptT (calculateTxFee $ Ledger.taTx tx)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = unTxHash $ Crypto.hash (Ledger.taTx tx)
                , DB.txBlock = blkId
                , DB.txOutSum = vfValue valFee
                , DB.txFee = vfFee valFee
                -- Would be really nice to have a way to get the transaction size
                -- without re-serializing it.
                , DB.txSize = fromIntegral $ BS.length (serialize' $ Ledger.taTx tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    lift $ zipWithM_ (insertTxOut tracer txId) [0 ..] (toList . Ledger.txOutputs $ Ledger.taTx tx)
    mapMVExceptT (insertTxIn tracer txId) (toList . Ledger.txInputs $ Ledger.taTx tx)
  where
    annotateTx :: ExplorerNodeError -> ExplorerNodeError
    annotateTx ee =
      case ee of
        ENEInvariant loc ei -> ENEInvariant loc (annotateInvariantTx (Ledger.taTx tx) ei)
        _other -> ee

insertTxOut
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> Word32 -> Ledger.TxOut
    -> ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout =
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Ledger.addrToBase58 (Ledger.txOutAddress txout)
              , DB.txOutValue = Ledger.unsafeGetLovelace $ Ledger.txOutValue txout
              }


insertTxIn
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> Ledger.TxIn
    -> ExceptT ExplorerNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Ledger.TxInUtxo txHash inIndex) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId (unTxHash txHash)
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              }

-- -----------------------------------------------------------------------------

calculateTxFee :: MonadIO m => Ledger.Tx -> ReaderT SqlBackend m (Either ExplorerNodeError ValueFee)
calculateTxFee tx =
    runExceptT $ do
      outval <- firstExceptT (\e -> ENEError $ "calculateTxFee: " <> textShow e) $ hoistEither output
      when (null inputs) $
        explorerError "calculateTxFee: List of transaction inputs is zero."
      inval <- sum <$> mapMExceptT (liftLookupFail "calculateTxFee" . DB.queryTxOutValue) inputs
      if inval < outval
        then explorerInvariant "calculateTxFee" $ EInvInOut inval outval
        else pure $ ValueFee outval (inval - outval)
  where
    -- [(Hash of tx, index within tx)]
    inputs :: [(ByteString, Word16)]
    inputs = map unpack $ toList (Ledger.txInputs tx)

    unpack :: Ledger.TxIn -> (ByteString, Word16)
    unpack (Ledger.TxInUtxo txHash index) = (unTxHash txHash, fromIntegral index)

    output :: Either Ledger.LovelaceError Word64
    output =
      Ledger.unsafeGetLovelace
        <$> Ledger.sumLovelace (map Ledger.txOutValue $ Ledger.txOutputs tx)

-- | An 'ExceptT' version of 'mapM' which will 'left' the first 'Left' it finds.
mapMExceptT :: Monad m => (a -> ExceptT e m b) -> [a] -> ExceptT e m [b]
mapMExceptT action xs =
  case xs of
    [] -> pure []
    (y:ys) -> (:) <$> action y <*> mapMExceptT action ys

-- | An 'ExceptT' version of 'mapM_' which will 'left' the first 'Left' it finds.
mapMVExceptT :: Monad m => (a -> ExceptT e m ()) -> [a] -> ExceptT e m ()
mapMVExceptT action xs =
  case xs of
    [] -> pure ()
    (y:ys) -> action y >> mapMVExceptT action ys
