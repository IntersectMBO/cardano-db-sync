{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Plugin.Default.Insert
  ( insertByronBlock
  ) where

import           Cardano.Binary (serialize')
import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT,
                    runExceptT)

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron

import qualified Cardano.Crypto as Crypto (serializeCborHash)

import           Cardano.Prelude

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))
import           Ouroboros.Network.Block (BlockNo (..), Tip, getTipBlockNo)
import           Ouroboros.Network.Point (withOrigin)

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !Word64
  , vfFee :: !Word64
  }

insertByronBlock
    :: Trace IO Text -> ByronBlock -> Tip ByronBlock
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertByronBlock tracer blk tip = do
  runExceptT $
    case byronBlockRaw blk of
      Byron.ABOBBlock ablk -> insertABlock tracer ablk tip
      Byron.ABOBBoundary abblk -> insertABOBBoundary tracer abblk

insertABOBBoundary
    :: MonadIO m
    => Trace IO Text -> Byron.ABoundaryBlock ByteString
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary tracer blk = do
  let prevHash = case Byron.boundaryPrevHash (Byron.boundaryHeader blk) of
                    Left gh -> Byron.genesisToHeaderHash gh
                    Right hh -> hh
  meta <- liftLookupFail "insertABOBBoundary" DB.queryMeta
  pbid <- liftLookupFail "insertABOBBoundary" $ DB.queryBlockId (Byron.unHeaderHash prevHash)
  mle <- liftLookupFail "insertABOBBoundary: " $ DB.queryEpochNo pbid
  slid <- lift . DB.insertSlotLeader $ DB.SlotLeader (BS.replicate 28 '\0') "Epoch boundary slot leader"
  void . lift . DB.insertBlock $
            DB.Block
              { DB.blockHash = Byron.unHeaderHash $ Byron.boundaryHashAnnotated blk
              , DB.blockEpochNo = Just $ maybe 0 (+1) mle
              , DB.blockSlotNo = Nothing -- No slotNo for a boundary block
              , DB.blockBlockNo = Nothing
              , DB.blockPrevious = Just pbid
              , DB.blockMerkelRoot = Nothing -- No merkelRoot for a boundary block
              , DB.blockSlotLeader = slid
              , DB.blockSize = fromIntegral $ Byron.boundaryBlockLength blk
              , DB.blockTime = DB.epochUtcTime meta (maybe 0 (+1) mle)
              , DB.blockTxCount = 0
              }

  liftIO . logInfo tracer $
        Text.concat
          [ "insertABOBBoundary: epoch "
          , textShow (Byron.boundaryEpoch $ Byron.boundaryHeader blk)
          , " hash "
          , Byron.renderAbstractHash (Byron.boundaryHashAnnotated blk)
          ]

insertABlock
    :: MonadIO m
    => Trace IO Text -> Byron.ABlock ByteString -> Tip ByronBlock
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertABlock tracer blk tip = do
    meta <- liftLookupFail "insertABlock" DB.queryMeta
    pbid <- liftLookupFail "insertABlock" $ DB.queryBlockId (Byron.unHeaderHash $ Byron.blockPreviousHash blk)

    let slotsPerEpoch = 10 * DB.metaProtocolConst meta

    slid <- lift . DB.insertSlotLeader $ Byron.mkSlotLeader blk
    blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Byron.blockHash blk
                    , DB.blockEpochNo = Just $ Byron.slotNumber blk `div` slotsPerEpoch
                    , DB.blockSlotNo = Just $ Byron.slotNumber blk
                    , DB.blockBlockNo = Just $ Byron.blockNumber blk
                    , DB.blockPrevious = Just pbid
                    , DB.blockMerkelRoot = Just $ Byron.unCryptoHash (Byron.blockMerkelRoot blk)
                    , DB.blockSlotLeader = slid
                    , DB.blockSize = fromIntegral $ Byron.blockLength blk
                    , DB.blockTime = DB.slotUtcTime meta (Byron.slotNumber blk)
                    , DB.blockTxCount = fromIntegral $ length (Byron.blockPayload blk)
                    }

    zipWithM_ (insertTx tracer blkId) (Byron.blockPayload blk) [ 0 .. ]

    liftIO $ do
      let followingClosely = withOrigin 0 unBlockNo (getTipBlockNo tip) - Byron.blockNumber blk < 20
          (epoch, slotWithinEpoch) = Byron.slotNumber blk `divMod` slotsPerEpoch
      when (followingClosely && slotWithinEpoch /= 0 && Byron.slotNumber blk > 0 && Byron.slotNumber blk `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertABlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch, ")"
            ]
      logger tracer $ mconcat
        [ "insertABlock: slot ", textShow (Byron.slotNumber blk)
        , ", block ", textShow (Byron.blockNumber blk)
        , ", hash ", Byron.renderByteArray (Byron.blockHash blk)
        ]
  where
    logger :: Trace IO a -> a -> IO ()
    logger
      | withOrigin 0 unBlockNo (getTipBlockNo tip) - Byron.blockNumber blk < 20 = logInfo
      | Byron.slotNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug


insertTx
    :: MonadIO m
    => Trace IO Text -> DB.BlockId -> Byron.TxAux -> Word64
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId tx blockIndex = do
    valFee <- firstExceptT annotateTx $ newExceptT (calculateTxFee $ Byron.taTx tx)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Byron.unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
                , DB.txBlock = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = vfValue valFee
                , DB.txFee = vfFee valFee
                -- Would be really nice to have a way to get the transaction size
                -- without re-serializing it.
                , DB.txSize = fromIntegral $ BS.length (serialize' $ Byron.taTx tx)
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    lift $ zipWithM_ (insertTxOut tracer txId) [0 ..] (toList . Byron.txOutputs $ Byron.taTx tx)
    void $ zipWithM_ (insertTxIn tracer txId) [0 ..] (toList . Byron.txInputs $ Byron.taTx tx)
  where
    annotateTx :: DbSyncNodeError -> DbSyncNodeError
    annotateTx ee =
      case ee of
        NEInvariant loc ei -> NEInvariant loc (annotateInvariantTx (Byron.taTx tx) ei)
        _other -> ee

insertTxOut
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> Word16 -> Byron.TxOut
    -> ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout =
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
              , DB.txOutValue = Byron.unsafeGetLovelace $ Byron.txOutValue txout
              }


insertTxIn
    :: MonadIO m
    => Trace IO Text -> DB.TxId -> Word16 -> Byron.TxIn
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId txInIndex (Byron.TxInUtxo txHash inIndex) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId (Byron.unTxHash txHash)
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxIndex = txInIndex
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              }

-- -----------------------------------------------------------------------------

calculateTxFee :: MonadIO m => Byron.Tx -> ReaderT SqlBackend m (Either DbSyncNodeError ValueFee)
calculateTxFee tx =
    runExceptT $ do
      outval <- firstExceptT (\e -> NEError $ "calculateTxFee: " <> textShow e) $ hoistEither output
      when (null inputs) $
        dbSyncNodeError "calculateTxFee: List of transaction inputs is zero."
      inval <- sum <$> mapMExceptT (liftLookupFail "calculateTxFee" . DB.queryTxOutValue) inputs
      if inval < outval
        then dbSyncInvariant "calculateTxFee" $ EInvInOut inval outval
        else pure $ ValueFee outval (inval - outval)
  where
    -- [(Hash of tx, index within tx)]
    inputs :: [(ByteString, Word16)]
    inputs = map unpack $ toList (Byron.txInputs tx)

    unpack :: Byron.TxIn -> (ByteString, Word16)
    unpack (Byron.TxInUtxo txHash index) = (Byron.unTxHash txHash, fromIntegral index)

    output :: Either Byron.LovelaceError Word64
    output =
      Byron.unsafeGetLovelace
        <$> Byron.sumLovelace (map Byron.txOutValue $ Byron.txOutputs tx)

-- | An 'ExceptT' version of 'mapM' which will 'left' the first 'Left' it finds.
mapMExceptT :: Monad m => (a -> ExceptT e m b) -> [a] -> ExceptT e m [b]
mapMExceptT action xs =
  case xs of
    [] -> pure []
    (y:ys) -> (:) <$> action y <*> mapMExceptT action ys

