{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Byron.Insert
  ( insertByronBlock
  ) where

import           Cardano.BM.Trace (Trace, logDebug, logInfo)
import           Cardano.Binary (serialize')

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)

import qualified Cardano.Binary as Binary

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron hiding (protocolVersion)

import qualified Cardano.Crypto as Crypto (serializeCborHash)

import           Cardano.Db (DbLovelace (..))

import           Cardano.DbSync.Database
import           Cardano.DbSync.Types

import           Cardano.Prelude

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !DbLovelace
  , vfFee :: !DbLovelace
  }

insertByronBlock
    :: Trace IO Text -> Cache -> ByronBlock -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either DbSyncNodeError ())
insertByronBlock tracer cache blk details = do
  res <- runExceptT $
            case byronBlockRaw blk of
              Byron.ABOBBlock ablk -> insertABlock tracer cache ablk details
              Byron.ABOBBoundary abblk -> insertABOBBoundary tracer abblk details
  -- Serializiing things during syncing can drastically slow down full sync
  -- times (ie 10x or more).
  when (getSyncStatus details == SyncFollowing)
    DB.transactionCommit
  pure res


insertABOBBoundary
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Byron.ABoundaryBlock ByteString -> SlotDetails
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary tracer blk details = do
  let prevHash = case Byron.boundaryPrevHash (Byron.boundaryHeader blk) of
                    Left gh -> Byron.genesisToHeaderHash gh
                    Right hh -> hh
  pbid <- liftLookupFail "insertABOBBoundary" $ DB.queryBlockId (Byron.unHeaderHash prevHash)
  slid <- lift . DB.insertSlotLeader $
                  DB.SlotLeader
                    { DB.slotLeaderHash = BS.replicate 28 '\0'
                    , DB.slotLeaderPoolHashId = Nothing
                    , DB.slotLeaderDescription = "Epoch boundary slot leader"
                    }
  void . lift . DB.insertBlock $
            DB.Block
              { DB.blockHash = Byron.unHeaderHash $ Byron.boundaryHashAnnotated blk
              , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
              -- No slotNo for a boundary block
              , DB.blockSlotNo = Nothing
              , DB.blockEpochSlotNo = Nothing
              , DB.blockBlockNo = Nothing
              , DB.blockPreviousId = Just pbid
              -- No merkleRoot for a boundary block
              , DB.blockMerkleRoot = Nothing
              , DB.blockSlotLeaderId = slid
              , DB.blockSize = fromIntegral $ Byron.boundaryBlockLength blk
              , DB.blockTime = sdSlotTime details
              , DB.blockTxCount = 0
              -- EBBs do not seem to have protocol version fields, so set this to '0'.
              , DB.blockProtoMajor = 0
              , DB.blockProtoMinor = 0
              -- Shelley specific
              , DB.blockVrfKey = Nothing
              , DB.blockOpCert = Nothing
              }

  liftIO . logInfo tracer $
        Text.concat
          [ "insertABOBBoundary: epoch "
          , textShow (Byron.boundaryEpoch $ Byron.boundaryHeader blk)
          , " hash "
          , Byron.renderAbstractHash (Byron.boundaryHashAnnotated blk)
          ]

insertABlock
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Cache -> Byron.ABlock ByteString -> SlotDetails
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertABlock tracer cache blk details = do
    blkId <- withCachedPrevBlock
      "insertABlock"
      cache
      (Byron.unHeaderHash $ Byron.blockPreviousHash blk)
      $ \pbid -> do
          slid <- lift . DB.insertSlotLeader $ Byron.mkSlotLeader blk
          blkId <- lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Byron.blockHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ Byron.slotNumber blk
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
                    , DB.blockBlockNo = Just $ Byron.blockNumber blk
                    , DB.blockPreviousId = Just pbid
                    , DB.blockMerkleRoot = Just $ Byron.unCryptoHash (Byron.blockMerkleRoot blk)
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = fromIntegral $ Byron.blockLength blk
                    , DB.blockTime = sdSlotTime details
                    , DB.blockTxCount = fromIntegral $ length (Byron.blockPayload blk)
                    , DB.blockProtoMajor = Byron.pvMajor (Byron.protocolVersion blk)
                    , DB.blockProtoMinor = Byron.pvMinor (Byron.protocolVersion blk)
                    -- Shelley specific
                    , DB.blockVrfKey = Nothing
                    , DB.blockOpCert = Nothing
                    }
          return (blkId, Byron.blockHash blk)

    zipWithM_ (insertTx tracer blkId) (Byron.blockPayload blk) [ 0 .. ]

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          followingClosely = getSyncStatus details == SyncFollowing

      when (followingClosely && slotWithinEpoch /= 0 && Byron.blockNumber blk `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertByronBlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ "insertByronBlock: slot ", textShow (Byron.slotNumber blk)
        , ", block ", textShow (Byron.blockNumber blk)
        , ", hash ", renderByteArray (Byron.blockHash blk)
        ]
  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | followingClosely = logInfo
      | Byron.blockNumber blk `mod` 5000 == 0 = logInfo
      | otherwise = logDebug


insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.BlockId -> Byron.TxAux -> Word64
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkId tx blockIndex = do
    valFee <- firstExceptT annotateTx $ newExceptT (calculateTxFee $ Byron.taTx tx)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Byron.unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
                , DB.txBlockId = blkId
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = vfValue valFee
                , DB.txFee = vfFee valFee
                , DB.txDeposit = 0 -- Byron does not have deposits/refunds
                -- Would be really nice to have a way to get the transaction size
                -- without re-serializing it.
                , DB.txSize = fromIntegral $ BS.length (serialize' $ Byron.taTx tx)
                , DB.txInvalidHereafter = Nothing
                , DB.txInvalidBefore = Nothing
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    lift $ zipWithM_ (insertTxOut tracer txId) [0 ..] (toList . Byron.txOutputs $ Byron.taTx tx)
    mapMVExceptT (insertTxIn tracer txId) (toList . Byron.txInputs $ Byron.taTx tx)
  where
    annotateTx :: DbSyncNodeError -> DbSyncNodeError
    annotateTx ee =
      case ee of
        NEInvariant loc ei -> NEInvariant loc (annotateInvariantTx (Byron.taTx tx) ei)
        _other -> ee

insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Word32 -> Byron.TxOut
    -> ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout =
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
              , DB.txOutAddressRaw = Binary.serialize' (Byron.txOutAddress txout)
              , DB.txOutPaymentCred = Nothing -- Byron does not have a payment credential.
              , DB.txOutStakeAddressId = Nothing -- Byron does not have a stake address.
              , DB.txOutValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
              }


insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> DB.TxId -> Byron.TxIn
    -> ExceptT DbSyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer txInId (Byron.TxInUtxo txHash inIndex) = do
  txOutId <- liftLookupFail "insertTxIn" $ DB.queryTxId (Byron.unTxHash txHash)
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              }

-- -----------------------------------------------------------------------------

calculateTxFee :: (MonadBaseControl IO m, MonadIO m) => Byron.Tx -> ReaderT SqlBackend m (Either DbSyncNodeError ValueFee)
calculateTxFee tx =
    runExceptT $ do
      outval <- firstExceptT (\e -> NEError $ "calculateTxFee: " <> textShow e) $ hoistEither output
      when (null inputs) $
        dbSyncNodeError "calculateTxFee: List of transaction inputs is zero."
      inval <- sum . map unDbLovelace <$> mapMExceptT (liftLookupFail "calculateTxFee" . DB.queryTxOutValue) inputs
      if inval < outval
        then dbSyncInvariant "calculateTxFee" $ EInvInOut inval outval
        else pure $ ValueFee (DbLovelace outval) (DbLovelace $ inval - outval)
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

-- | An 'ExceptT' version of 'mapM_' which will 'left' the first 'Left' it finds.
mapMVExceptT :: Monad m => (a -> ExceptT e m ()) -> [a] -> ExceptT e m ()
mapMVExceptT action xs =
  case xs of
    [] -> pure ()
    (y:ys) -> action y >> mapMVExceptT action ys
