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

import           Cardano.Prelude

import           Cardano.Binary (serialize')
import           Cardano.BM.Trace (Trace, logDebug, logInfo)

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (firstExceptT)

import qualified Cardano.Binary as Binary

-- Import all 'cardano-ledger' functions and data types qualified so they do not
-- clash with the Cardano.Db functions and data types which are also imported
-- qualified.
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Update as Byron hiding (protocolVersion)
import qualified Cardano.Chain.UTxO as Byron

import qualified Cardano.Crypto as Crypto (serializeCborHash)

import           Cardano.Db (DbLovelace (..))
import           Cardano.DbSync.Era.Util (liftLookupFail)

import           Cardano.DbSync.Types

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Api
import           Cardano.DbSync.Cache
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
    :: (MonadBaseControl IO m, MonadIO m)
    => SyncEnv -> Bool -> ByronBlock -> SlotDetails
    -> ReaderT SqlBackend m (Either SyncNodeError ())
insertByronBlock env firstBlockOfEpoch blk details = do
    res <- runExceptT $
              case byronBlockRaw blk of
                Byron.ABOBBlock ablk -> insertABlock tracer firstBlockOfEpoch ablk details
                Byron.ABOBBoundary abblk -> insertABOBBoundary tracer cache abblk details
    -- Serializing things during syncing can drastically slow down full sync
    -- times (ie 10x or more).
    when (getSyncStatus details == SyncFollowing)
      DB.transactionCommit
    pure res
  where
    tracer :: Trace IO Text
    tracer = getTrace env

    cache :: Cache
    cache = envCache env

insertABOBBoundary
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Cache -> Byron.ABoundaryBlock ByteString -> SlotDetails
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary tracer cache blk details = do
  -- Will not get called in the OBFT part of the Byron era.
  slid <- lift . DB.insertSlotLeader $
                  DB.SlotLeader
                    { DB.slotLeaderHash = BS.replicate 28 '\0'
                    , DB.slotLeaderPoolHashId = Nothing
                    , DB.slotLeaderDescription = "Epoch boundary slot leader"
                    }
  void . lift . insertBlockAndCache cache $
            DB.Block
              { DB.blockHash = Byron.unHeaderHash $ Byron.boundaryHashAnnotated blk
              , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
              -- No slotNo for a boundary block
              , DB.blockSlotNo = Nothing
              , DB.blockEpochSlotNo = Nothing
              , DB.blockBlockNo = 0
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
              , DB.blockOpCertCounter = Nothing
              }

  liftIO . logInfo tracer $
        Text.concat
          [ "insertABOBBoundary: epoch "
          , textShow (Byron.boundaryEpoch $ Byron.boundaryHeader blk)
          , ", hash "
          , Byron.renderAbstractHash (Byron.boundaryHashAnnotated blk)
          ]

insertABlock
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Bool -> Byron.ABlock ByteString -> SlotDetails
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABlock tracer firstBlockOfEpoch blk details = do
    slid <- lift . DB.insertSlotLeader $ Byron.mkSlotLeader blk
    let blkNo = Byron.blockNumber blk
    void . lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = Byron.blockHash blk
                    , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
                    , DB.blockSlotNo = Just $ Byron.slotNumber blk
                    , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
                    , DB.blockBlockNo = fromIntegral blkNo
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = fromIntegral $ Byron.blockLength blk
                    , DB.blockTime = sdSlotTime details
                    , DB.blockTxCount = fromIntegral $ length (Byron.blockPayload blk)
                    , DB.blockProtoMajor = Byron.pvMajor (Byron.protocolVersion blk)
                    , DB.blockProtoMinor = Byron.pvMinor (Byron.protocolVersion blk)
                    -- Shelley specific
                    , DB.blockVrfKey = Nothing
                    , DB.blockOpCert = Nothing
                    , DB.blockOpCertCounter = Nothing
                    }

    zipWithM_ (insertTx tracer (BlockNo blkNo)) (Byron.blockPayload blk) [ 0 .. ]

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          followingClosely = getSyncStatus details == SyncFollowing

      when (followingClosely && slotWithinEpoch /= 0 && blkNo `mod` 20 == 0) $ do
        logInfo tracer $
          mconcat
            [ "insertByronBlock: continuing epoch ", textShow epoch
            , " (slot ", textShow slotWithinEpoch , "/"
            , textShow (unEpochSize $ sdEpochSize details), ")"
            ]
      logger followingClosely tracer $ mconcat
        [ "insertByronBlock: epoch ", textShow (unEpochNo $ sdEpochNo details)
        , ", slot ", textShow (Byron.slotNumber blk)
        , ", block ", textShow blkNo
        , ", hash ", renderByteArray (Byron.blockHash blk)
        ]
  where
    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | firstBlockOfEpoch = logInfo
      | followingClosely = logInfo
      | Byron.blockNumber blk `mod` 1000 == 0 = logInfo
      | otherwise = logDebug


insertTx
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> BlockNo -> Byron.TxAux -> Word64
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTx tracer blkNo tx blockIndex = do
    resolvedInputs <- mapM resolveTxInputs (toList $ Byron.txInputs (Byron.taTx tx))
    valFee <- firstExceptT annotateTx $ ExceptT $ pure (calculateTxFee (Byron.taTx tx) resolvedInputs)
    txId <- lift . DB.insertTx $
              DB.Tx
                { DB.txHash = Byron.unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
                , DB.txBlockNo = fromIntegral $ unBlockNo blkNo
                , DB.txBlockIndex = blockIndex
                , DB.txOutSum = vfValue valFee
                , DB.txFee = vfFee valFee
                , DB.txDeposit = 0 -- Byron does not have deposits/refunds
                -- Would be really nice to have a way to get the transaction size
                -- without re-serializing it.
                , DB.txSize = fromIntegral $ BS.length (serialize' $ Byron.taTx tx)
                , DB.txInvalidHereafter = Nothing
                , DB.txInvalidBefore = Nothing
                , DB.txValidContract = True
                , DB.txScriptSize = 0
                }

    -- Insert outputs for a transaction before inputs in case the inputs for this transaction
    -- references the output (not sure this can even happen).
    lift $ zipWithM_ (insertTxOut tracer blkNo txId) [0 ..] (toList . Byron.txOutputs $ Byron.taTx tx)
    mapMVExceptT (insertTxIn tracer blkNo txId) resolvedInputs
  where
    annotateTx :: SyncNodeError -> SyncNodeError
    annotateTx ee =
      case ee of
        NEInvariant loc ei -> NEInvariant loc (annotateInvariantTx (Byron.taTx tx) ei)
        _other -> ee

insertTxOut
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> BlockNo -> DB.TxId -> Word32 -> Byron.TxOut
    -> ReaderT SqlBackend m ()
insertTxOut _tracer blkNo txId index txout =
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = fromIntegral index
              , DB.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
              , DB.txOutAddressRaw = Binary.serialize' (Byron.txOutAddress txout)
              , DB.txOutAddressHasScript = False
              , DB.txOutPaymentCred = Nothing -- Byron does not have a payment credential.
              , DB.txOutStakeAddressId = Nothing -- Byron does not have a stake address.
              , DB.txOutValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
              , DB.txOutDataHash = Nothing
              , DB.txOutInlineDatumId = Nothing
              , DB.txOutReferenceScriptId = Nothing
              , DB.txOutBlockNo = fromIntegral $ unBlockNo blkNo
              }


insertTxIn
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> BlockNo -> DB.TxId -> (Byron.TxIn, DB.TxId, DbLovelace)
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertTxIn _tracer blkNo txInId (Byron.TxInUtxo _txHash inIndex, txOutId, _lovelace) = do
  void . lift . DB.insertTxIn $
            DB.TxIn
              { DB.txInTxInId = txInId
              , DB.txInTxOutId = txOutId
              , DB.txInTxOutIndex = fromIntegral inIndex
              , DB.txInRedeemerId = Nothing
              , DB.txInBlockNo = fromIntegral $ unBlockNo blkNo
              }

-- -----------------------------------------------------------------------------

resolveTxInputs :: MonadIO m => Byron.TxIn -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Byron.TxIn, DB.TxId, DbLovelace)
resolveTxInputs txIn@(Byron.TxInUtxo txHash index) = do
    res <- liftLookupFail "resolveInput" $ DB.queryTxOutValue (Byron.unTxHash txHash, fromIntegral index)
    pure $ convert res
  where
    convert :: (DB.TxId, DbLovelace) -> (Byron.TxIn, DB.TxId, DbLovelace)
    convert (txId, lovelace) = (txIn, txId, lovelace)

calculateTxFee :: Byron.Tx -> [(Byron.TxIn, DB.TxId, DbLovelace)] -> Either SyncNodeError ValueFee
calculateTxFee tx resolvedInputs = do
      outval <- first (\e -> NEError $ "calculateTxFee: " <> textShow e) output
      when (null resolvedInputs) $
        Left $ NEError "calculateTxFee: List of transaction inputs is zero."
      let inval = sum $ map (unDbLovelace . thrd3) resolvedInputs
      if inval < outval
        then Left $ NEInvariant "calculateTxFee" $ EInvInOut inval outval
        else Right $ ValueFee (DbLovelace outval) (DbLovelace $ inval - outval)
  where
    output :: Either Byron.LovelaceError Word64
    output =
      Byron.unsafeGetLovelace
        <$> Byron.sumLovelace (map Byron.txOutValue $ Byron.txOutputs tx)

-- | An 'ExceptT' version of 'mapM_' which will 'left' the first 'Left' it finds.
mapMVExceptT :: Monad m => (a -> ExceptT e m ()) -> [a] -> ExceptT e m ()
mapMVExceptT action xs =
  case xs of
    [] -> pure ()
    (y:ys) -> action y >> mapMVExceptT action ys
