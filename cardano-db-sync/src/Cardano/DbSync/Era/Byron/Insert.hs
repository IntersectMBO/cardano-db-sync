{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Byron.Insert (
  insertByronBlock,
  resolveTxInputs,
) where

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import Cardano.Binary (serialize')
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron hiding (protocolVersion)
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (
  insertBlockAndCache,
  queryPrevBlockWithCache,
 )
import Cardano.DbSync.Cache.Epoch (writeEpochBlockDiffToCache)
import Cardano.DbSync.Cache.Types (CacheStatus (..), EpochBlockDiff (..))
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.Persist.Sql (SqlBackend)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !DbLovelace
  , vfFee :: !DbLovelace
  }

insertByronBlock ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  ByronBlock ->
  SlotDetails ->
  ReaderT SqlBackend m (Either SyncNodeError ())
insertByronBlock syncEnv firstBlockOfEpoch blk details = do
  res <- runExceptT $
    case byronBlockRaw blk of
      Byron.ABOBBlock ablk -> insertABlock syncEnv firstBlockOfEpoch ablk details
      Byron.ABOBBoundary abblk -> insertABOBBoundary syncEnv abblk details
  -- Serializing things during syncing can drastically slow down full sync
  -- times (ie 10x or more).
  when
    (getSyncStatus details == SyncFollowing)
    DB.transactionCommit
  pure res

insertABOBBoundary ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Byron.ABoundaryBlock ByteString ->
  SlotDetails ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABOBBoundary syncEnv blk details = do
  let tracer = getTrace syncEnv
      cache = envCache syncEnv
  -- Will not get called in the OBFT part of the Byron era.
  pbid <- queryPrevBlockWithCache "insertABOBBoundary" cache (Byron.ebbPrevHash blk)
  let epochNo = unEpochNo $ sdEpochNo details
  slid <-
    lift . DB.insertSlotLeader $
      DB.SlotLeader
        { DB.slotLeaderHash = BS.replicate 28 '\0'
        , DB.slotLeaderPoolHashId = Nothing
        , DB.slotLeaderDescription = "Epoch boundary slot leader"
        }
  blkId <-
    lift . insertBlockAndCache cache $
      DB.Block
        { DB.blockHash = Byron.unHeaderHash $ Byron.boundaryHashAnnotated blk
        , DB.blockEpochNo = Just epochNo
        , -- No slotNo for a boundary block
          DB.blockSlotNo = Nothing
        , DB.blockEpochSlotNo = Nothing
        , DB.blockBlockNo = Nothing
        , DB.blockPreviousId = Just pbid
        , DB.blockSlotLeaderId = slid
        , DB.blockSize = fromIntegral $ Byron.boundaryBlockLength blk
        , DB.blockTime = sdSlotTime details
        , DB.blockTxCount = 0
        , -- EBBs do not seem to have protocol version fields, so set this to '0'.
          DB.blockProtoMajor = 0
        , DB.blockProtoMinor = 0
        , -- Shelley specific
          DB.blockVrfKey = Nothing
        , DB.blockOpCert = Nothing
        , DB.blockOpCertCounter = Nothing
        }

  -- now that we've inserted the Block and all it's txs lets cache what we'll need
  -- when we later update the epoch values.
  -- If have --dissable-epoch && --dissable-cache then no need to cache data.
  when (soptEpochAndCacheEnabled $ envOptions syncEnv)
    . newExceptT
    $ writeEpochBlockDiffToCache
      cache
      EpochBlockDiff
        { ebdBlockId = blkId
        , ebdFees = 0
        , ebdOutSum = 0
        , ebdTxCount = 0
        , ebdEpochNo = epochNo
        , ebdTime = sdSlotTime details
        }

  liftIO . logInfo tracer $
    Text.concat
      [ "insertABOBBoundary: epoch "
      , textShow (Byron.boundaryEpoch $ Byron.boundaryHeader blk)
      , ", hash "
      , Byron.renderAbstractHash (Byron.boundaryHashAnnotated blk)
      ]

insertABlock ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  Byron.ABlock ByteString ->
  SlotDetails ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertABlock syncEnv firstBlockOfEpoch blk details = do
  pbid <- queryPrevBlockWithCache "insertABlock" cache (Byron.blockPreviousHash blk)
  slid <- lift . DB.insertSlotLeader $ Byron.mkSlotLeader blk
  let txs = Byron.blockPayload blk
  blkId <-
    lift . insertBlockAndCache cache $
      DB.Block
        { DB.blockHash = Byron.blockHash blk
        , DB.blockEpochNo = Just $ unEpochNo (sdEpochNo details)
        , DB.blockSlotNo = Just $ Byron.slotNumber blk
        , DB.blockEpochSlotNo = Just $ unEpochSlot (sdEpochSlot details)
        , DB.blockBlockNo = Just $ Byron.blockNumber blk
        , DB.blockPreviousId = Just pbid
        , DB.blockSlotLeaderId = slid
        , DB.blockSize = fromIntegral $ Byron.blockLength blk
        , DB.blockTime = sdSlotTime details
        , DB.blockTxCount = fromIntegral $ length txs
        , DB.blockProtoMajor = Byron.pvMajor (Byron.protocolVersion blk)
        , DB.blockProtoMinor = Byron.pvMinor (Byron.protocolVersion blk)
        , -- Shelley specific
          DB.blockVrfKey = Nothing
        , DB.blockOpCert = Nothing
        , DB.blockOpCertCounter = Nothing
        }

  txFees <- zipWithM (insertByronTx syncEnv blkId) (Byron.blockPayload blk) [0 ..]
  let byronTxOutValues = concatMap (toList . (\tx -> map Byron.txOutValue (Byron.txOutputs $ Byron.taTx tx))) txs
      outSum = sum $ map Byron.lovelaceToInteger byronTxOutValues

  -- now that we've inserted the Block and all it's txs lets cache what we'll need
  -- when we later update the epoch values.
  -- If have --dissable-epoch && --dissable-cache then no need to cache data.
  when (soptEpochAndCacheEnabled $ envOptions syncEnv)
    . newExceptT
    $ writeEpochBlockDiffToCache
      cache
      EpochBlockDiff
        { ebdBlockId = blkId
        , ebdFees = sum txFees
        , ebdOutSum = fromIntegral outSum
        , ebdTxCount = fromIntegral $ length txs
        , ebdEpochNo = unEpochNo (sdEpochNo details)
        , ebdTime = sdSlotTime details
        }

  liftIO $ do
    let epoch = unEpochNo (sdEpochNo details)
        slotWithinEpoch = unEpochSlot (sdEpochSlot details)
        followingClosely = getSyncStatus details == SyncFollowing

    when (followingClosely && slotWithinEpoch /= 0 && Byron.blockNumber blk `mod` 20 == 0) $ do
      logInfo tracer $
        mconcat
          [ "Insert Byron Block: continuing epoch "
          , textShow epoch
          , " (slot "
          , textShow slotWithinEpoch
          , "/"
          , textShow (unEpochSize $ sdEpochSize details)
          , ")"
          ]
    logger followingClosely tracer $
      mconcat
        [ "Insert Byron Block: epoch "
        , textShow (unEpochNo $ sdEpochNo details)
        , ", slot "
        , textShow (Byron.slotNumber blk)
        , ", block "
        , textShow (Byron.blockNumber blk)
        , ", hash "
        , renderByteArray (Byron.blockHash blk)
        ]
  where
    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    cache :: CacheStatus
    cache = envCache syncEnv

    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | firstBlockOfEpoch = logInfo
      | followingClosely = logInfo
      | Byron.blockNumber blk `mod` 1000 == 0 = logInfo
      | otherwise = logDebug

insertByronTx ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.BlockId ->
  Byron.TxAux ->
  Word64 ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
insertByronTx syncEnv blkId tx blockIndex = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  if disInOut
    then do
      txId <-
        lift . DB.insertTx $
          DB.Tx
            { DB.txHash = Byron.unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
            , DB.txBlockId = blkId
            , DB.txBlockIndex = blockIndex
            , DB.txOutSum = DbLovelace 0
            , DB.txFee = DbLovelace 0
            , DB.txDeposit = Nothing -- Byron does not have deposits/refunds
            -- Would be really nice to have a way to get the transaction size
            -- without re-serializing it.
            , DB.txSize = fromIntegral $ BS.length (serialize' $ Byron.taTx tx)
            , DB.txInvalidHereafter = Nothing
            , DB.txInvalidBefore = Nothing
            , DB.txValidContract = True
            , DB.txScriptSize = 0
            , DB.txTreasuryDonation = DbLovelace 0
            }

      when (ioTxCBOR iopts) $ do
        void
          . lift
          . DB.insertTxCBOR
          $ DB.TxCbor
            { DB.txCborTxId = txId
            , DB.txCborBytes = serialize' $ Byron.taTx tx
            }

      pure 0
    else insertByronTx' syncEnv blkId tx blockIndex
  where
    iopts = getInsertOptions syncEnv

insertByronTx' ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  DB.BlockId ->
  Byron.TxAux ->
  Word64 ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) Word64
insertByronTx' syncEnv blkId tx blockIndex = do
  resolvedInputs <- mapM (resolveTxInputs txOutTableType) (toList $ Byron.txInputs (Byron.taTx tx))
  valFee <- firstExceptT annotateTx $ ExceptT $ pure (calculateTxFee (Byron.taTx tx) resolvedInputs)
  txId <-
    lift . DB.insertTx $
      DB.Tx
        { DB.txHash = Byron.unTxHash $ Crypto.serializeCborHash (Byron.taTx tx)
        , DB.txBlockId = blkId
        , DB.txBlockIndex = blockIndex
        , DB.txOutSum = vfValue valFee
        , DB.txFee = vfFee valFee
        , DB.txDeposit = Just 0 -- Byron does not have deposits/refunds
        -- Would be really nice to have a way to get the transaction size
        -- without re-serializing it.
        , DB.txSize = fromIntegral $ BS.length (serialize' $ Byron.taTx tx)
        , DB.txInvalidHereafter = Nothing
        , DB.txInvalidBefore = Nothing
        , DB.txValidContract = True
        , DB.txScriptSize = 0
        , DB.txTreasuryDonation = DbLovelace 0
        }

  when (ioTxCBOR iopts) $ do
    void
      . lift
      . DB.insertTxCBOR
      $ DB.TxCbor
        { DB.txCborTxId = txId
        , DB.txCborBytes = serialize' $ Byron.taTx tx
        }

  -- Insert outputs for a transaction before inputs in case the inputs for this transaction
  -- references the output (not sure this can even happen).
  disInOut <- liftIO $ getDisableInOutState syncEnv
  lift $ zipWithM_ (insertTxOutByron syncEnv (getHasConsumedOrPruneTxOut syncEnv) disInOut txId) [0 ..] (toList . Byron.txOutputs $ Byron.taTx tx)
  unless (getSkipTxIn syncEnv) $
    mapM_ (insertTxIn tracer txId) resolvedInputs
  whenConsumeOrPruneTxOut syncEnv $
    lift $
      DB.updateListTxOutConsumedByTxId (prepUpdate txId <$> resolvedInputs)
  -- fees are being returned so we can sum them and put them in cache to use when updating epochs
  pure $ unDbLovelace $ vfFee valFee
  where
    txOutTableType = getTxOutTableType syncEnv
    iopts = getInsertOptions syncEnv

    tracer :: Trace IO Text
    tracer = getTrace syncEnv

    annotateTx :: SyncNodeError -> SyncNodeError
    annotateTx ee =
      case ee of
        SNErrInvariant loc ei -> SNErrInvariant loc (annotateInvariantTx (Byron.taTx tx) ei)
        _other -> ee

    prepUpdate txId (_, _, txOutId, _) = (txOutId, txId)

insertTxOutByron ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  Bool ->
  DB.TxId ->
  Word32 ->
  Byron.TxOut ->
  ReaderT SqlBackend m ()
insertTxOutByron syncEnv _hasConsumed bootStrap txId index txout =
  unless bootStrap $
    case ioTxOutTableType . soptInsertOptions $ envOptions syncEnv of
      DB.TxOutCore -> do
        void . DB.insertTxOut $
          DB.CTxOutW $
            C.TxOut
              { C.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
              , C.txOutAddressHasScript = False
              , C.txOutDataHash = Nothing
              , C.txOutConsumedByTxId = Nothing
              , C.txOutIndex = fromIntegral index
              , C.txOutInlineDatumId = Nothing
              , C.txOutPaymentCred = Nothing -- Byron does not have a payment credential.
              , C.txOutReferenceScriptId = Nothing
              , C.txOutStakeAddressId = Nothing -- Byron does not have a stake address.
              , C.txOutTxId = txId
              , C.txOutValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
              }
      DB.TxOutVariantAddress -> do
        addrDetailId <- insertAddress
        void . DB.insertTxOut $ DB.VTxOutW (vTxOut addrDetailId) Nothing
  where
    addrRaw :: ByteString
    addrRaw = serialize' (Byron.txOutAddress txout)

    vTxOut :: V.AddressId -> V.TxOut
    vTxOut addrDetailId =
      V.TxOut
        { V.txOutAddressId = addrDetailId
        , V.txOutConsumedByTxId = Nothing
        , V.txOutDataHash = Nothing
        , V.txOutIndex = fromIntegral index
        , V.txOutInlineDatumId = Nothing
        , V.txOutReferenceScriptId = Nothing
        , V.txOutTxId = txId
        , V.txOutValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
        , V.txOutStakeAddressId = Nothing
        }

    vAddress :: V.Address
    vAddress =
      V.Address
        { V.addressAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
        , V.addressRaw = addrRaw
        , V.addressHasScript = False
        , V.addressPaymentCred = Nothing -- Byron does not have a payment credential.
        , V.addressStakeAddressId = Nothing -- Byron does not have a stake address.
        }

    insertAddress ::
      (MonadBaseControl IO m, MonadIO m) =>
      ReaderT SqlBackend m V.AddressId
    insertAddress = do
      mAddrId <- DB.queryAddressId addrRaw
      case mAddrId of
        Nothing -> DB.insertAddress vAddress
        -- this address is already in the database, so we can just return the id to be linked to the txOut.
        Just addrId -> pure addrId

insertTxIn ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  DB.TxId ->
  (Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace) ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) DB.TxInId
insertTxIn _tracer txInTxId (Byron.TxInUtxo _txHash inIndex, txOutTxId, _, _) = do
  lift . DB.insertTxIn $
    DB.TxIn
      { DB.txInTxInId = txInTxId
      , DB.txInTxOutId = txOutTxId
      , DB.txInTxOutIndex = fromIntegral inIndex
      , DB.txInRedeemerId = Nothing
      }

-- -----------------------------------------------------------------------------

resolveTxInputs :: MonadIO m => DB.TxOutTableType -> Byron.TxIn -> ExceptT SyncNodeError (ReaderT SqlBackend m) (Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace)
resolveTxInputs txOutTableType txIn@(Byron.TxInUtxo txHash index) = do
  res <- liftLookupFail "resolveInput" $ DB.queryTxOutIdValue txOutTableType (Byron.unTxHash txHash, fromIntegral index)
  pure $ convert res
  where
    convert :: (DB.TxId, DB.TxOutIdW, DbLovelace) -> (Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace)
    convert (txId, txOutId, lovelace) = (txIn, txId, txOutId, lovelace)

calculateTxFee :: Byron.Tx -> [(Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace)] -> Either SyncNodeError ValueFee
calculateTxFee tx resolvedInputs = do
  outval <- first (\e -> SNErrDefault $ "calculateTxFee: " <> textShow e) output
  when (null resolvedInputs) $
    Left $
      SNErrDefault "calculateTxFee: List of transaction inputs is zero."
  let inval = sum $ map (unDbLovelace . forth4) resolvedInputs
  if inval < outval
    then Left $ SNErrInvariant "calculateTxFee" $ EInvInOut inval outval
    else Right $ ValueFee (DbLovelace outval) (DbLovelace $ inval - outval)
  where
    output :: Either Byron.LovelaceError Word64
    output =
      Byron.unsafeGetLovelace
        <$> Byron.sumLovelace (map Byron.txOutValue $ Byron.txOutputs tx)
