{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Byron.Insert (
  insertByronBlock,
  resolveTxInputsByron,
)
where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import Cardano.Binary (serialize')
import qualified Cardano.Chain.Block as Byron hiding (blockHash)
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Chain.Update as Byron hiding (protocolVersion)
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

import Cardano.Db (DbLovelace (..))
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (insertAddressUsingCache, insertBlockAndCache, queryPrevBlockWithCache)
import Cardano.DbSync.Cache.Epoch (writeEpochBlockDiffToCache)
import Cardano.DbSync.Cache.Types (CacheAction (..), EpochBlockDiff (..))
import Cardano.DbSync.DbEvent (liftDbLookup)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.DbSync.Error
import Cardano.DbSync.Types
import Cardano.DbSync.Util

-- Trivial local data type for use in place of a tuple.
data ValueFee = ValueFee
  { vfValue :: !DbLovelace
  , vfFee :: !DbLovelace
  }

insertByronBlock ::
  SyncEnv ->
  Bool ->
  ByronBlock ->
  SlotDetails ->
  ExceptT SyncNodeError DB.DbM ()
insertByronBlock syncEnv firstBlockOfEpoch blk details = do
  case byronBlockRaw blk of
    Byron.ABOBBlock ablk -> insertABlock syncEnv firstBlockOfEpoch ablk details
    Byron.ABOBBoundary abblk -> insertABOBBoundary syncEnv abblk details

insertABOBBoundary ::
  SyncEnv ->
  Byron.ABoundaryBlock ByteString ->
  SlotDetails ->
  ExceptT SyncNodeError DB.DbM ()
insertABOBBoundary syncEnv blk details = do
  let tracer = getTrace syncEnv
  -- Will not get called in the OBFT part of the Byron era.
  pbid <- queryPrevBlockWithCache syncEnv (Byron.ebbPrevHash blk) "insertABOBBoundary"
  let epochNo = unEpochNo $ sdEpochNo details
  slid <-
    lift $
      DB.insertSlotLeader $
        DB.SlotLeader
          { DB.slotLeaderHash = BS.replicate 28 '\0'
          , DB.slotLeaderPoolHashId = Nothing
          , DB.slotLeaderDescription = "Epoch boundary slot leader"
          }
  blkId <-
    insertBlockAndCache syncEnv $
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
  when (soptEpochAndCacheEnabled $ envOptions syncEnv) $
    writeEpochBlockDiffToCache
      (envCache syncEnv)
      EpochBlockDiff
        { ebdBlockId = blkId
        , ebdFees = 0
        , ebdOutSum = 0
        , ebdTxCount = 0
        , ebdEpochNo = epochNo
        , ebdTime = sdSlotTime details
        }

  liftIO
    . logInfo tracer
    $ Text.concat
      [ "insertABOBBoundary: epoch "
      , textShow (Byron.boundaryEpoch $ Byron.boundaryHeader blk)
      , ", hash "
      , Byron.renderAbstractHash (Byron.boundaryHashAnnotated blk)
      ]

insertABlock ::
  SyncEnv ->
  Bool ->
  Byron.ABlock ByteString ->
  SlotDetails ->
  ExceptT SyncNodeError DB.DbM ()
insertABlock syncEnv firstBlockOfEpoch blk details = do
  pbid <- queryPrevBlockWithCache syncEnv (Byron.blockPreviousHash blk) "insertABlock"
  slid <- lift $ DB.insertSlotLeader $ Byron.mkSlotLeader blk
  let txs = Byron.blockPayload blk
  blkId <-
    insertBlockAndCache syncEnv $
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
  when (soptEpochAndCacheEnabled $ envOptions syncEnv) $
    writeEpochBlockDiffToCache
      (envCache syncEnv)
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

    logger :: Bool -> Trace IO a -> a -> IO ()
    logger followingClosely
      | firstBlockOfEpoch = logInfo
      | followingClosely = logInfo
      | Byron.blockNumber blk `mod` 1000 == 0 = logInfo
      | otherwise = logDebug

insertByronTx ::
  SyncEnv ->
  DB.BlockId ->
  Byron.TxAux ->
  Word64 ->
  ExceptT SyncNodeError DB.DbM Word64
insertByronTx syncEnv blkId tx blockIndex = do
  disInOut <- liftIO $ getDisableInOutState syncEnv
  if disInOut
    then do
      txId <-
        lift $
          DB.insertTx $
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
        void $
          lift $
            DB.insertTxCbor $
              DB.TxCbor
                { DB.txCborTxId = txId
                , DB.txCborBytes = serialize' $ Byron.taTx tx
                }

      pure 0
    else insertByronTx' syncEnv blkId tx blockIndex
  where
    iopts = getInsertOptions syncEnv

insertByronTx' ::
  SyncEnv ->
  DB.BlockId ->
  Byron.TxAux ->
  Word64 ->
  ExceptT SyncNodeError DB.DbM Word64
insertByronTx' syncEnv blkId tx blockIndex = do
  -- Resolve all blockchain transaction inputs - any failure will throw via MonadError
  resolvedInputs <- mapM (resolveTxInputsByron txOutVariantType) (toList $ Byron.txInputs (Byron.taTx tx))

  -- Calculate blockchain transaction fee
  valFee <- case calculateTxFee (Byron.taTx tx) resolvedInputs of
    Left err -> throwError $ SNErrDefault mkSyncNodeCallStack (show (annotateTx err))
    Right vf -> pure vf

  -- Insert the blockchain transaction record
  txId <-
    lift $
      DB.insertTx $
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

  -- Insert CBOR if enabled
  when (ioTxCBOR iopts) $ do
    void $
      lift $
        DB.insertTxCbor $
          DB.TxCbor
            { DB.txCborTxId = txId
            , DB.txCborBytes = serialize' $ Byron.taTx tx
            }

  -- Insert outputs for this blockchain transaction before inputs in case the inputs
  -- reference the output (not sure this can even happen).
  disInOut <- liftIO $ getDisableInOutState syncEnv
  zipWithM_ (insertTxOutByron syncEnv (getHasConsumedOrPruneTxOut syncEnv) disInOut txId) [0 ..] (toList . Byron.txOutputs $ Byron.taTx tx)

  -- Insert blockchain transaction inputs (only if we have resolved inputs and TxIn is not disabled)
  unless (getSkipTxIn syncEnv) $
    mapM_ (insertTxIn tracer txId) resolvedInputs

  -- Update consumed TxOut records if enabled
  whenConsumeOrPruneTxOut syncEnv $
    lift $
      DB.updateListTxOutConsumedByTxIdBP [prepUpdate txId <$> resolvedInputs]

  -- Return fee amount for caching/epoch calculations
  pure $ unDbLovelace $ vfFee valFee
  where
    txOutVariantType = getTxOutVariantType syncEnv
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
  SyncEnv ->
  Bool ->
  Bool ->
  DB.TxId ->
  Word32 ->
  Byron.TxOut ->
  ExceptT SyncNodeError DB.DbM ()
insertTxOutByron syncEnv _hasConsumed bootStrap txId index txout =
  unless bootStrap $
    case ioTxOutVariantType . soptInsertOptions $ envOptions syncEnv of
      DB.TxOutVariantCore -> do
        void
          . lift
          $ DB.insertTxOut
          $ DB.VCTxOutW
          $ VC.TxOutCore
            { VC.txOutCoreAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
            , VC.txOutCoreAddressHasScript = False
            , VC.txOutCoreDataHash = Nothing
            , VC.txOutCoreConsumedByTxId = Nothing
            , VC.txOutCoreIndex = fromIntegral index
            , VC.txOutCoreInlineDatumId = Nothing
            , VC.txOutCorePaymentCred = Nothing -- Byron does not have a payment credential.
            , VC.txOutCoreReferenceScriptId = Nothing
            , VC.txOutCoreStakeAddressId = Nothing -- Byron does not have a stake address.
            , VC.txOutCoreTxId = txId
            , VC.txOutCoreValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
            }
      DB.TxOutVariantAddress -> do
        addrDetailId <- insertAddressUsingCache syncEnv UpdateCache addrRaw vAddress
        void . lift $ DB.insertTxOut $ DB.VATxOutW (vTxOut addrDetailId) Nothing
  where
    addrRaw :: ByteString
    addrRaw = serialize' (Byron.txOutAddress txout)

    vTxOut :: DB.AddressId -> VA.TxOutAddress
    vTxOut addrDetailId =
      VA.TxOutAddress
        { VA.txOutAddressAddressId = addrDetailId
        , VA.txOutAddressConsumedByTxId = Nothing
        , VA.txOutAddressDataHash = Nothing
        , VA.txOutAddressIndex = fromIntegral index
        , VA.txOutAddressInlineDatumId = Nothing
        , VA.txOutAddressReferenceScriptId = Nothing
        , VA.txOutAddressTxId = txId
        , VA.txOutAddressValue = DbLovelace (Byron.unsafeGetLovelace $ Byron.txOutValue txout)
        , VA.txOutAddressStakeAddressId = Nothing
        }

    vAddress :: VA.Address
    vAddress =
      VA.Address
        { VA.addressAddress = Text.decodeUtf8 $ Byron.addrToBase58 (Byron.txOutAddress txout)
        , VA.addressRaw = addrRaw
        , VA.addressHasScript = False
        , VA.addressPaymentCred = Nothing -- Byron does not have a payment credential.
        , VA.addressStakeAddressId = Nothing -- Byron does not have a stake address.
        }

insertTxIn ::
  Trace IO Text ->
  DB.TxId ->
  (Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace) ->
  ExceptT SyncNodeError DB.DbM DB.TxInId
insertTxIn _tracer txInTxId (Byron.TxInUtxo _txHash inIndex, txOutTxId, _, _) =
  lift $
    DB.insertTxIn $
      DB.TxIn
        { DB.txInTxInId = txInTxId
        , DB.txInTxOutId = txOutTxId
        , DB.txInTxOutIndex = fromIntegral inIndex
        , DB.txInRedeemerId = Nothing
        }

-------------------------------------------------------------------------------

resolveTxInputsByron ::
  DB.TxOutVariantType ->
  Byron.TxIn ->
  ExceptT SyncNodeError DB.DbM (Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace)
resolveTxInputsByron txOutVariantType txIn@(Byron.TxInUtxo txHash index) = do
  result <- liftDbLookup mkSyncNodeCallStack $ DB.queryTxOutIdValueEither txOutVariantType (Byron.unTxHash txHash, fromIntegral index)
  pure $ convert result
  where
    convert (txId, txOutId, lovelace) = (txIn, txId, txOutId, lovelace)

calculateTxFee :: Byron.Tx -> [(Byron.TxIn, DB.TxId, DB.TxOutIdW, DbLovelace)] -> Either SyncNodeError ValueFee
calculateTxFee tx resolvedInputs = do
  outval <- first (SNErrDefault mkSyncNodeCallStack . textShow) output
  when (null resolvedInputs) $
    Left $
      SNErrDefault mkSyncNodeCallStack "List of transaction inputs is zero."
  let inval = sum $ map (unDbLovelace . forth4) resolvedInputs
  if inval < outval
    then Left $ SNErrInvariant "calculateTxFee" $ EInvInOut inval outval
    else Right $ ValueFee (DbLovelace outval) (DbLovelace $ inval - outval)
  where
    output :: Either Byron.LovelaceError Word64
    output =
      Byron.unsafeGetLovelace
        <$> Byron.sumLovelace (map Byron.txOutValue $ Byron.txOutputs tx)
