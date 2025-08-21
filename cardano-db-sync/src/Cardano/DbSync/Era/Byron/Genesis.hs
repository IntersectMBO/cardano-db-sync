{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Byron.Genesis (
  insertValidateByronGenesisDist,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Binary (serialize')
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto
import Cardano.Prelude
import Paths_cardano_db_sync (version)

import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (..))
import Cardano.DbSync.Cache (insertAddressUsingCache)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import Cardano.DbSync.Config.Types
import Cardano.DbSync.DbEvent (liftDbLookup, runDbSyncTransaction)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.DbSync.Error
import Cardano.DbSync.Util

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateByronGenesisDist ::
  SyncEnv ->
  NetworkName ->
  Byron.Config ->
  ExceptT SyncNodeError IO ()
insertValidateByronGenesisDist syncEnv (NetworkName networkName) cfg = do
  -- Genesis insertion is always syncing, use ReadCommitted for better performance
  ExceptT $ runDbSyncTransaction (getTrace syncEnv) (envDbEnv syncEnv) (Just DB.ReadCommitted) insertAction
  where
    tracer = getTrace syncEnv

    insertAction :: ExceptT SyncNodeError DB.DbM ()
    insertAction = do
      disInOut <- liftIO $ getDisableInOutState syncEnv
      let prunes = getPrunes syncEnv

      ebid <- lift $ DB.queryBlockIdEither (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution syncEnv prunes disInOut tracer networkName cfg bid
        Left err -> do
          liftIO $ logInfo tracer "Inserting Byron Genesis distribution"
          count <- lift DB.queryBlockCount
          when (not disInOut && count > 0) $
            throwError $
              SNErrDefault mkSyncNodeCallStack ("Genesis data mismatch. " <> show err)
          void . lift $
            DB.insertMeta $
              DB.Meta
                { DB.metaStartTime = Byron.configStartTime cfg
                , DB.metaNetworkName = networkName
                , DB.metaVersion = textShow version
                }

          -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
          -- need this block to attach the genesis distribution transactions to.
          -- It would be nice to not need this artificial block, but that would
          -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
          -- which would be a pain in the neck.
          slid <-
            lift $
              DB.insertSlotLeader $
                DB.SlotLeader
                  { DB.slotLeaderHash = BS.take 28 $ configGenesisHash cfg
                  , DB.slotLeaderPoolHashId = Nothing
                  , DB.slotLeaderDescription = "Genesis slot leader"
                  }
          bid <-
            lift $
              DB.insertBlock $
                DB.Block
                  { DB.blockHash = configGenesisHash cfg
                  , DB.blockEpochNo = Nothing
                  , DB.blockSlotNo = Nothing
                  , DB.blockEpochSlotNo = Nothing
                  , DB.blockBlockNo = Nothing
                  , DB.blockPreviousId = Nothing
                  , DB.blockSlotLeaderId = slid
                  , DB.blockSize = 0
                  , DB.blockTime = Byron.configStartTime cfg
                  , DB.blockTxCount = fromIntegral (length $ genesisTxos cfg)
                  , -- Genesis block does not have a protocol version, so set this to '0'.
                    DB.blockProtoMajor = 0
                  , DB.blockProtoMinor = 0
                  , -- Shelley specific
                    DB.blockVrfKey = Nothing
                  , DB.blockOpCert = Nothing
                  , DB.blockOpCertCounter = Nothing
                  }
          mapM_ (insertTxOutsByron syncEnv disInOut bid) $ genesisTxos cfg
          liftIO . logInfo tracer $
            "Initial genesis distribution populated. Hash "
              <> renderByteArray (configGenesisHash cfg)

          supply <- lift $ DB.queryTotalSupply $ getTxOutVariantType syncEnv
          liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution ::
  SyncEnv ->
  Bool ->
  Bool ->
  Trace IO Text ->
  Text ->
  Byron.Config ->
  DB.BlockId ->
  ExceptT SyncNodeError DB.DbM ()
validateGenesisDistribution syncEnv prunes disInOut tracer networkName cfg bid = do
  metaMaybe <- liftDbLookup mkSyncNodeCallStack DB.queryMeta

  -- Only validate if meta table has data
  case metaMaybe of
    Nothing -> do
      -- Meta table is empty, this is valid for initial startup
      liftIO $ logInfo tracer "Meta table is empty, skipping genesis validation"
      pure ()
    Just meta -> do
      when (DB.metaStartTime meta /= Byron.configStartTime cfg) $
        throwError $
          SNErrDefault
            mkSyncNodeCallStack
            ( Text.concat
                [ "Mismatch chain start time. Config value "
                , textShow (Byron.configStartTime cfg)
                , " does not match DB value of "
                , textShow (DB.metaStartTime meta)
                ]
            )

      when (DB.metaNetworkName meta /= networkName) $
        throwError $
          SNErrDefault
            mkSyncNodeCallStack
            ( Text.concat
                [ "Provided network name "
                , networkName
                , " does not match DB value "
                , DB.metaNetworkName meta
                ]
            )

      txCount <- lift $ DB.queryBlockTxCount bid
      let expectedTxCount = fromIntegral $ length (genesisTxos cfg)
      when (txCount /= expectedTxCount) $
        throwError $
          SNErrDefault
            mkSyncNodeCallStack
            ( Text.concat
                [ "Expected initial block to have "
                , textShow expectedTxCount
                , " but got "
                , textShow txCount
                ]
            )
      unless disInOut $ do
        totalSupply <- lift $ DB.queryGenesisSupply $ getTxOutVariantType syncEnv
        case DB.word64ToAda <$> configGenesisSupply cfg of
          Left err -> throwError $ SNErrDefault mkSyncNodeCallStack (textShow err)
          Right expectedSupply ->
            when (expectedSupply /= totalSupply && not prunes) $
              throwError $
                SNErrDefault
                  mkSyncNodeCallStack
                  ( Text.concat
                      [ "Expected total supply to be "
                      , DB.renderAda expectedSupply
                      , " but got "
                      , DB.renderAda totalSupply
                      ]
                  )
        liftIO $ do
          logInfo tracer "Initial genesis distribution present and correct"
          logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda totalSupply)

-------------------------------------------------------------------------------

insertTxOutsByron ::
  SyncEnv ->
  Bool ->
  DB.BlockId ->
  (Byron.Address, Byron.Lovelace) ->
  ExceptT SyncNodeError DB.DbM ()
insertTxOutsByron syncEnv disInOut blkId (address, value) = do
  case txHashOfAddress address of
    Left err -> throwError $ SNErrDefault mkSyncNodeCallStack $ Text.concat ["txHashOfAddress: ", show err]
    Right val -> do
      -- Each address/value pair of the initial coin distribution comes from an artifical transaction
      -- with a hash generated by hashing the address.
      txId <- do
        lift $
          DB.insertTx $
            DB.Tx
              { DB.txHash = Byron.unTxHash val
              , DB.txBlockId = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = DB.DbLovelace (Byron.unsafeGetLovelace value)
              , DB.txFee = DB.DbLovelace 0
              , DB.txDeposit = Just 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              , DB.txInvalidHereafter = Nothing
              , DB.txInvalidBefore = Nothing
              , DB.txValidContract = True
              , DB.txScriptSize = 0
              , DB.txTreasuryDonation = DB.DbLovelace 0
              }
      --
      unless disInOut $
        case getTxOutVariantType syncEnv of
          DB.TxOutVariantCore ->
            void . lift $
              DB.insertTxOut $
                DB.VCTxOutW
                  VC.TxOutCore
                    { VC.txOutCoreTxId = txId
                    , VC.txOutCoreIndex = 0
                    , VC.txOutCoreAddress = Text.decodeUtf8 $ Byron.addrToBase58 address
                    , VC.txOutCoreAddressHasScript = False
                    , VC.txOutCorePaymentCred = Nothing
                    , VC.txOutCoreStakeAddressId = Nothing
                    , VC.txOutCoreValue = DB.DbLovelace (Byron.unsafeGetLovelace value)
                    , VC.txOutCoreDataHash = Nothing
                    , VC.txOutCoreInlineDatumId = Nothing
                    , VC.txOutCoreReferenceScriptId = Nothing
                    , VC.txOutCoreConsumedByTxId = Nothing
                    }
          DB.TxOutVariantAddress -> do
            let addrRaw = serialize' address
                vAddress = mkVAddress addrRaw
            addrDetailId <- insertAddressUsingCache syncEnv UpdateCache addrRaw vAddress
            void . lift $
              DB.insertTxOut $
                DB.VATxOutW (mkTxOutAddress txId addrDetailId) Nothing
  where
    mkTxOutAddress :: DB.TxId -> DB.AddressId -> VA.TxOutAddress
    mkTxOutAddress txId addrDetailId =
      VA.TxOutAddress
        { VA.txOutAddressTxId = txId
        , VA.txOutAddressIndex = 0
        , VA.txOutAddressValue = DB.DbLovelace (Byron.unsafeGetLovelace value)
        , VA.txOutAddressDataHash = Nothing
        , VA.txOutAddressInlineDatumId = Nothing
        , VA.txOutAddressReferenceScriptId = Nothing
        , VA.txOutAddressAddressId = addrDetailId
        , VA.txOutAddressConsumedByTxId = Nothing
        , VA.txOutAddressStakeAddressId = Nothing
        }

    mkVAddress :: ByteString -> VA.Address
    mkVAddress addrRaw = do
      VA.Address
        { VA.addressAddress = Text.decodeUtf8 $ Byron.addrToBase58 address
        , VA.addressRaw = addrRaw
        , VA.addressHasScript = False
        , VA.addressPaymentCred = Nothing -- Byron does not have a payment credential.
        , VA.addressStakeAddressId = Nothing -- Byron does not have a stake address.
        }

---------------------------------------------------------------------------------

configGenesisHash :: Byron.Config -> ByteString
configGenesisHash =
  Byron.unAbstractHash . Byron.unGenesisHash . Byron.configGenesisHash

configGenesisSupply :: Byron.Config -> Either Byron.LovelaceError Word64
configGenesisSupply =
  fmap Byron.unsafeGetLovelace . Byron.sumLovelace . map snd . genesisTxos

genesisTxos :: Byron.Config -> [(Byron.Address, Byron.Lovelace)]
genesisTxos config =
  avvmBalances <> nonAvvmBalances
  where
    avvmBalances :: [(Byron.Address, Byron.Lovelace)]
    avvmBalances =
      first (Byron.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Byron.unGenesisAvvmBalances $ Byron.configAvvmDistr config)

    networkMagic :: Byron.NetworkMagic
    networkMagic = Byron.makeNetworkMagic (Byron.configProtocolMagic config)

    nonAvvmBalances :: [(Byron.Address, Byron.Lovelace)]
    nonAvvmBalances =
      Map.toList $ Byron.unGenesisNonAvvmBalances (Byron.configNonAvvmBalances config)

txHashOfAddress :: Byron.Address -> Either SyncNodeError (Crypto.Hash Byron.Tx)
txHashOfAddress ba = do
  case hashFromBS of
    Just res -> Right res
    Nothing -> Left $ SNErrInsertGenesis "Cardano.DbSync.Era.Byron.Genesis.txHashOfAddress"
  where
    hashFromBS =
      Crypto.abstractHashFromBytes
        . Crypto.abstractHashToBytes
        $ Crypto.serializeCborHash ba
