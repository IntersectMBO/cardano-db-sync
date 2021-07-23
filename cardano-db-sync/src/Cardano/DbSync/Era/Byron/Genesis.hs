{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Byron.Genesis
  ( insertValidateGenesisDist
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (Hash, fromCompactRedeemVerificationKey,
                   serializeCborHash)

import qualified Cardano.Db as DB
import           Cardano.DbSync.Era.Util (liftLookupFail)
import qualified Cardano.Sync.Era.Byron.Util as Byron
import           Cardano.Sync.Error
import           Cardano.Sync.Util

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Sql (SqlBackend)

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> Text -> Byron.Config
    -> ExceptT SyncNodeError IO ()
insertValidateGenesisDist backend tracer networkName cfg = do
    -- Setting this to True will log all 'Persistent' operations which is great
    -- for debugging, but otherwise *way* too chatty.
    if False
      then newExceptT $ DB.runDbIohkLogging backend tracer insertAction
      else newExceptT $ DB.runDbIohkNoLogging backend insertAction
  where
    insertAction :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Either SyncNodeError ())
    insertAction = do
      ebid <- DB.queryBlockId (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution tracer networkName cfg bid
        Left _ ->
          runExceptT $ do
            liftIO $ logInfo tracer "Inserting Byron Genesis distribution"
            count <- lift DB.queryBlockCount
            when (count > 0) $
              dbSyncNodeError "insertValidateGenesisDist: Genesis data mismatch."
            void . lift $ DB.insertMeta $
                            DB.Meta
                              { DB.metaStartTime = Byron.configStartTime cfg
                              , DB.metaNetworkName = networkName
                              }

            -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
            -- need this block to attach the genesis distribution transactions to.
            -- It would be nice to not need this artificial block, but that would
            -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
            -- which would be a pain in the neck.
            slid <- lift . DB.insertSlotLeader $
                            DB.SlotLeader
                              { DB.slotLeaderHash = BS.take 28 $ configGenesisHash cfg
                              , DB.slotLeaderPoolHashId = Nothing
                              , DB.slotLeaderDescription = "Genesis slot leader"
                              }
            bid <- lift . DB.insertBlock $
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
                        -- Genesis block does not have a protocol version, so set this to '0'.
                        , DB.blockProtoMajor = 0
                        , DB.blockProtoMinor = 0
                        -- Shelley specific
                        , DB.blockVrfKey = Nothing
                        , DB.blockOpCert = Nothing
                        , DB.blockOpCertCounter = Nothing
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisTxos cfg
            liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                            <> renderByteArray (configGenesisHash cfg)

            supply <- lift DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> Byron.Config -> DB.BlockId
    -> ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    meta <- liftLookupFail "validateGenesisDistribution" DB.queryMeta

    when (DB.metaStartTime meta /= Byron.configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch chain start time. Config value "
            , textShow (Byron.configStartTime cfg)
            , " does not match DB value of ", textShow (DB.metaStartTime meta)
            ]

    when (DB.metaNetworkName meta /= networkName) $
          dbSyncNodeError $ Text.concat
              [ "validateGenesisDistribution: Provided network name "
              , networkName
              , " does not match DB value "
              , DB.metaNetworkName meta
              ]

    txCount <- lift $ DB.queryBlockTxCount bid
    let expectedTxCount = fromIntegral $length (genesisTxos cfg)
    when (txCount /= expectedTxCount) $
      dbSyncNodeError $ Text.concat
              [ "validateGenesisDistribution: Expected initial block to have "
              , textShow expectedTxCount
              , " but got "
              , textShow txCount
              ]
    totalSupply <- lift DB.queryGenesisSupply
    case DB.word64ToAda <$> configGenesisSupply cfg of
      Left err -> dbSyncNodeError $ "validateGenesisDistribution: " <> textShow err
      Right expectedSupply ->
        when (expectedSupply /= totalSupply) $
          dbSyncNodeError  $ Text.concat
                [ "validateGenesisDistribution: Expected total supply to be "
                , DB.renderAda expectedSupply
                , " but got "
                , DB.renderAda totalSupply
                ]
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda totalSupply)

-- -----------------------------------------------------------------------------

insertTxOuts :: (MonadBaseControl IO m, MonadIO m) => DB.BlockId -> (Byron.Address, Byron.Lovelace) -> ReaderT SqlBackend m ()
insertTxOuts blkId (address, value) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Byron.unTxHash $ txHashOfAddress address
              , DB.txBlockId = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = DB.DbLovelace (Byron.unsafeGetLovelace value)
              , DB.txFee = DB.DbLovelace 0
              , DB.txDeposit = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              , DB.txInvalidHereafter = Nothing
              , DB.txInvalidBefore = Nothing
              , DB.txValidContract = True
              , DB.txExUnitNumber = 0
              , DB.txExUnitFee = DB.DbLovelace 0
              , DB.txScriptSize = 0
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 address
              , DB.txOutAddressRaw = Binary.serialize' address
              , DB.txOutPaymentCred = Nothing
              , DB.txOutStakeAddressId = Nothing
              , DB.txOutValue = DB.DbLovelace (Byron.unsafeGetLovelace value)
              }

-- -----------------------------------------------------------------------------

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

txHashOfAddress :: Byron.Address -> Crypto.Hash Byron.Tx
txHashOfAddress = coerce . Crypto.serializeCborHash
