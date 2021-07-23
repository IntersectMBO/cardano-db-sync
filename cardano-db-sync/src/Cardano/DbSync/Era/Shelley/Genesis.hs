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

module Cardano.DbSync.Era.Shelley.Genesis
  ( insertValidateGenesisDist
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT)

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import           Cardano.DbSync.Era.Util (liftLookupFail)
import           Cardano.Sync.Error
import           Cardano.Sync.Util

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import           Cardano.Ledger.Era (Crypto)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))

import qualified Shelley.Spec.Ledger.Genesis as Shelley
import           Shelley.Spec.Ledger.Scripts ()
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: SqlBackend -> Trace IO Text -> Text -> ShelleyGenesis StandardShelley
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
            liftIO $ logInfo tracer "Inserting Shelley Genesis distribution"
            emeta <- lift DB.queryMeta
            case emeta of
              Right _ -> pure () -- Metadata from Byron era already exists. TODO Validate metadata.
              Left _ -> do
                count <- lift DB.queryBlockCount
                when (count > 0) $
                  dbSyncNodeError $ "Shelley.insertValidateGenesisDist: Genesis data mismatch. count " <> textShow count
                void . lift $ DB.insertMeta $
                            DB.Meta
                              { DB.metaStartTime = configStartTime cfg
                              , DB.metaNetworkName = networkName
                              }
                -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
                -- need this block to attach the genesis distribution transactions to.
                -- It would be nice to not need this artificial block, but that would
                -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
                -- which would be a pain in the neck.
                slid <- lift . DB.insertSlotLeader $
                                DB.SlotLeader
                                  { DB.slotLeaderHash = genesisHashSlotLeader cfg
                                  , DB.slotLeaderPoolHashId = Nothing
                                  , DB.slotLeaderDescription = "Shelley Genesis slot leader"
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
                            , DB.blockTime = configStartTime cfg
                            , DB.blockTxCount = fromIntegral (length $ genesisTxos cfg)
                            -- Genesis block does not have a protocol version, so set this to '0'.
                            , DB.blockProtoMajor = 0
                            , DB.blockProtoMinor = 0
                            -- Shelley specific
                            , DB.blockVrfKey = Nothing
                            , DB.blockOpCert = Nothing
                            , DB.blockOpCertCounter = Nothing
                            }
                lift $ mapM_ (insertTxOuts bid) $ genesisUtxOs cfg
                liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                                <> renderByteArray (configGenesisHash cfg)

                supply <- lift DB.queryTotalSupply
                liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> ShelleyGenesis StandardShelley -> DB.BlockId
    -> ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    liftIO $ logInfo tracer "Validating Genesis distribution"
    meta <- liftLookupFail "Shelley.validateGenesisDistribution" DB.queryMeta

    when (DB.metaStartTime meta /= configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch chain start time. Config value "
            , textShow (configStartTime cfg)
            , " does not match DB value of ", textShow (DB.metaStartTime meta)
            ]

    when (DB.metaNetworkName meta /= networkName) $
      dbSyncNodeError $ Text.concat
            [ "Shelley.validateGenesisDistribution: Provided network name "
            , networkName
            , " does not match DB value "
            , DB.metaNetworkName meta
            ]

    txCount <- lift $ DB.queryBlockTxCount bid
    let expectedTxCount = fromIntegral $length (genesisTxos cfg)
    when (txCount /= expectedTxCount) $
      dbSyncNodeError $ Text.concat
              [ "Shelley.validateGenesisDistribution: Expected initial block to have "
              , textShow expectedTxCount
              , " but got "
              , textShow txCount
              ]
    totalSupply <- lift DB.queryGenesisSupply
    let expectedSupply = configGenesisSupply cfg
    when (expectedSupply /= totalSupply) $
      dbSyncNodeError  $ Text.concat
         [ "Shelley.validateGenesisDistribution: Expected total supply to be "
         , textShow expectedSupply
         , " but got "
         , textShow totalSupply
         ]
    supply <- lift DB.queryGenesisSupply
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- -----------------------------------------------------------------------------

insertTxOuts
    :: (MonadBaseControl IO m, MonadIO m)
    => DB.BlockId -> (Shelley.TxIn (Crypto StandardShelley), Shelley.TxOut StandardShelley)
    -> ReaderT SqlBackend m ()
insertTxOuts blkId (Shelley.TxIn txInId _, txOut) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Generic.unTxHash txInId
              , DB.txBlockId = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = Generic.coinToDbLovelace (txOutCoin txOut)
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
              , DB.txOutAddress = Generic.renderAddress (txOutAddress txOut)
              , DB.txOutAddressRaw = Ledger.serialiseAddr (txOutAddress txOut)
              , DB.txOutPaymentCred = Generic.maybePaymentCred (txOutAddress txOut)
              , DB.txOutStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
              , DB.txOutValue = Generic.coinToDbLovelace (txOutCoin txOut)
              }
  where
    txOutAddress :: Shelley.TxOut StandardShelley -> Ledger.Addr StandardCrypto
    txOutAddress (Shelley.TxOut out _) = out

    txOutCoin :: Shelley.TxOut StandardShelley -> Ledger.Coin
    txOutCoin (Shelley.TxOut _ coin) = coin

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis StandardShelley -> ByteString
configGenesisHash _ =  BS.take 28 ("GenesisHash " <> BS.replicate 28 '\0')

genesisHashSlotLeader :: ShelleyGenesis StandardShelley -> ByteString
genesisHashSlotLeader = configGenesisHash

configGenesisSupply :: ShelleyGenesis StandardShelley -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map (Ledger.unCoin . snd) . genesisTxoAssocList

genesisTxos :: ShelleyGenesis StandardShelley -> [Shelley.TxOut StandardShelley]
genesisTxos = map (uncurry Shelley.TxOut) . genesisTxoAssocList

genesisTxoAssocList :: ShelleyGenesis StandardShelley -> [(Ledger.Addr StandardCrypto, Ledger.Coin)]
genesisTxoAssocList =
    map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: Shelley.TxOut StandardShelley -> (Ledger.Addr StandardCrypto, Ledger.Coin)
    unTxOut (Shelley.TxOut addr amount) = (addr, amount)

genesisUtxOs :: ShelleyGenesis StandardShelley -> [(Shelley.TxIn (Crypto StandardShelley), Shelley.TxOut StandardShelley)]
genesisUtxOs =
    Map.toList . unUTxO . Shelley.genesisUTxO
  where
    -- Sigh!
    unUTxO :: Shelley.UTxO StandardShelley -> Map (Shelley.TxIn (Crypto StandardShelley)) (Shelley.TxOut StandardShelley)
    unUTxO (Shelley.UTxO m) = m

configStartTime :: ShelleyGenesis StandardShelley -> UTCTime
configStartTime = roundToMillseconds . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
    UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
