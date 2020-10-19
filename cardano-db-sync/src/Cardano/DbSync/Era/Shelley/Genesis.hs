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

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (StandardShelley)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: Trace IO Text -> Text -> ShelleyGenesis StandardShelley
    -> ExceptT DbSyncNodeError IO ()
insertValidateGenesisDist tracer networkName cfg = do
    -- Setting this to True will log all 'Persistent' operations which is great
    -- for debugging, but otherwise *way* too chatty.
    if False
      then newExceptT $ DB.runDbIohkLogging tracer insertAction
      else newExceptT $ DB.runDbNoLogging insertAction
  where
    insertAction :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend m (Either DbSyncNodeError ())
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
                            , DB.blockPrevious = Nothing
                            , DB.blockMerkelRoot = Nothing
                            , DB.blockSlotLeader = slid
                            , DB.blockSize = 0
                            , DB.blockTime = configStartTime cfg
                            , DB.blockTxCount = 0

                            -- Shelley specific
                            , DB.blockVrfKey = Nothing
                            , DB.blockOpCert = Nothing
                            , DB.blockProtoVersion = Nothing
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
    -> ReaderT SqlBackend m (Either DbSyncNodeError ())
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

insertTxOuts :: (MonadBaseControl IO m, MonadIO m) => DB.BlockId -> (ShelleyTxIn, ShelleyTxOut) -> ReaderT SqlBackend m ()
insertTxOuts blkId (Shelley.TxIn txInId _, txOut) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Shelley.unTxHash txInId
              , DB.txBlock = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = Shelley.coinToDbLovelace (txOutCoin txOut)
              , DB.txFee = DB.DbLovelace 0
              , DB.txDeposit = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Shelley.renderAddress (txOutAddress txOut)
              , DB.txOutAddressRaw = Shelley.serialiseAddr (txOutAddress txOut)
              , DB.txOutPaymentCred = Shelley.maybePaymentCred (txOutAddress txOut)
              , DB.txOutStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
              , DB.txOutValue = Shelley.coinToDbLovelace (txOutCoin txOut)
              }
  where
    txOutAddress :: ShelleyTxOut -> ShelleyAddress
    txOutAddress (Shelley.TxOut out _) = out

    txOutCoin :: ShelleyTxOut -> Shelley.Coin
    txOutCoin (Shelley.TxOut _ coin) = coin

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis StandardShelley -> ByteString
configGenesisHash _ = Shelley.fakeGenesisHash

genesisHashSlotLeader :: ShelleyGenesis StandardShelley -> ByteString
genesisHashSlotLeader = configGenesisHash

configGenesisSupply :: ShelleyGenesis StandardShelley -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map (Shelley.unCoin . snd) . genesisTxoAssocList

genesisTxos :: ShelleyGenesis StandardShelley -> [ShelleyTxOut]
genesisTxos = map (uncurry Shelley.TxOut) . genesisTxoAssocList

genesisTxoAssocList :: ShelleyGenesis StandardShelley -> [(ShelleyAddress, Shelley.Coin)]
genesisTxoAssocList =
    map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: ShelleyTxOut -> (ShelleyAddress, Shelley.Coin)
    unTxOut (Shelley.TxOut addr amount) = (addr, amount)

genesisUtxOs :: ShelleyGenesis StandardShelley -> [(ShelleyTxIn, ShelleyTxOut)]
genesisUtxOs =
    Map.toList . unUTxO . Shelley.genesisUtxO
  where
    -- Sigh!
    unUTxO :: Shelley.UTxO StandardShelley -> Map ShelleyTxIn ShelleyTxOut
    unUTxO (Shelley.UTxO m) = m

configStartTime :: ShelleyGenesis StandardShelley -> UTCTime
configStartTime = roundToMillseconds . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
    UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
