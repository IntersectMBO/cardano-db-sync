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

import qualified Cardano.Binary as Binary
import           Cardano.BM.Trace (Trace, logInfo)
import           Cardano.Slotting.Slot (EpochSize (..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time

import           Database.Persist.Sql (SqlBackend)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Shelley.Util as Shelley
import           Cardano.DbSync.Error
import           Cardano.DbSync.Types
import           Cardano.DbSync.Util

import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                  (mkSlotLength, slotLengthToMillisec)

import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: Trace IO Text -> Text -> ShelleyGenesis TPraosStandardCrypto
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
            liftIO $ logInfo tracer "Inserting Genesis distribution"
            count <- lift DB.queryBlockCount
            when (count > 0) $
              dbSyncNodeError "Shelley.insertValidateGenesisDist: Genesis data mismatch."
            void . lift . DB.insertMeta
                $ DB.Meta
                    (protocolConstant cfg)
                    (configSlotDuration cfg)
                    (configStartTime cfg)
                    (configSlotsPerEpoch cfg)
                    (Just networkName)
            -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
            -- need this block to attach the genesis distribution transactions to.
            -- It would be nice to not need this artificial block, but that would
            -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
            -- which would be a pain in the neck.
            slid <- lift . DB.insertSlotLeader $ DB.SlotLeader (genesisHashSlotLeader cfg) "Genesis slot leader"
            bid <- lift . DB.insertBlock $
                      DB.Block
                        { DB.blockHash = configGenesisHash cfg
                        , DB.blockEpochNo = Nothing
                        , DB.blockSlotNo = Nothing
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

            supply <- lift $ DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> ShelleyGenesis TPraosStandardCrypto -> DB.BlockId
    -> ReaderT SqlBackend m (Either DbSyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    liftIO $ logInfo tracer "Validating Genesis distribution"
    meta <- liftLookupFail "Shelley.validateGenesisDistribution" $ DB.queryMeta

    when (DB.metaProtocolConst meta /= protocolConstant cfg) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch protocol constant. Config value "
            , textShow (protocolConstant cfg)
            , " does not match DB value of ", textShow (DB.metaProtocolConst meta)
            ]

    when (DB.metaSlotDuration meta /= configSlotDuration cfg) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch slot duration time. Config value "
            , textShow (configSlotDuration cfg)
            , " does not match DB value of ", textShow (DB.metaSlotDuration meta)
            ]

    when (DB.metaStartTime meta /= configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch chain start time. Config value "
            , textShow (configStartTime cfg)
            , " does not match DB value of ", textShow (DB.metaStartTime meta)
            ]

    when (DB.metaSlotsPerEpoch meta /= configSlotsPerEpoch cfg) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch in slots per epoch. Config value "
            , textShow (configSlotsPerEpoch cfg)
            , " does not match DB value of ", textShow (DB.metaSlotsPerEpoch meta)
            ]

    case DB.metaNetworkName meta of
      Nothing ->
        dbSyncNodeError $ "Shelley.validateGenesisDistribution: Missing network name"
      Just name ->
        when (name /= networkName) $
          dbSyncNodeError $ Text.concat
              [ "Shelley.validateGenesisDistribution: Provided network name "
              , networkName
              , " does not match DB value "
              , name
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
              , DB.txOutSum = unCoin (txOutCoin txOut)
              , DB.txFee = 0
              , DB.txDeposit = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Text.decodeUtf8 $ Base16.encode (Binary.serialize' $ txOutAddress txOut)
              , DB.txOutValue = unCoin (txOutCoin txOut)
              }
  where
    txOutAddress :: ShelleyTxOut -> ShelleyAddress
    txOutAddress (Shelley.TxOut out _) = out

    txOutCoin :: ShelleyTxOut -> Shelley.Coin
    txOutCoin (Shelley.TxOut _ coin) = coin

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis TPraosStandardCrypto -> ByteString
configGenesisHash _ = Shelley.fakeGenesisHash

genesisHashSlotLeader :: ShelleyGenesis TPraosStandardCrypto -> ByteString
genesisHashSlotLeader = configGenesisHash

configGenesisSupply :: ShelleyGenesis TPraosStandardCrypto -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map (unCoin . snd) . genesisTxoAssocList

genesisTxos :: ShelleyGenesis TPraosStandardCrypto -> [ShelleyTxOut]
genesisTxos = map (uncurry Shelley.TxOut) . genesisTxoAssocList

genesisTxoAssocList :: ShelleyGenesis TPraosStandardCrypto -> [(ShelleyAddress, Shelley.Coin)]
genesisTxoAssocList =
    map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: ShelleyTxOut -> (ShelleyAddress, Shelley.Coin)
    unTxOut (Shelley.TxOut addr amount) = (addr, amount)

genesisUtxOs :: ShelleyGenesis TPraosStandardCrypto -> [(ShelleyTxIn, ShelleyTxOut)]
genesisUtxOs =
    Map.toList . unUTxO . Shelley.genesisUtxO
  where
    -- Sigh!
    unUTxO :: Shelley.UTxO TPraosStandardCrypto -> Map ShelleyTxIn ShelleyTxOut
    unUTxO (Shelley.UTxO m) = m

protocolConstant :: ShelleyGenesis TPraosStandardCrypto -> Word64
protocolConstant = Shelley.sgSecurityParam

unCoin :: Shelley.Coin -> Word64
unCoin (Shelley.Coin c) = fromIntegral c

-- | The genesis data is a NominalDiffTime (in picoseconds) and we need
-- it as milliseconds.
configSlotDuration :: ShelleyGenesis TPraosStandardCrypto -> Word64
configSlotDuration =
  fromIntegral . slotLengthToMillisec . mkSlotLength . sgSlotLength

configSlotsPerEpoch :: ShelleyGenesis TPraosStandardCrypto -> Word64
configSlotsPerEpoch sg = unEpochSize (Shelley.sgEpochLength sg)

configStartTime :: ShelleyGenesis TPraosStandardCrypto -> UTCTime
configStartTime = roundToMillseconds . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
    UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
