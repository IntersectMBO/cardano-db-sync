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
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Slotting.Slot (EpochSize (..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
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

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Shelley
import qualified Ouroboros.Consensus.Config.SecurityParam as Shelley
import qualified Ouroboros.Consensus.Shelley.Genesis as Shelley
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Credential as Shelley
import           Shelley.Spec.Ledger.Crypto (HASH)
import qualified Shelley.Spec.Ledger.Keys as Shelley
import qualified Shelley.Spec.Ledger.Scripts as Shelley
import qualified Shelley.Spec.Ledger.TxData as Shelley

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
    -- liftIO $ panic ("We want to stop here: " <> textShow (length $ genesisUtxOs cfg))
  where
    insertAction :: MonadIO m => ReaderT SqlBackend m (Either DbSyncNodeError ())
    insertAction = do
      ebid <- DB.queryBlockId (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution tracer networkName cfg bid
        Left _ ->
          runExceptT $ do
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
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisUtxOs cfg
            liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                            <> renderByteArray (configGenesisHash cfg)

            supply <- lift $ DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: MonadIO m
    => Trace IO Text -> Text -> ShelleyGenesis TPraosStandardCrypto -> DB.BlockId
    -> ReaderT SqlBackend m (Either DbSyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
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

insertTxOuts :: MonadIO m => DB.BlockId -> (ShelleyTxIn, ShelleyTxOut) -> ReaderT SqlBackend m ()
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
genesisHashSlotLeader = BS.take 32 . configGenesisHash

configGenesisSupply :: ShelleyGenesis TPraosStandardCrypto -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map (unCoin . snd) . genesisTxoAssocList

genesisTxos :: ShelleyGenesis TPraosStandardCrypto -> [ShelleyTxOut]
genesisTxos = map (uncurry Shelley.TxOut) . genesisTxoAssocList

genesisTxoAssocList :: ShelleyGenesis TPraosStandardCrypto -> [(ShelleyAddress, Shelley.Coin)]
genesisTxoAssocList = Map.toList . Shelley.sgInitialFunds

protocolConstant :: ShelleyGenesis TPraosStandardCrypto -> Word64
protocolConstant = Shelley.maxRollbacks . Shelley.sgSecurityParam

unCoin :: Shelley.Coin -> Word64
unCoin (Shelley.Coin c) = fromIntegral c

-- | The genesis data is a NominalDiffTime (in picoseconds) and we need
-- it as milliseconds.
configSlotDuration :: ShelleyGenesis TPraosStandardCrypto -> Word64
configSlotDuration sg =
  floor $ 1000 * Shelley.getSlotLength (Shelley.sgSlotLength sg)

configSlotsPerEpoch :: ShelleyGenesis TPraosStandardCrypto -> Word64
configSlotsPerEpoch sg = unEpochSize (Shelley.sgEpochLength sg)

configStartTime :: ShelleyGenesis TPraosStandardCrypto -> UTCTime
configStartTime = roundToMillseconds . Shelley.getSystemStart . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
    UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs



-- -----------------------------------------------------------------------------
-- All this should be provided by a lower level library

genesisUtxOs :: ShelleyGenesis TPraosStandardCrypto -> [(ShelleyTxIn, ShelleyTxOut)]
genesisUtxOs sg =
  [ (txIn, txOut)
  | (addr, amount) <- Map.toList (sgInitialFunds sg)
  , let txIn  = initialFundsPseudoTxIn addr
        txOut = Shelley.TxOut addr amount
  ]

initialFundsPseudoTxIn :: ShelleyAddress -> ShelleyTxIn
initialFundsPseudoTxIn addr =
    case addr of
      Shelley.Addr _networkId (Shelley.KeyHashObj    (Shelley.KeyHash    h)) _sref -> pseudoTxIn h
      Shelley.Addr _networkId (Shelley.ScriptHashObj (Shelley.ScriptHash h)) _sref -> pseudoTxIn h
      Shelley.AddrBootstrap byronAddr ->
        panic $ "Unsupported Byron address in the genesis UTxO: " <> show byronAddr
  where
    pseudoTxIn :: Crypto.Hash (HASH TPraosStandardCrypto) a -> ShelleyTxIn
    pseudoTxIn h = Shelley.TxIn (pseudoTxId h) 0

    pseudoTxId :: Crypto.Hash (HASH TPraosStandardCrypto) a -> ShelleyTxId
    pseudoTxId = Shelley.TxId . castHash

    --TODO: move this to the hash API module
    castHash :: Crypto.Hash (HASH TPraosStandardCrypto) a -> Crypto.Hash (HASH TPraosStandardCrypto) b
    castHash (Crypto.UnsafeHash h) = Crypto.UnsafeHash h

