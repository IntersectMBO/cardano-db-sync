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

import           Database.Persist.Class (updateWhere)
import           Database.Persist.Sql (SqlBackend, (=.), (==.))

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util
-- import qualified Cardano.DbSync.Era.Shelley.Util as Shelley

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as Shelley
import qualified Ouroboros.Consensus.Config.SecurityParam as Shelley
import qualified Ouroboros.Consensus.Shelley.Genesis as Shelley
import           Ouroboros.Consensus.Shelley.Node (ShelleyGenesis (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosStandardCrypto)

import qualified Shelley.Spec.Ledger.Address as Shelley
import qualified Shelley.Spec.Ledger.Coin as Shelley


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
            void . lift $ DB.insertMeta $ DB.Meta
                                    (protocolConstant cfg)
                                    (configSlotDuration cfg)
                                    (Shelley.getSystemStart $ Shelley.sgSystemStart cfg)
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
                        , DB.blockTime = Shelley.getSystemStart (Shelley.sgSystemStart cfg)
                        , DB.blockTxCount = 0
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisTxos cfg
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
            , " does not match DB value of ", textShow (configSlotDuration cfg)
            ]

    when (DB.metaStartTime meta /= Shelley.getSystemStart (Shelley.sgSystemStart cfg)) $
      dbSyncNodeError $ Text.concat
            [ "Shelley: Mismatch chain start time. Config value "
            , textShow (Shelley.sgSystemStart cfg)
            , " does not match DB value of ", textShow (Shelley.sgSystemStart cfg)
            ]

    case DB.metaNetworkName meta of
      Nothing -> lift $ updateWhere
                  [DB.MetaStartTime ==. Shelley.getSystemStart (Shelley.sgSystemStart cfg)]
                  [DB.MetaNetworkName =. Just networkName]
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
         [ "validateGenesisDistribution: Expected total supply to be "
         , textShow expectedSupply
         , " but got "
         , textShow totalSupply
         ]
    supply <- lift DB.queryGenesisSupply
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- -----------------------------------------------------------------------------

insertTxOuts :: MonadIO m => DB.BlockId -> (Shelley.Addr TPraosStandardCrypto, Shelley.Coin) -> ReaderT SqlBackend m ()
insertTxOuts blkId (address, value) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = txHashOfAddress address
              , DB.txBlock = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = unCoin value
              , DB.txFee = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Text.decodeUtf8 $ Base16.encode (Binary.serialize' address)
              , DB.txOutValue = unCoin value
              }

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis TPraosStandardCrypto -> ByteString
configGenesisHash = const "Hash of genesis configuration"

genesisHashSlotLeader :: ShelleyGenesis TPraosStandardCrypto -> ByteString
genesisHashSlotLeader = BS.take 28 . configGenesisHash

configGenesisSupply :: ShelleyGenesis TPraosStandardCrypto -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map (unCoin . snd) . genesisTxos

genesisTxos :: ShelleyGenesis TPraosStandardCrypto -> [(Shelley.Addr TPraosStandardCrypto, Shelley.Coin)]
genesisTxos = Map.toList . Shelley.sgInitialFunds

protocolConstant :: ShelleyGenesis TPraosStandardCrypto -> Word64
protocolConstant = Shelley.maxRollbacks . Shelley.sgSecurityParam

renderByteArray :: ByteString -> Text
renderByteArray = Text.decodeUtf8 . Base16.encode

txHashOfAddress :: Shelley.Addr TPraosStandardCrypto -> ByteString
txHashOfAddress = Shelley.serialiseAddr

unCoin :: Shelley.Coin -> Word64
unCoin (Shelley.Coin c) = fromIntegral c

-- | The genesis data is a NominalDiffTime (in picoseconds) and we need
-- it as milliseconds.
configSlotDuration :: ShelleyGenesis TPraosStandardCrypto -> Word64
configSlotDuration sg =
  floor $ 1e-6 * Shelley.getSlotLength (Shelley.sgSlotLength sg)
