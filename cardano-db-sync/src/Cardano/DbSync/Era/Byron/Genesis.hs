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
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (Hash, fromCompactRedeemVerificationKey, serializeCborHash)

import qualified Cardano.Db as DB
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Except.Extra (newExceptT, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Class (updateWhere)
import           Database.Persist.Sql (SqlBackend, (=.), (==.))

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDist
    :: Trace IO Text -> Text -> Byron.Config
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
            count <- lift DB.queryBlockCount
            when (count > 0) $
              dbSyncNodeError "insertValidateGenesisDist: Genesis data mismatch."
            void . lift $ DB.insertMeta $ DB.Meta
                                    (Byron.unBlockCount $ Byron.configK cfg)
                                    (Byron.configSlotDuration cfg)
                                    (Byron.configStartTime cfg)
                                    (10 * Byron.unBlockCount (Byron.configK cfg))
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
                        , DB.blockTime = Byron.configStartTime cfg
                        , DB.blockTxCount = 0

                        -- Shelley specific
                        , DB.blockVrfKey = Nothing
                        , DB.blockNonceVrf = Nothing
                        , DB.blockLeaderVrf = Nothing
                        , DB.blockOpCert = Nothing
                        , DB.blockProtoVersion = Nothing
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisTxos cfg
            liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                            <> renderByteArray (configGenesisHash cfg)

            supply <- lift $ DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Text -> Byron.Config -> DB.BlockId
    -> ReaderT SqlBackend m (Either DbSyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    meta <- liftLookupFail "validateGenesisDistribution" $ DB.queryMeta

    when (DB.metaProtocolConst meta /= Byron.unBlockCount (Byron.configK cfg)) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch protocol constant. Config value "
            , textShow (Byron.unBlockCount $ Byron.configK cfg)
            , " does not match DB value of ", textShow (DB.metaProtocolConst meta)
            ]

    when (DB.metaSlotDuration meta /= Byron.configSlotDuration cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch slot duration time. Config value "
            , textShow (Byron.configSlotDuration cfg)
            , " does not match DB value of ", textShow (DB.metaSlotDuration meta)
            ]

    when (DB.metaStartTime meta /= Byron.configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch chain start time. Config value "
            , textShow (Byron.configStartTime cfg)
            , " does not match DB value of ", textShow (Byron.configStartTime cfg)
            ]

    case DB.metaNetworkName meta of
      Nothing -> lift $ updateWhere
                  [DB.MetaStartTime ==. Byron.configStartTime cfg]
                  [DB.MetaNetworkName =. Just networkName]
      Just name ->
        when (name /= networkName) $
          dbSyncNodeError $ Text.concat
              [ "validateGenesisDistribution: Provided network name "
              , networkName
              , " does not match DB value "
              , name
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
    case configGenesisSupply cfg of
      Left err -> dbSyncNodeError $ "validateGenesisDistribution: " <> textShow err
      Right expectedSupply ->
        when (DB.word64ToAda expectedSupply /= totalSupply) $
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

insertTxOuts :: (MonadBaseControl IO m, MonadIO m) => DB.BlockId -> (Byron.Address, Byron.Lovelace) -> ReaderT SqlBackend m ()
insertTxOuts blkId (address, value) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = Byron.unTxHash $ txHashOfAddress address
              , DB.txBlock = blkId
              , DB.txBlockIndex = 0
              , DB.txOutSum = Byron.unsafeGetLovelace value
              , DB.txFee = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Text.decodeUtf8 $ Byron.addrToBase58 address
              , DB.txOutValue = Byron.unsafeGetLovelace value
              }

-- -----------------------------------------------------------------------------

configGenesisHash :: Byron.Config -> ByteString
configGenesisHash =
  Byron.unAbstractHash . Byron.unGenesisHash . Byron.configGenesisHash

genesisHashSlotLeader :: Byron.Config -> ByteString
genesisHashSlotLeader =
  BS.take 28 . configGenesisHash


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
