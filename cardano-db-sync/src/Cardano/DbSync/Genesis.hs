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

module Cardano.DbSync.Genesis
  ( insertValidateGenesisDistribution
  ) where

import           Cardano.Prelude

import qualified Cardano.Crypto as Crypto

import           Cardano.BM.Trace (Trace, logInfo)
import qualified Cardano.Chain.Common as Ledger
import qualified Cardano.Chain.Genesis as Ledger
import qualified Cardano.Chain.UTxO as Ledger

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Except.Extra (runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Database.Persist.Class (updateWhere)
import           Database.Persist.Sql (SqlBackend, (=.), (==.))

import qualified Cardano.Db as DB
import           Cardano.DbSync.Error
import           Cardano.DbSync.Util

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
insertValidateGenesisDistribution
    :: Trace IO Text -> Text -> Ledger.Config
    -> IO (Either DbSyncNodeError ())
insertValidateGenesisDistribution tracer networkName cfg = do
    -- Setting this to True will log all 'Persistent' operations which is great
    -- for debugging, but otherwise *way* too chatty.
    if False
      then DB.runDbIohkLogging tracer insertAction
      else DB.runDbNoLogging insertAction
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
              dbSyncNodeError "insertValidateGenesisDistribution: Genesis data mismatch."
            void . lift $ DB.insertMeta $ DB.Meta
                                    (Ledger.unBlockCount $ Ledger.configK cfg)
                                    (configSlotDuration cfg)
                                    (Ledger.configStartTime cfg)
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
                        , DB.blockTime = Ledger.configStartTime cfg
                        , DB.blockTxCount = 0
                        }
            lift $ mapM_ (insertTxOuts bid) $ genesisTxos cfg
            liftIO . logInfo tracer $ "Initial genesis distribution populated. Hash "
                            <> renderAbstractHash (configGenesisHash cfg)

            supply <- lift $ DB.queryTotalSupply
            liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution
    :: MonadIO m
    => Trace IO Text -> Text -> Ledger.Config -> DB.BlockId
    -> ReaderT SqlBackend m (Either DbSyncNodeError ())
validateGenesisDistribution tracer networkName cfg bid =
  runExceptT $ do
    meta <- liftLookupFail "validateGenesisDistribution" $ DB.queryMeta

    when (DB.metaProtocolConst meta /= Ledger.unBlockCount (Ledger.configK cfg)) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch protocol constant. Config value "
            , textShow (Ledger.unBlockCount $ Ledger.configK cfg)
            , " does not match DB value of ", textShow (DB.metaProtocolConst meta)
            ]

    when (DB.metaSlotDuration meta /= configSlotDuration cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch slot duration time. Config value "
            , textShow (configSlotDuration cfg)
            , " does not match DB value of ", textShow (configSlotDuration cfg)
            ]

    when (DB.metaStartTime meta /= Ledger.configStartTime cfg) $
      dbSyncNodeError $ Text.concat
            [ "Mismatch chain start time. Config value "
            , textShow (Ledger.configStartTime cfg)
            , " does not match DB value of ", textShow (Ledger.configStartTime cfg)
            ]

    case DB.metaNetworkName meta of
      Nothing -> lift $ updateWhere
                  [DB.MetaStartTime ==. Ledger.configStartTime cfg]
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

insertTxOuts :: MonadIO m => DB.BlockId -> (Ledger.Address, Ledger.Lovelace) -> ReaderT SqlBackend m ()
insertTxOuts blkId (address, value) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <- DB.insertTx $
            DB.Tx
              { DB.txHash = unTxHash $ txHashOfAddress address
              , DB.txBlock = blkId
              , DB.txOutSum = Ledger.unsafeGetLovelace value
              , DB.txFee = 0
              , DB.txSize = 0 -- Genesis distribution address to not have a size.
              }
  void . DB.insertTxOut $
            DB.TxOut
              { DB.txOutTxId = txId
              , DB.txOutIndex = 0
              , DB.txOutAddress = Text.decodeUtf8 $ Ledger.addrToBase58 address
              , DB.txOutValue = Ledger.unsafeGetLovelace value
              }

-- -----------------------------------------------------------------------------

configGenesisHash :: Ledger.Config -> ByteString
configGenesisHash =
  unAbstractHash . Ledger.unGenesisHash . Ledger.configGenesisHash

genesisHashSlotLeader :: Ledger.Config -> ByteString
genesisHashSlotLeader =
  BS.take 28 . configGenesisHash


configGenesisSupply :: Ledger.Config -> Either Ledger.LovelaceError Word64
configGenesisSupply =
  fmap Ledger.unsafeGetLovelace . Ledger.sumLovelace . map snd . genesisTxos

genesisTxos :: Ledger.Config -> [(Ledger.Address, Ledger.Lovelace)]
genesisTxos config =
    avvmBalances <> nonAvvmBalances
  where
    avvmBalances :: [(Ledger.Address, Ledger.Lovelace)]
    avvmBalances =
      first (Ledger.makeRedeemAddress networkMagic . Crypto.fromCompactRedeemVerificationKey)
        <$> Map.toList (Ledger.unGenesisAvvmBalances $ Ledger.configAvvmDistr config)

    networkMagic :: Ledger.NetworkMagic
    networkMagic = Ledger.makeNetworkMagic (Ledger.configProtocolMagic config)

    nonAvvmBalances :: [(Ledger.Address, Ledger.Lovelace)]
    nonAvvmBalances =
      Map.toList $ Ledger.unGenesisNonAvvmBalances (Ledger.configNonAvvmBalances config)

txHashOfAddress :: Ledger.Address -> Crypto.Hash Ledger.Tx
txHashOfAddress = coerce . Crypto.hash
