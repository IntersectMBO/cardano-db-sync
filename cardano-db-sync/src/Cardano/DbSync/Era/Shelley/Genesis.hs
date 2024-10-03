{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Genesis (
  insertValidateGenesisDist,
) where

import Cardano.BM.Trace (Trace, logError, logInfo)
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Core.TxOut as C
import qualified Cardano.Db.Schema.Variant.TxOut as V
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (tryUpdateCacheTx)
import Cardano.DbSync.Cache.Types (CacheStatus (..), useNoCache)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertDelegation, insertStakeRegistration)
import Cardano.DbSync.Era.Universal.Insert.Other (insertStakeAddressRefIfMissing)
import Cardano.DbSync.Era.Universal.Insert.Pool (insertPoolRegister)
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Util
import Cardano.Ledger.Address (serialiseAddr)
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Ledger.Shelley.Scripts ()
import Cardano.Ledger.Shelley.TxOut
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.TxIn
import Cardano.Prelude
import Cardano.Slotting.Slot (EpochNo (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except.Extra (newExceptT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time
import Database.Persist.Sql (SqlBackend)
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (StandardCrypto, StandardShelley)
import Ouroboros.Consensus.Shelley.Node (
  ShelleyGenesis (..),
  ShelleyGenesisStaking (..),
  emptyGenesisStaking,
 )
import Paths_cardano_db_sync (version)

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
-- 'shelleyInitiation' is True for testnets that fork at 0 to Shelley.
insertValidateGenesisDist ::
  SyncEnv ->
  Text ->
  ShelleyGenesis StandardCrypto ->
  Bool ->
  ExceptT SyncNodeError IO ()
insertValidateGenesisDist syncEnv networkName cfg shelleyInitiation = do
  let prunes = getPrunes syncEnv
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise *way* too chatty.
  when (not shelleyInitiation && (hasInitialFunds || hasStakes)) $ do
    liftIO $ logError tracer $ show SNErrIgnoreShelleyInitiation
    throwError SNErrIgnoreShelleyInitiation
  if False
    then newExceptT $ DB.runDbIohkLogging (envBackend syncEnv) tracer (insertAction prunes)
    else newExceptT $ DB.runDbIohkNoLogging (envBackend syncEnv) (insertAction prunes)
  where
    tracer = getTrace syncEnv

    hasInitialFunds :: Bool
    hasInitialFunds = not $ null $ ListMap.unListMap $ sgInitialFunds cfg

    hasStakes :: Bool
    hasStakes = sgStaking cfg /= emptyGenesisStaking

    expectedTxCount :: Word64
    expectedTxCount = fromIntegral $ genesisUTxOSize cfg + if hasStakes then 1 else 0

    insertAction :: (MonadBaseControl IO m, MonadIO m) => Bool -> ReaderT SqlBackend m (Either SyncNodeError ())
    insertAction prunes = do
      ebid <- DB.queryBlockId (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution syncEnv prunes networkName cfg bid expectedTxCount
        Left _ ->
          runExceptT $ do
            liftIO $ logInfo tracer "Inserting Shelley Genesis distribution"
            emeta <- lift DB.queryMeta
            case emeta of
              Right _ -> pure () -- Metadata from Shelley era already exists. TODO Validate metadata.
              Left _ -> do
                count <- lift DB.queryBlockCount
                when (count > 0) $
                  dbSyncNodeError $
                    "Shelley.insertValidateGenesisDist: Genesis data mismatch. count " <> textShow count
                void . lift $
                  DB.insertMeta $
                    DB.Meta
                      { DB.metaStartTime = configStartTime cfg
                      , DB.metaNetworkName = networkName
                      , DB.metaVersion = textShow version
                      }
            -- No reason to insert the artificial block if there are no funds or stakes definitions.
            when (hasInitialFunds || hasStakes) $ do
              -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
              -- need this block to attach the genesis distribution transactions to.
              -- It would be nice to not need this artificial block, but that would
              -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
              -- which would be a pain in the neck.
              slid <-
                lift . DB.insertSlotLeader $
                  DB.SlotLeader
                    { DB.slotLeaderHash = genesisHashSlotLeader cfg
                    , DB.slotLeaderPoolHashId = Nothing
                    , DB.slotLeaderDescription = "Shelley Genesis slot leader"
                    }
              -- We attach the Genesis Shelley Block after the block with the biggest Slot.
              -- In most cases this will simply be the Genesis Byron artificial Block,
              -- since this configuration is used for networks which start from Shelley.
              -- This means the previous block will have two blocks after it, resulting in a
              -- tree format, which is unavoidable.
              pid <- lift DB.queryLatestBlockId
              liftIO $ logInfo tracer $ textShow pid
              bid <-
                lift . DB.insertBlock $
                  DB.Block
                    { DB.blockHash = configGenesisHash cfg
                    , DB.blockEpochNo = Nothing
                    , DB.blockSlotNo = Nothing
                    , DB.blockEpochSlotNo = Nothing
                    , DB.blockBlockNo = Nothing
                    , DB.blockPreviousId = pid
                    , DB.blockSlotLeaderId = slid
                    , DB.blockSize = 0
                    , DB.blockTime = configStartTime cfg
                    , DB.blockTxCount = expectedTxCount
                    , -- Genesis block does not have a protocol version, so set this to '0'.
                      DB.blockProtoMajor = 0
                    , DB.blockProtoMinor = 0
                    , -- Shelley specific
                      DB.blockVrfKey = Nothing
                    , DB.blockOpCert = Nothing
                    , DB.blockOpCertCounter = Nothing
                    }
              disInOut <- liftIO $ getDisableInOutState syncEnv
              unless disInOut $ do
                lift $ mapM_ (insertTxOuts syncEnv tracer bid) $ genesisUtxOs cfg
              liftIO . logInfo tracer $
                "Initial genesis distribution populated. Hash "
                  <> renderByteArray (configGenesisHash cfg)
              when hasStakes $
                insertStaking tracer useNoCache bid cfg

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Bool ->
  Text ->
  ShelleyGenesis StandardCrypto ->
  DB.BlockId ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution syncEnv prunes networkName cfg bid expectedTxCount =
  runExceptT $ do
    let tracer = getTrace syncEnv
        txOutTableType = getTxOutTableType syncEnv
    liftIO $ logInfo tracer "Validating Genesis distribution"
    meta <- liftLookupFail "Shelley.validateGenesisDistribution" DB.queryMeta

    when (DB.metaStartTime meta /= configStartTime cfg) $
      dbSyncNodeError $
        Text.concat
          [ "Shelley: Mismatch chain start time. Config value "
          , textShow (configStartTime cfg)
          , " does not match DB value of "
          , textShow (DB.metaStartTime meta)
          ]

    when (DB.metaNetworkName meta /= networkName) $
      dbSyncNodeError $
        Text.concat
          [ "Shelley.validateGenesisDistribution: Provided network name "
          , networkName
          , " does not match DB value "
          , DB.metaNetworkName meta
          ]

    txCount <- lift $ DB.queryBlockTxCount bid
    when (txCount /= expectedTxCount) $
      dbSyncNodeError $
        Text.concat
          [ "Shelley.validateGenesisDistribution: Expected initial block to have "
          , textShow expectedTxCount
          , " but got "
          , textShow txCount
          ]
    totalSupply <- lift $ DB.queryShelleyGenesisSupply txOutTableType
    let expectedSupply = configGenesisSupply cfg
    when (expectedSupply /= totalSupply && not prunes) $
      dbSyncNodeError $
        Text.concat
          [ "Shelley.validateGenesisDistribution: Expected total supply to be "
          , textShow expectedSupply
          , " but got "
          , textShow totalSupply
          ]
    liftIO $ do
      logInfo tracer "Initial genesis distribution present and correct"
      logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda totalSupply)

-- -----------------------------------------------------------------------------

insertTxOuts ::
  (MonadBaseControl IO m, MonadIO m) =>
  SyncEnv ->
  Trace IO Text ->
  DB.BlockId ->
  (TxIn StandardCrypto, ShelleyTxOut StandardShelley) ->
  ReaderT SqlBackend m ()
insertTxOuts syncEnv trce blkId (TxIn txInId _, txOut) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <-
    DB.insertTx $
      DB.Tx
        { DB.txHash = Generic.unTxHash txInId
        , DB.txBlockId = blkId
        , DB.txBlockIndex = 0
        , DB.txOutSum = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
        , DB.txFee = DB.DbLovelace 0
        , DB.txDeposit = Just 0
        , DB.txSize = 0 -- Genesis distribution address to not have a size.
        , DB.txInvalidHereafter = Nothing
        , DB.txInvalidBefore = Nothing
        , DB.txValidContract = True
        , DB.txScriptSize = 0
        , DB.txTreasuryDonation = DB.DbLovelace 0
        }

  tryUpdateCacheTx (envCache syncEnv) txInId txId
  _ <- insertStakeAddressRefIfMissing trce useNoCache (txOut ^. Core.addrTxOutL)
  case ioTxOutTableType . soptInsertOptions $ envOptions syncEnv of
    DB.TxOutCore ->
      void . DB.insertTxOut $
        DB.CTxOutW
          C.TxOut
            { C.txOutAddress = Generic.renderAddress addr
            , C.txOutAddressHasScript = hasScript
            , C.txOutDataHash = Nothing -- No output datum in Shelley Genesis
            , C.txOutIndex = 0
            , C.txOutInlineDatumId = Nothing
            , C.txOutPaymentCred = Generic.maybePaymentCred addr
            , C.txOutReferenceScriptId = Nothing
            , C.txOutStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
            , C.txOutTxId = txId
            , C.txOutValue = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
            , C.txOutConsumedByTxId = Nothing
            }
    DB.TxOutVariantAddress -> do
      addrDetailId <- insertAddress
      void . DB.insertTxOut $ DB.VTxOutW (makeVTxOut addrDetailId txId) Nothing
  where
    addr = txOut ^. Core.addrTxOutL
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)
    addrRaw = serialiseAddr addr

    makeVTxOut :: V.AddressId -> DB.TxId -> V.TxOut
    makeVTxOut addrDetailId txId =
      V.TxOut
        { V.txOutAddressId = addrDetailId
        , V.txOutConsumedByTxId = Nothing
        , V.txOutDataHash = Nothing -- No output datum in Shelley Genesis
        , V.txOutIndex = 0
        , V.txOutInlineDatumId = Nothing
        , V.txOutReferenceScriptId = Nothing
        , V.txOutTxId = txId
        , V.txOutValue = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
        , V.txOutStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
        }

    vAddress :: V.Address
    vAddress =
      V.Address
        { V.addressAddress = Generic.renderAddress addr
        , V.addressRaw = addrRaw
        , V.addressHasScript = hasScript
        , V.addressPaymentCred = Generic.maybePaymentCred addr
        , V.addressStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
        }

    insertAddress ::
      (MonadBaseControl IO m, MonadIO m) =>
      ReaderT SqlBackend m V.AddressId
    insertAddress = do
      mAddrId <- DB.queryAddressId addrRaw
      case mAddrId of
        Nothing -> DB.insertAddress vAddress
        -- this address is already in the database, so we can just return the id to be linked to the txOut.
        Just addrId -> pure addrId

-- Insert pools and delegations coming from Genesis.
insertStaking ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  CacheStatus ->
  DB.BlockId ->
  ShelleyGenesis StandardCrypto ->
  ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertStaking tracer cache blkId genesis = do
  -- All Genesis staking comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <-
    lift $
      DB.insertTx $
        DB.Tx
          { DB.txHash = configGenesisStakingHash
          , DB.txBlockId = blkId
          , DB.txBlockIndex = 0
          , DB.txOutSum = DB.DbLovelace 0
          , DB.txFee = DB.DbLovelace 0
          , DB.txDeposit = Just 0
          , DB.txSize = 0
          , DB.txInvalidHereafter = Nothing
          , DB.txInvalidBefore = Nothing
          , DB.txValidContract = True
          , DB.txScriptSize = 0
          , DB.txTreasuryDonation = DB.DbLovelace 0
          }
  let params = zip [0 ..] $ ListMap.elems $ sgsPools $ sgStaking genesis
  let network = sgNetworkId genesis
  -- TODO: add initial deposits for genesis pools.
  forM_ params $ uncurry (insertPoolRegister tracer useNoCache (const False) Nothing network (EpochNo 0) blkId txId)
  let stakes = zip [0 ..] $ ListMap.toList (sgsStake $ sgStaking genesis)
  forM_ stakes $ \(n, (keyStaking, keyPool)) -> do
    -- TODO: add initial deposits for genesis stake keys.
    insertStakeRegistration tracer cache (EpochNo 0) Nothing txId (2 * n) (Generic.annotateStakingCred network (KeyHashObj keyStaking))
    insertDelegation tracer cache network (EpochNo 0) 0 txId (2 * n + 1) Nothing (KeyHashObj keyStaking) keyPool

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis StandardCrypto -> ByteString
configGenesisHash _ = BS.take 32 ("Shelley Genesis Block Hash " <> BS.replicate 32 '\0')

genesisHashSlotLeader :: ShelleyGenesis StandardCrypto -> ByteString
genesisHashSlotLeader _ = BS.take 28 ("Shelley Genesis SlotLeader Hash" <> BS.replicate 28 '\0')

configGenesisStakingHash :: ByteString
configGenesisStakingHash = BS.take 32 ("Shelley Genesis Staking Tx Hash " <> BS.replicate 32 '\0')

configGenesisSupply :: ShelleyGenesis StandardCrypto -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map Ledger.unCoin . genesisTxoAssocList

genesisUTxOSize :: ShelleyGenesis StandardCrypto -> Int
genesisUTxOSize = length . genesisUtxOs

genesisTxoAssocList :: ShelleyGenesis StandardCrypto -> [Ledger.Coin]
genesisTxoAssocList =
  map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: ShelleyTxOut StandardShelley -> Ledger.Coin
    unTxOut txOut = txOut ^. Core.valueTxOutL

genesisUtxOs :: ShelleyGenesis StandardCrypto -> [(TxIn StandardCrypto, ShelleyTxOut StandardShelley)]
genesisUtxOs =
  Map.toList . Shelley.unUTxO . Shelley.genesisUTxO

configStartTime :: ShelleyGenesis StandardCrypto -> UTCTime
configStartTime = roundToMillseconds . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
  UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
