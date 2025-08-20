{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Era.Shelley.Genesis (
  insertValidateShelleyGenesisDist,
) where

import Cardano.BM.Trace (logError, logInfo)
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (InsertOptions (..), SyncEnv (..), SyncOptions (..))
import Cardano.DbSync.Cache (insertAddressUsingCache, tryUpdateCacheTx)
import Cardano.DbSync.Cache.Epoch (withNoCache)
import Cardano.DbSync.Cache.Types (CacheAction (..))
import Cardano.DbSync.DbEvent (liftDbLookup, runDbSyncNoTransaction, runDbSyncNoTransactionNoLogging)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Universal.Insert.Certificate (insertDelegation, insertStakeRegistration)
import Cardano.DbSync.Era.Universal.Insert.Other (insertStakeAddressRefIfMissing)
import Cardano.DbSync.Era.Universal.Insert.Pool (insertPoolRegister)
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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime (..))
import qualified Data.Time.Clock as Time
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (ShelleyEra)
import Ouroboros.Consensus.Shelley.Node (
  ShelleyGenesis (..),
  ShelleyGenesisStaking (..),
  emptyGenesisStaking,
 )
import Paths_cardano_db_sync (version)

-- | Idempotent insert the initial Genesis distribution transactions into the DB.
-- If these transactions are already in the DB, they are validated.
-- 'shelleyInitiation' is True for testnets that fork at 0 to Shelley.
insertValidateShelleyGenesisDist ::
  SyncEnv ->
  Text ->
  ShelleyGenesis ->
  Bool ->
  ExceptT SyncNodeError IO ()
insertValidateShelleyGenesisDist syncEnv networkName cfg shelleyInitiation = do
  let prunes = getPrunes syncEnv
  when (not shelleyInitiation && (hasInitialFunds || hasStakes)) $ do
    liftIO $ logError tracer $ show SNErrIgnoreShelleyInitiation
    throwError SNErrIgnoreShelleyInitiation

  case DB.dbTracer $ envDbEnv syncEnv of
    Just trce -> ExceptT $ runDbSyncNoTransaction trce (envDbEnv syncEnv) (insertAction prunes)
    Nothing -> ExceptT $ runDbSyncNoTransactionNoLogging (envDbEnv syncEnv) (insertAction prunes)
  where
    tracer = getTrace syncEnv

    hasInitialFunds :: Bool
    hasInitialFunds = not $ null $ ListMap.unListMap $ sgInitialFunds cfg

    hasStakes :: Bool
    hasStakes = sgStaking cfg /= emptyGenesisStaking

    expectedTxCount :: Word64
    expectedTxCount = fromIntegral $ genesisUTxOSize cfg + if hasStakes then 1 else 0

    insertAction :: Bool -> ExceptT SyncNodeError DB.DbM ()
    insertAction prunes = do
      ebid <- lift $ DB.queryBlockIdEither (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution syncEnv prunes networkName cfg bid expectedTxCount
        Left err -> do
          liftIO $ logInfo tracer "Inserting Shelley Genesis distribution"
          emeta <- liftDbLookup mkSyncNodeCallStack DB.queryMeta
          case emeta of
            Just _ -> pure () -- Metadata from Shelley era already exists.
            Nothing -> do
              count <- lift DB.queryBlockCount
              when (count > 0) $
                throwError $
                  SNErrDbSessionErr mkSyncNodeCallStack $
                    DB.mkDbSessionError (show err <> " Genesis data mismatch. count " <> textShow count)
              void $ lift $ DB.insertMeta metaRecord
          -- No reason to insert the artificial block if there are no funds or stakes definitions.
          when (hasInitialFunds || hasStakes) $ do
            -- Insert an 'artificial' Genesis block (with a genesis specific slot leader). We
            -- need this block to attach the genesis distribution transactions to.
            -- It would be nice to not need this artificial block, but that would
            -- require plumbing the Genesis.Config into 'insertByronBlockOrEBB'
            -- which would be a pain in the neck.
            slid <- lift $ DB.insertSlotLeader slotLeaderRecord
            -- We attach the Genesis Shelley Block after the block with the biggest Slot.
            -- In most cases this will simply be the Genesis Byron artificial Block,
            -- since this configuration is used for networks which start from Shelley.
            -- This means the previous block will have two blocks after it, resulting in a
            -- tree format, which is unavoidable.
            pid <- lift DB.queryLatestBlockId
            liftIO $ logInfo tracer $ textShow pid
            bid <- lift $ DB.insertBlock (blockRecord pid slid)

            disInOut <- liftIO $ getDisableInOutState syncEnv
            unless disInOut $ do
              mapM_ (insertTxOuts syncEnv bid) $ genesisUtxOs cfg

            liftIO
              . logInfo tracer
              $ "Initial genesis distribution populated. Hash "
                <> renderByteArray (configGenesisHash cfg)
            when hasStakes $
              insertStaking (withNoCache syncEnv) bid cfg

    metaRecord =
      DB.Meta
        { DB.metaStartTime = configStartTime cfg
        , DB.metaNetworkName = networkName
        , DB.metaVersion = textShow version
        }

    slotLeaderRecord =
      DB.SlotLeader
        { DB.slotLeaderHash = genesisHashSlotLeader cfg
        , DB.slotLeaderPoolHashId = Nothing
        , DB.slotLeaderDescription = "Shelley Genesis slot leader"
        }

    blockRecord pid slid =
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
        , DB.blockVrfKey = Nothing
        , DB.blockOpCert = Nothing
        , DB.blockOpCertCounter = Nothing
        }

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution ::
  SyncEnv ->
  Bool ->
  Text ->
  ShelleyGenesis ->
  DB.BlockId ->
  Word64 ->
  ExceptT SyncNodeError DB.DbM ()
validateGenesisDistribution syncEnv prunes networkName cfg bid expectedTxCount = do
  let tracer = getTrace syncEnv
      txOutVariantType = getTxOutVariantType syncEnv
  liftIO $ logInfo tracer "Validating Genesis distribution"

  -- During validation, meta MUST exist.
  metaMaybe <- liftDbLookup mkSyncNodeCallStack DB.queryMeta
  meta <- case metaMaybe of
    Just m -> pure m
    Nothing ->
      throwError $
        SNErrDbSessionErr mkSyncNodeCallStack $
          DB.mkDbSessionError
            "Meta table is empty during validation - this should not happen"

  when (DB.metaStartTime meta /= configStartTime cfg) $
    throwError $
      SNErrDbSessionErr mkSyncNodeCallStack $
        DB.mkDbSessionError
          ( Text.concat
              [ "Shelley: Mismatch chain start time. Config value "
              , textShow (configStartTime cfg)
              , " does not match DB value of "
              , textShow (DB.metaStartTime meta)
              ]
          )

  when (DB.metaNetworkName meta /= networkName) $
    throwError $
      SNErrDbSessionErr mkSyncNodeCallStack $
        DB.mkDbSessionError
          ( Text.concat
              [ "Shelley.validateGenesisDistribution: Provided network name "
              , networkName
              , " does not match DB value "
              , DB.metaNetworkName meta
              ]
          )

  txCount <- lift $ DB.queryBlockTxCount bid
  when (txCount /= expectedTxCount) $
    throwError $
      SNErrDbSessionErr mkSyncNodeCallStack $
        DB.mkDbSessionError
          ( Text.concat
              [ "Shelley.validateGenesisDistribution: Expected initial block to have "
              , textShow expectedTxCount
              , " but got "
              , textShow txCount
              ]
          )

  totalSupply <- lift $ DB.queryShelleyGenesisSupply txOutVariantType
  let expectedSupply = configGenesisSupply cfg
  when (expectedSupply /= totalSupply && not prunes) $
    throwError $
      SNErrDbSessionErr mkSyncNodeCallStack $
        DB.mkDbSessionError
          ( Text.concat
              [ "Shelley.validateGenesisDistribution: Expected total supply to be "
              , textShow expectedSupply
              , " but got "
              , textShow totalSupply
              ]
          )

  liftIO $ do
    logInfo tracer "Initial genesis distribution present and correct"
    logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda totalSupply)

-----------------------------------------------------------------------------
insertTxOuts ::
  SyncEnv ->
  DB.BlockId ->
  (TxIn, ShelleyTxOut ShelleyEra) ->
  ExceptT SyncNodeError DB.DbM ()
insertTxOuts syncEnv blkId (TxIn txInId _, txOut) = do
  -- Each address/value pair of the initial coin distribution comes from an artifical transaction
  -- with a hash generated by hashing the address.
  txId <-
    lift $
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
  _ <- insertStakeAddressRefIfMissing (withNoCache syncEnv) (txOut ^. Core.addrTxOutL)
  case ioTxOutVariantType . soptInsertOptions $ envOptions syncEnv of
    DB.TxOutVariantCore ->
      void
        . lift
        $ DB.insertTxOut
        $ DB.VCTxOutW
          VC.TxOutCore
            { VC.txOutCoreAddress = Generic.renderAddress addr
            , VC.txOutCoreAddressHasScript = hasScript
            , VC.txOutCoreDataHash = Nothing -- No output datum in Shelley Genesis
            , VC.txOutCoreIndex = 0
            , VC.txOutCoreInlineDatumId = Nothing
            , VC.txOutCorePaymentCred = Generic.maybePaymentCred addr
            , VC.txOutCoreReferenceScriptId = Nothing
            , VC.txOutCoreStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
            , VC.txOutCoreTxId = txId
            , VC.txOutCoreValue = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
            , VC.txOutCoreConsumedByTxId = Nothing
            }
    DB.TxOutVariantAddress -> do
      addrDetailId <- insertAddressUsingCache syncEnv UpdateCache addrRaw vAddress
      void . lift $ DB.insertTxOut $ DB.VATxOutW (makeVTxOut addrDetailId txId) Nothing
  where
    addr = txOut ^. Core.addrTxOutL
    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)
    addrRaw = serialiseAddr addr

    makeVTxOut :: DB.AddressId -> DB.TxId -> VA.TxOutAddress
    makeVTxOut addrDetailId txId =
      VA.TxOutAddress
        { VA.txOutAddressAddressId = addrDetailId
        , VA.txOutAddressConsumedByTxId = Nothing
        , VA.txOutAddressDataHash = Nothing -- No output datum in Shelley Genesis
        , VA.txOutAddressIndex = 0
        , VA.txOutAddressInlineDatumId = Nothing
        , VA.txOutAddressReferenceScriptId = Nothing
        , VA.txOutAddressTxId = txId
        , VA.txOutAddressValue = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
        , VA.txOutAddressStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
        }

    vAddress :: VA.Address
    vAddress =
      VA.Address
        { VA.addressAddress = Generic.renderAddress addr
        , VA.addressRaw = addrRaw
        , VA.addressHasScript = hasScript
        , VA.addressPaymentCred = Generic.maybePaymentCred addr
        , VA.addressStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
        }

-- Insert pools and delegations coming from Genesis.
insertStaking ::
  SyncEnv ->
  DB.BlockId ->
  ShelleyGenesis ->
  ExceptT SyncNodeError DB.DbM ()
insertStaking syncEnv blkId genesis = do
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
  forM_ params $ uncurry (insertPoolRegister syncEnv (const False) Nothing network (EpochNo 0) blkId txId)
  let stakes = zip [0 ..] $ ListMap.toList (sgsStake $ sgStaking genesis)
  forM_ stakes $ \(n, (keyStaking, keyPool)) -> do
    -- TODO: add initial deposits for genesis stake keys.
    insertStakeRegistration syncEnv (EpochNo 0) Nothing txId (2 * n) (Generic.annotateStakingCred network (KeyHashObj keyStaking))
    insertDelegation syncEnv network (EpochNo 0) 0 txId (2 * n + 1) Nothing (KeyHashObj keyStaking) keyPool

-- -----------------------------------------------------------------------------

configGenesisHash :: ShelleyGenesis -> ByteString
configGenesisHash _ = BS.take 32 ("Shelley Genesis Block Hash " <> BS.replicate 32 '\0')

genesisHashSlotLeader :: ShelleyGenesis -> ByteString
genesisHashSlotLeader _ = BS.take 28 ("Shelley Genesis SlotLeader Hash" <> BS.replicate 28 '\0')

configGenesisStakingHash :: ByteString
configGenesisStakingHash = BS.take 32 ("Shelley Genesis Staking Tx Hash " <> BS.replicate 32 '\0')

configGenesisSupply :: ShelleyGenesis -> DB.Ada
configGenesisSupply =
  DB.word64ToAda . fromIntegral . sum . map Ledger.unCoin . genesisTxoAssocList

genesisUTxOSize :: ShelleyGenesis -> Int
genesisUTxOSize = length . genesisUtxOs

genesisTxoAssocList :: ShelleyGenesis -> [Ledger.Coin]
genesisTxoAssocList =
  map (unTxOut . snd) . genesisUtxOs
  where
    unTxOut :: ShelleyTxOut ShelleyEra -> Ledger.Coin
    unTxOut txOut = txOut ^. Core.valueTxOutL

genesisUtxOs :: ShelleyGenesis -> [(TxIn, ShelleyTxOut ShelleyEra)]
genesisUtxOs =
  Map.toList . Shelley.unUTxO . Shelley.genesisUTxO

configStartTime :: ShelleyGenesis -> UTCTime
configStartTime = roundToMillseconds . Shelley.sgSystemStart

roundToMillseconds :: UTCTime -> UTCTime
roundToMillseconds (UTCTime day picoSecs) =
  UTCTime day (Time.picosecondsToDiffTime $ 1000000 * (picoSeconds `div` 1000000))
  where
    picoSeconds :: Integer
    picoSeconds = Time.diffTimeToPicoseconds picoSecs
