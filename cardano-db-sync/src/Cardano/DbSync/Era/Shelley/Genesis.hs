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
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types (SyncEnv (envBackend))
import Cardano.DbSync.Cache.Types (Cache (..), uninitiatedCache)
import qualified Cardano.DbSync.Era.Shelley.Generic.Util as Generic
import Cardano.DbSync.Era.Shelley.Insert
import Cardano.DbSync.Era.Util (liftLookupFail)
import Cardano.DbSync.Error
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj))
import qualified Cardano.Ledger.Shelley.Genesis as Shelley
import Cardano.Ledger.Shelley.Scripts ()
import qualified Cardano.Ledger.Shelley.Tx as ShelleyTx
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
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
  hasConsumed <- liftIO $ getHasConsumedOrPruneTxOut syncEnv
  prunes <- liftIO $ getPrunes syncEnv
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise *way* too chatty.
  when (not shelleyInitiation && (hasInitialFunds || hasStakes)) $ do
    liftIO $ logError tracer $ show SNErrIgnoreShelleyInitiation
    throwError SNErrIgnoreShelleyInitiation
  if False
    then newExceptT $ DB.runDbIohkLogging (envBackend syncEnv) tracer (insertAction hasConsumed prunes)
    else newExceptT $ DB.runDbIohkNoLogging (envBackend syncEnv) (insertAction hasConsumed prunes)
  where
    tracer = getTrace syncEnv

    hasInitialFunds :: Bool
    hasInitialFunds = not $ null $ ListMap.unListMap $ sgInitialFunds cfg

    hasStakes :: Bool
    hasStakes = sgStaking cfg /= emptyGenesisStaking

    expectedTxCount :: Word64
    expectedTxCount = fromIntegral $ genesisUTxOSize cfg + if hasStakes then 1 else 0

    insertAction :: (MonadBaseControl IO m, MonadIO m) => Bool -> Bool -> ReaderT SqlBackend m (Either SyncNodeError ())
    insertAction hasConsumed prunes = do
      ebid <- DB.queryBlockId (configGenesisHash cfg)
      case ebid of
        Right bid -> validateGenesisDistribution prunes tracer networkName cfg bid expectedTxCount
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
              lift $ mapM_ (insertTxOuts tracer hasConsumed bid) $ genesisUtxOs cfg
              liftIO . logInfo tracer $
                "Initial genesis distribution populated. Hash "
                  <> renderByteArray (configGenesisHash cfg)
              when hasStakes $
                insertStaking tracer uninitiatedCache bid cfg
              supply <- lift DB.queryTotalSupply
              liftIO $ logInfo tracer ("Total genesis supply of Ada: " <> DB.renderAda supply)

-- | Validate that the initial Genesis distribution in the DB matches the Genesis data.
validateGenesisDistribution ::
  (MonadBaseControl IO m, MonadIO m) =>
  Bool ->
  Trace IO Text ->
  Text ->
  ShelleyGenesis StandardCrypto ->
  DB.BlockId ->
  Word64 ->
  ReaderT SqlBackend m (Either SyncNodeError ())
validateGenesisDistribution prunes tracer networkName cfg bid expectedTxCount =
  runExceptT $ do
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
    totalSupply <- lift DB.queryShelleyGenesisSupply
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
  Trace IO Text ->
  Bool ->
  DB.BlockId ->
  (ShelleyTx.TxIn StandardCrypto, Shelley.ShelleyTxOut StandardShelley) ->
  ReaderT SqlBackend m ()
insertTxOuts trce hasConsumed blkId (ShelleyTx.TxIn txInId _, txOut) = do
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
        }
  _ <- insertStakeAddressRefIfMissing trce uninitiatedCache txId (txOut ^. Core.addrTxOutL)
  void . DB.insertTxOutPlex hasConsumed $
    DB.TxOut
      { DB.txOutTxId = txId
      , DB.txOutIndex = 0
      , DB.txOutAddress = Generic.renderAddress addr
      , DB.txOutAddressRaw = Ledger.serialiseAddr addr
      , DB.txOutAddressHasScript = hasScript
      , DB.txOutPaymentCred = Generic.maybePaymentCred addr
      , DB.txOutStakeAddressId = Nothing -- No stake addresses in Shelley Genesis
      , DB.txOutValue = Generic.coinToDbLovelace (txOut ^. Core.valueTxOutL)
      , DB.txOutDataHash = Nothing -- No output datum in Shelley Genesis
      , DB.txOutInlineDatumId = Nothing
      , DB.txOutReferenceScriptId = Nothing
      }
  where
    addr = txOut ^. Core.addrTxOutL

    hasScript = maybe False Generic.hasCredScript (Generic.getPaymentCred addr)

-- Insert pools and delegations coming from Genesis.
insertStaking ::
  (MonadBaseControl IO m, MonadIO m) =>
  Trace IO Text ->
  Cache ->
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
          }
  let params = zip [0 ..] $ ListMap.elems $ sgsPools $ sgStaking genesis
  let network = sgNetworkId genesis
  forM_ params $ uncurry (insertPoolRegister tracer uninitiatedCache (const False) network 0 blkId txId)
  let stakes = zip [0 ..] $ ListMap.toList (sgsStake $ sgStaking genesis)
  forM_ stakes $ \(n, (keyStaking, keyPool)) -> do
    insertStakeRegistration (EpochNo 0) txId (2 * n) (Generic.annotateStakingCred network (KeyHashObj keyStaking))
    insertDelegation cache network 0 0 txId (2 * n + 1) Nothing (KeyHashObj keyStaking) keyPool

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
    unTxOut :: Shelley.ShelleyTxOut StandardShelley -> Ledger.Coin
    unTxOut txOut = txOut ^. Core.valueTxOutL

genesisUtxOs :: ShelleyGenesis StandardCrypto -> [(ShelleyTx.TxIn StandardCrypto, Shelley.ShelleyTxOut StandardShelley)]
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
