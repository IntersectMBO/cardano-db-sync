{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Plugin.SMASH
  ( smashDbSyncNodePlugin
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logDebug, logError, logInfo)

import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT)

import           Cardano.SMASH.DB (DBFail (..), DataLayer (..))
import           Cardano.SMASH.Offline (fetchInsertNewPoolMetadata)
import           Cardano.SMASH.Types (PoolIdentifier (..), PoolMetaHash (..), PoolUrl (..))

import           Cardano.Chain.Block (ABlockOrBoundary (..))

import qualified Data.ByteString.Base16 as B16

import           Database.Persist.Sql (IsolationLevel (..), SqlBackend,
                   transactionSaveWithIsolation)

import qualified Cardano.SMASH.DB as DB

import           Cardano.Sync.Error
import           Cardano.Sync.Types as DbSync

import           Cardano.Sync (SyncEnv (..), SyncNodePlugin (..))
import           Cardano.Sync.Util

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import qualified Cardano.DbSync.Era.Shelley.Generic as Shelley
import qualified Cardano.Sync.Era.Byron.Util as Byron

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.TxBody as Shelley

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..))

import           Ouroboros.Consensus.Cardano.Block (HardForkBlock (..), StandardCrypto)

import           Cardano.DbSync.Plugin.Default (defDbSyncNodePlugin)
import           Cardano.DbSync.Plugin.Epoch (epochPluginInsertBlock, epochPluginOnStartup,
                   epochPluginRollbackBlock)

-- | The SMASH node plugin.
smashDbSyncNodePlugin :: Trace IO Text -> SqlBackend -> SyncNodePlugin
smashDbSyncNodePlugin trace' backend =
  let defPlugin = defDbSyncNodePlugin backend
      dataLayer = DB.postgresqlDataLayer backend trace'
  in  defPlugin
        { plugOnStartup =
            plugOnStartup defPlugin
              ++ [epochPluginOnStartup backend]
        , plugInsertBlock =
            plugInsertBlock defPlugin
              ++ [epochPluginInsertBlock backend]
              ++ [insertSMASHBlocks dataLayer backend]
        , plugRollbackBlock =
            plugRollbackBlock defPlugin
              ++ [epochPluginRollbackBlock]
        }

-- For information on what era we are in.
data BlockName
    = Shelley
    | Allegra
    | Mary
    deriving (Eq, Show)

insertSMASHBlocks
    :: DataLayer
    -> SqlBackend
    -> Trace IO Text
    -> SyncEnv
    -> [BlockDetails]
    -> IO (Either SyncNodeError ())
insertSMASHBlocks dataLayer sqlBackend tracer env blockDetails =
    DB.runDbIohkLogging sqlBackend tracer $
      traverseMEither (insertSMASHBlock dataLayer tracer env) blockDetails

-- |TODO(KS): We need to abstract over these blocks so we can test this functionality
-- separatly from the actual blockchain, using tests only.
insertSMASHBlock
    :: DataLayer
    -> Trace IO Text
    -> SyncEnv
    -> BlockDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
insertSMASHBlock dataLayer tracer env (BlockDetails cblk details) =
  case cblk of
    BlockByron blk -> do
      insertByronBlock tracer blk details
    BlockShelley blk -> do
      insertShelleyBlock Shelley dataLayer tracer env (Generic.fromShelleyBlock blk) details
    BlockAllegra blk -> do
      insertShelleyBlock Allegra dataLayer tracer env (Generic.fromAllegraBlock blk) details
    BlockMary blk -> do
      insertShelleyBlock Mary dataLayer tracer env (Generic.fromMaryBlock blk) details



-- We don't care about Byron, no pools there.
-- Also, we don't want to add additional info to clutter the logs.
insertByronBlock
    :: Trace IO Text
    -> ByronBlock
    -> DbSync.SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
insertByronBlock tracer blk _details = do
  case byronBlockRaw blk of
    ABOBBlock byronBlock -> do
        let slotNum = Byron.slotNumber byronBlock

        -- Output in intervals, don't add too much noise to the output.
        when (slotNum `mod` 5000 == 0) $
            liftIO . logDebug tracer $ "Byron block, slot: " <> show slotNum

    ABOBBoundary {} -> pure ()

  return $ Right ()

-- Here we insert pools.
insertShelleyBlock
    :: BlockName
    -> DataLayer
    -> Trace IO Text
    -> SyncEnv
    -> Generic.Block
    -> SlotDetails
    -> ReaderT SqlBackend (LoggingT IO) (Either SyncNodeError ())
insertShelleyBlock blockName dataLayer tracer env blk details = do

  runExceptT $ do

    let blockNumber = Generic.blkBlockNo blk
    zipWithM_ (insertTx dataLayer blockNumber tracer env) [0 .. ] (Shelley.blkTxs blk)

    liftIO $ do
      let epoch = unEpochNo (sdEpochNo details)
          slotWithinEpoch = unEpochSlot (sdEpochSlot details)
          globalSlot = epoch * unEpochSize (sdEpochSize details) + slotWithinEpoch

      when (slotWithinEpoch `mod` 1000 == 0) $
        logDebug tracer $ mconcat
          [ "Insert '", show blockName
          , "' block pool info: epoch ", show epoch
          , ", slot ", show slotWithinEpoch
          , ", block ", show blockNumber
          , ", global slot ", show globalSlot
          ]

    lift $ transactionSaveWithIsolation Serializable

insertTx
    :: (MonadIO m)
    => DataLayer
    -> BlockNo
    -> Trace IO Text
    -> SyncEnv
    -> Word64
    -> Generic.Tx
    -> ExceptT SyncNodeError m ()
insertTx dataLayer blockNumber tracer env _blockIndex tx =
    mapM_ (insertCertificate dataLayer blockNumber tracer env) $ Generic.txCertificates tx

insertCertificate
    :: (MonadIO m)
    => DataLayer
    -> BlockNo
    -> Trace IO Text
    -> SyncEnv
    -> Generic.TxCertificate
    -> ExceptT SyncNodeError m ()
insertCertificate dataLayer blockNumber tracer _env (Generic.TxCertificate _idx cert) =
  case cert of
    Shelley.DCertDeleg _deleg ->
        -- Since at some point we start to have a large number of delegation
        -- certificates, this should be output just in debug mode.
        liftIO $ logDebug tracer "insertCertificate: DCertDeleg"
    Shelley.DCertPool pool ->
        insertPoolCert dataLayer blockNumber tracer pool
    Shelley.DCertMir _mir ->
        liftIO $ logDebug tracer "insertCertificate: DCertMir"
    Shelley.DCertGenesis _gen ->
        liftIO $ logDebug tracer "insertCertificate: DCertGenesis"

insertPoolCert
    :: (MonadIO m)
    => DataLayer
    -> BlockNo
    -> Trace IO Text
    -> Shelley.PoolCert StandardCrypto
    -> ExceptT SyncNodeError m ()
insertPoolCert dataLayer blockNumber tracer pCert =
  case pCert of
    Shelley.RegPool pParams -> do
        let poolIdHash = B16.encode . Generic.unKeyHashRaw $ Shelley._poolId pParams
        let poolId = PoolIdentifier . decodeUtf8 $ poolIdHash

        -- Insert pool id
        let addPool = dlAddPool dataLayer
        addedPool <- liftIO $ addPool poolId

        case addedPool of
          Left _err -> liftIO . logInfo tracer $ "Pool already registered with pool id: " <> decodeUtf8 poolIdHash
          Right _pool -> liftIO . logInfo tracer $ "Inserting pool register with pool id: " <> decodeUtf8 poolIdHash

        -- TODO(KS): Check whether the pool is retired, and if yes,
        -- if the current block number is greater, remove that record.
        let checkRetiredPool = dlCheckRetiredPool dataLayer
        retiredPoolId <- liftIO $ checkRetiredPool poolId

        -- This could be chained, revives the pool if it was re-submited when already being retired.
        case retiredPoolId of
            Left _err -> liftIO . logInfo tracer $ "Pool not retired: " <> decodeUtf8 poolIdHash
            Right (poolId', retiredPoolBlockNo) ->
                -- This is a superfluous check, like this word, but could be relevent in some cases.
                if retiredPoolBlockNo > unBlockNo blockNumber
                    then liftIO . logInfo tracer $ "Pool retired after this block, not reviving: " <> decodeUtf8 poolIdHash
                    else do
                        -- REVIVE retired pool
                        let removeRetiredPool = dlRemoveRetiredPool dataLayer
                        removedPoolId <- liftIO $ removeRetiredPool poolId'

                        case removedPoolId of
                            Left err -> liftIO . logInfo tracer $ "Pool retired, not revived. " <> show err
                            Right removedPoolId' -> liftIO . logInfo tracer $ "Pool retired, revived: " <> show removedPoolId'

        -- Finally, insert the metadata!
        insertPoolRegister dataLayer tracer pParams

    -- RetirePool (KeyHash 'StakePool era) _ = PoolId
    Shelley.RetirePool poolPubKey _epochNum -> do
        let poolIdHash = B16.encode . Generic.unKeyHashRaw $ poolPubKey
        let poolId = PoolIdentifier . decodeUtf8 $ poolIdHash

        liftIO . logInfo tracer $ "Retiring pool with poolId: " <> show poolId

        let addRetiredPool = dlAddRetiredPool dataLayer

        eitherPoolId <- liftIO $ addRetiredPool poolId (unBlockNo blockNumber)

        case eitherPoolId of
            Left err -> liftIO . logError tracer $ "Error adding retiring pool: " <> show err
            Right poolId' -> liftIO . logInfo tracer $ "Added retiring pool with poolId: " <> show poolId'

insertPoolRegister
    :: forall m. (MonadIO m)
    => DataLayer
    -> Trace IO Text
    -> Shelley.PoolParams StandardCrypto
    -> ExceptT SyncNodeError m ()
insertPoolRegister dataLayer tracer params = do
  let poolIdHash = B16.encode . Generic.unKeyHashRaw $ Shelley._poolId params
  let poolId = PoolIdentifier . decodeUtf8 $ poolIdHash

  case strictMaybeToMaybe $ Shelley._poolMD params of
    Just md -> do

        liftIO . logInfo tracer $ "Inserting metadata."
        let metadataUrl = PoolUrl . Shelley.urlToText $ Shelley._poolMDUrl md
        let metadataHash = PoolMetaHash . decodeUtf8 . B16.encode $ Shelley._poolMDHash md

        let addMetaDataReference = dlAddMetaDataReference dataLayer

        -- We need to map this to ExceptT
        refId <- firstExceptT (\(e :: DBFail) -> NEError $ show e) . newExceptT . liftIO $
            addMetaDataReference poolId metadataUrl metadataHash

        liftIO $ fetchInsertNewPoolMetadata dataLayer tracer refId poolId md

        liftIO . logInfo tracer $ "Metadata inserted."

    Nothing -> pure ()

  liftIO . logInfo tracer $ "Inserted pool register."
  pure ()

