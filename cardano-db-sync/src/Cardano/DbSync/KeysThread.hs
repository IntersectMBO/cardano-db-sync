{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.KeysThread where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Cache.Types
import Cardano.DbSync.Era.Shelley.Generic.Block
import Cardano.DbSync.Era.Universal.Insert.Other
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.TxBody
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Prelude hiding ((.))
import Control.Concurrent.STM
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (SqlBackend)
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import Data.List (nub)

data Thread = Thread
  { tResults :: ThreadResult
  , tThread :: Async ()
  }

newtype ThreadResult = ThreadResult {trAssets :: TVar (Map (PolicyID StandardCrypto, AssetName) DB.MultiAssetId)}

waitThread :: Thread -> IO ()
waitThread = wait . tThread

newEmptyThreadResult :: IO ThreadResult
newEmptyThreadResult =
  ThreadResult <$> newTVarIO Map.empty

getAssetId :: (MonadIO m, MonadBaseControl IO m) => Maybe Thread -> CacheStatus -> PolicyID StandardCrypto -> AssetName -> ReaderT SqlBackend m DB.MultiAssetId
getAssetId mThread cache policy name = case mThread of
  Nothing -> insertMultiAsset cache policy name
  Just thread -> liftIO $ atomically $ do
    mp <- readTVar $ trAssets (tResults thread)
    maybe retry pure (Map.lookup (policy, name) mp)

spawnKeysThread :: SyncEnv -> [CardanoBlock] -> IO Thread
spawnKeysThread syncEnv blocks = do
  threadResult <- newEmptyThreadResult
  asyncResult <- async $ runThread syncEnv threadResult blocks
  pure $ Thread threadResult asyncResult

runThread :: SyncEnv -> ThreadResult -> [CardanoBlock] -> IO ()
runThread syncEnv tr blocks = do
  DB.runDbIohkLogging connection tracer $ forM_ (nub assets) $ \(policy, name) -> do
    maId <- insertMultiAsset (envCache syncEnv) policy name
    liftIO $ atomically $ modifyTVar (trAssets tr) $ Map.insert (policy, name) maId
  where
    assets = extractAssets blocks
    connection = assetsBackend $ encSecondaryBackends syncEnv
    tracer = getTrace syncEnv

-- We want to extract them in the same order that they will be consumed.
-- This is an optimization property and correctness doesn't rely on it.
extractAssets :: [CardanoBlock] -> [(PolicyID StandardCrypto, AssetName)]
extractAssets = concatMap extractAssetsBlock

extractAssetsBlock :: CardanoBlock -> [(PolicyID StandardCrypto, AssetName)]
extractAssetsBlock = \case
  BlockByron _ -> []
  BlockShelley _ -> []
  BlockAllegra _ -> []
  BlockMary blk -> concatMap extractAssetsTxMary (snd <$> getTxs blk)
  BlockAlonzo blk -> concatMap extractAssetsTxMary (snd <$> getTxs blk)
  BlockBabbage blk -> concatMap extractAssetsTxMary (snd <$> getTxs blk)
  BlockConway blk -> concatMap extractAssetsTxMary (snd <$> getTxs blk)

extractAssetsTxMary ::
  (Core.EraTx era, MaryEraTxBody era, Core.Value era ~ MaryValue StandardCrypto, Core.EraCrypto era ~ StandardCrypto) =>
  Core.Tx era ->
  [(PolicyID StandardCrypto, AssetName)]
extractAssetsTxMary tx = outputsAssets <> mintassets
  where
    txBody = tx ^. Core.bodyTxL
    outputs = toList (txBody ^. Core.outputsTxBodyL)
    outputsAssets = concatMap extractAssetsOutput outputs
    extractAssetsOutput output = case output ^. Core.valueTxOutL of
      MaryValue _ada ma -> extractMultiAsset ma
    extractMultiAsset (MultiAsset maMap) =
      concatMap (\(policy, names) -> (policy,) <$> names) $ Map.toList $ Map.map (fmap fst . Map.toList) maMap
    txMint = txBody ^. mintTxBodyL
    mintassets = extractMultiAsset txMint

shouldMultiAssetRun :: SyncEnv -> Bool
shouldMultiAssetRun syncEnv =
  ioInOut insOptions && ioMultiAssets insOptions
  where
    insOptions = soptInsertOptions $ envOptions syncEnv
