{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Cardano.DbSync.KeysThread where

import qualified Cardano.Db as DB
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Era.Shelley.Generic.Block
import Cardano.DbSync.Types
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary.TxBody
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Eras (StandardCrypto)

data Thread = Thread
  { tResults :: ThreadResult
  , tThread :: Async ()
  }

newtype ThreadResult = ThreadResult {trAssets :: TVar (Map (PolicyID StandardCrypto, AssetName) DB.MultiAssetId)}

newEmptyThreadResult :: IO ThreadResult
newEmptyThreadResult =
  ThreadResult <$> newTVarIO Map.empty

spawnKeysThread :: SyncEnv -> [CardanoBlock] -> IO Thread
spawnKeysThread syncEnv blocks = do
  threadResult <- newEmptyThreadResult
  asyncResult <- async $ runThread syncEnv threadResult blocks
  pure $ Thread threadResult asyncResult

runThread :: SyncEnv -> ThreadResult -> [CardanoBlock] -> IO ()
runThread _syncEnv _tr blocks = pure ()
  where
    _assets = extractAssets blocks

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
