{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.BulkConfig (
  -- * Bulk size functions
  getBulkSize,

  -- * Chunking functions
  chunkForBulkQuery,
  chunkForBulkQueryWith,

  -- * bulksize helpers
  getTxOutBulkSize,
  getMaTxOutBulkSize,
) where

import Cardano.Db.Statement.Types (DbInfo (..))
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, typeRep)

-- Schema imports
import qualified Cardano.Db.Schema.Core.Base as SCB
import qualified Cardano.Db.Schema.Core.GovernanceAndVoting as SGV
import qualified Cardano.Db.Schema.Core.MultiAsset as SMA
import qualified Cardano.Db.Schema.Core.OffChain as SO
import qualified Cardano.Db.Schema.Core.StakeDelegation as SS
import Cardano.Db.Schema.Variants (TxOutVariantType (..))
import qualified Cardano.Db.Schema.Variants.TxOutAddress as SVA
import qualified Cardano.Db.Schema.Variants.TxOutCore as SVC

--------------------------------------------------------------------------------
-- Bulk size configuration maps
--------------------------------------------------------------------------------

-- | Helper functions for common bulk size patterns
staticSize :: Int -> Maybe Bool -> Int
staticSize size _ = size

jsonbAware :: Int -> Int -> Maybe Bool -> Int
jsonbAware withJsonb withoutJsonb = \case
  Just True -> withJsonb -- Schema has JSONB
  _ -> withoutJsonb -- Schema without JSONB or default

-- | Unified bulk size configuration
bulkSizeMap :: Map.Map TypeRep (Maybe Bool -> Int)
bulkSizeMap =
  Map.fromList
    [ -- High volume tables
      (typeRep (Proxy @SS.EpochStake), staticSize 75000)
    , (typeRep (Proxy @SS.Reward), staticSize 50000)
    , (typeRep (Proxy @SS.RewardRest), staticSize 50000)
    , -- Standard tables
      (typeRep (Proxy @SCB.TxIn), staticSize 30000)
    , (typeRep (Proxy @SMA.MaTxMint), staticSize 30000)
    , (typeRep (Proxy @SVC.MaTxOutCore), staticSize 30000)
    , (typeRep (Proxy @SVA.MaTxOutAddress), staticSize 25000)
    , (typeRep (Proxy @SGV.DrepDistr), staticSize 30000)
    , (typeRep (Proxy @SS.Delegation), staticSize 25000)
    , -- TxOut variants
      (typeRep (Proxy @SVC.TxOutCore), staticSize 25000)
    , (typeRep (Proxy @SVA.TxOutAddress), staticSize 20000)
    , -- Lower volume tables
      (typeRep (Proxy @SGV.TreasuryWithdrawal), staticSize 20000)
    , (typeRep (Proxy @SO.OffChainVoteAuthor), staticSize 20000)
    , (typeRep (Proxy @SO.OffChainVoteReference), staticSize 20000)
    , (typeRep (Proxy @SO.OffChainVoteFetchError), staticSize 10000)
    , -- JSONB-aware tables
      (typeRep (Proxy @SCB.TxMetadata), jsonbAware 15000 30000)
    , (typeRep (Proxy @SO.OffChainVoteData), jsonbAware 10000 20000)
    , (typeRep (Proxy @SGV.GovActionProposal), jsonbAware 15000 25000)
    , (typeRep (Proxy @SO.OffChainVoteGovActionData), jsonbAware 15000 25000)
    , (typeRep (Proxy @SO.OffChainVoteExternalUpdate), jsonbAware 15000 20000)
    , (typeRep (Proxy @SO.OffChainVoteDrepData), jsonbAware 15000 20000)
    ]

--------------------------------------------------------------------------------
-- Bulk size lookup functions
--------------------------------------------------------------------------------

-- | Get bulk size for a table type with optional JSONB consideration
--
-- Examples:
-- >>> getBulkSize (Proxy @DB.Reward) Nothing
-- >>> getBulkSize (Proxy @DB.TxMetadata) (Just $ envIsJsonbInSchema syncEnv)
getBulkSize :: forall a. DbInfo a => Proxy a -> Maybe Bool -> Int
getBulkSize proxy jsonbState =
  case Map.lookup (typeRep proxy) bulkSizeMap of
    Just sizeFunc -> sizeFunc jsonbState
    Nothing -> 30000 -- Default size for unmapped tables

--------------------------------------------------------------------------------
-- Bulk insert helpers
--------------------------------------------------------------------------------

-- | Chunk a list for bulk database operations using the table's optimal size
--
-- Examples:
-- >>> DB.chunkForBulkQuery (Proxy @DB.Reward) Nothing rewards
-- >>> DB.chunkForBulkQuery (Proxy @DB.TxMetadata) (Just $ envIsJsonbInSchema syncEnv) metadata
chunkForBulkQuery :: forall a b. DbInfo a => Proxy a -> Maybe Bool -> [b] -> [[b]]
chunkForBulkQuery proxy jsonbState = chunkForBulkQueryWith (getBulkSize proxy jsonbState)

-- | Chunk a list with a specific size
chunkForBulkQueryWith :: Int -> [a] -> [[a]]
chunkForBulkQueryWith _ [] = []
chunkForBulkQueryWith n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunkForBulkQueryWith n rest

---------------------------------------------------------------------------------
-- Bulk size helpers
---------------------------------------------------------------------------------

getTxOutBulkSize :: TxOutVariantType -> Int
getTxOutBulkSize txOutVariantType =
  case txOutVariantType of
    TxOutVariantCore -> getBulkSize (Proxy @SVC.TxOutCore) Nothing
    TxOutVariantAddress -> getBulkSize (Proxy @SVA.TxOutAddress) Nothing

getMaTxOutBulkSize :: TxOutVariantType -> Int
getMaTxOutBulkSize txOutVariantType =
  case txOutVariantType of
    TxOutVariantCore -> getBulkSize (Proxy @SVC.MaTxOutCore) Nothing
    TxOutVariantAddress -> getBulkSize (Proxy @SVA.MaTxOutAddress) Nothing
