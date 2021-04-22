{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.DbSync.Era.Shelley.Insert.Epoch
  ( insertEpochStake
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)

import qualified Cardano.Db as DB

import qualified Cardano.DbSync.Era.Shelley.Generic as Generic
import           Cardano.DbSync.Era.Shelley.Query
import           Cardano.DbSync.Era.Util (liftLookupFail)

import qualified Cardano.Sync.Era.Shelley.Generic as Generic
import           Cardano.Sync.Error
import           Cardano.Sync.Util

import           Cardano.Slotting.Slot (EpochNo (..))

import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.List.Split.Internals (chunksOf)
import qualified Data.Map.Strict as Map

import           Database.Persist.Sql (SqlBackend, putMany)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)

import qualified Shelley.Spec.Ledger.Coin as Shelley
import qualified Shelley.Spec.Ledger.Keys as Shelley



insertEpochStake
    :: (MonadBaseControl IO m, MonadIO m)
    => Trace IO Text -> Generic.StakeDist
    -> ExceptT SyncNodeError (ReaderT SqlBackend m) ()
insertEpochStake tracer smap = do
    forM_ (chunksOf 1000 $ Map.toList (Generic.sdistStakeMap smap)) $ \stakeChunk -> do
      dbStakes <- mapM mkStake stakeChunk
      lift $ putMany dbStakes
    liftIO . logInfo tracer $
      mconcat
        [ "insertEpochStake: Epoch ", textShow (unEpochNo $ Generic.sdistEpochNo smap)
        , ", ", textShow (length $ Generic.sdistStakeMap smap), " stake addresses"
        ]
  where
    epoch :: Word64
    epoch = unEpochNo (Generic.sdistEpochNo smap)

    mkStake
        :: (MonadBaseControl IO m, MonadIO m)
        => (Generic.StakeCred, (Shelley.Coin, Shelley.KeyHash 'Shelley.StakePool StandardCrypto))
        -> ExceptT SyncNodeError (ReaderT SqlBackend m) DB.EpochStake
    mkStake (saddr, (coin, _)) = do
      (saId, poolId) <- liftLookupFail "insertEpochStake" $ queryStakeAddressAndPool epoch (Generic.unStakeCred saddr)
      pure $
        DB.EpochStake
          { DB.epochStakeAddrId = saId
          , DB.epochStakePoolId = poolId
          , DB.epochStakeAmount = Generic.coinToDbLovelace coin
          , DB.epochStakeEpochNo = epoch -- The epoch where this delegation becomes valid.
          }
