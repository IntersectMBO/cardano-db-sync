{-# LANGUAGE TypeApplications #-}
module Cardano.Db.Delete
  ( deleteAfterBlockNo
  , deleteEverything
  , deleteCascadeBlock
  , deleteCascadeAfter
  , deleteCascadeBlockNo
  , deleteCascadeSlotNo
  , deleteDelistedPool
  ) where

import           Cardano.Db.Query (isJust)
import           Cardano.Db.Run (transactionCommit)
import           Cardano.Db.Schema

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString (ByteString)
import           Data.Int (Int64)

import           Database.Esqueleto.Experimental (SqlExpr, Value, delete, deleteCount, from, just,
                   table, val, where_, (==.), (>.), (^.))

import           Database.Persist.Sql (SqlBackend)


-- | Delete all blocks with a block number greater than or equal to the supplied `BlockNo`.
-- Returns 'True' if any blocks were deleted and 'False' if none were found.
deleteAfterBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteAfterBlockNo blockNo = do
    count <- deleteCount $ do
              blk <- from $ table @Block
              where_ (nonGenesisBlockNo $ blk ^. BlockBlockNo)

    delete $ from (table @CollateralTxIn) >>= \ cti -> where_ (nonGenesisBlockNo $ cti ^. CollateralTxInBlockNo)
    delete $ from (table @CostModel) >>= \ cm -> where_ (nonGenesisBlockNo $ cm ^. CostModelBlockNo)
    delete $ from (table @Delegation) >>= \ d -> where_ (nonGenesisBlockNo $ d ^. DelegationBlockNo)
    delete $ from (table @ParamProposal) >>= \ pp -> where_ (nonGenesisBlockNo $ pp ^. ParamProposalBlockNo)
    delete $ from (table @Reserve) >>= \ r -> where_ (nonGenesisBlockNo $ r ^. ReserveBlockNo)
    delete $ from (table @StakeDeregistration) >>= \ sd -> where_ (nonGenesisBlockNo $ sd ^. StakeDeregistrationBlockNo)
    delete $ from (table @StakeRegistration) >>= \ sr -> where_ (nonGenesisBlockNo $ sr ^. StakeRegistrationBlockNo)
    delete $ from (table @Treasury) >>= \ tr -> where_ (nonGenesisBlockNo $ tr ^. TreasuryBlockNo)
    delete $ from (table @Tx) >>= \ tx -> where_ (nonGenesisBlockNo $ tx ^. TxBlockNo)
    delete $ from (table @TxIn) >>= \ txi -> where_ (nonGenesisBlockNo $ txi ^. TxInBlockNo)
    delete $ from (table @TxOut) >>= \ txo -> where_ (nonGenesisBlockNo $ txo ^. TxOutBlockNo)
    delete $ from (table @Withdrawal) >>= \ w -> where_ (nonGenesisBlockNo $ w ^. WithdrawalBlockNo)

    transactionCommit
    pure $ isNonZero count
  where
    blkNo :: Int64
    blkNo = fromIntegral $ unBlockNo blockNo

    -- Byron and Shelley genesis blocks have block nos of -1 and -2 repectively.
    nonGenesisBlockNo :: SqlExpr (Value Int64) -> SqlExpr (Value Bool)
    nonGenesisBlockNo expr = expr >. val blkNo


deleteEverything :: MonadIO m => ReaderT SqlBackend m ()
deleteEverything = do
  delete $ from (table @Block) >>= \ _blk -> where_ (val True)
  delete $ from (table @CollateralTxIn) >>= \ _cti -> where_ (val True)
  delete $ from (table @Delegation) >>= \ _d -> where_ (val True)
  delete $ from (table @ParamProposal) >>= \ _pp -> where_ (val True)
  delete $ from (table @Reserve) >>= \ _r -> where_ (val True)
  delete $ from (table @StakeDeregistration) >>= \ _sd -> where_ (val True)
  delete $ from (table @StakeRegistration) >>= \ _sr -> where_ (val True)
  delete $ from (table @Treasury) >>= \ _tr -> where_ (val True)
  delete $ from (table @Tx) >>= \ _tx -> where_ (val True)
  delete $ from (table @TxIn) >>= \ _txi -> where_ (val True)
  delete $ from (table @TxOut) >>= \ _txo -> where_ (val True)
  delete $ from (table @Withdrawal) >>= \ _w -> where_ (val True)
  transactionCommit


-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlock :: MonadIO m => Block -> ReaderT SqlBackend m Bool
deleteCascadeBlock block = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockHash ==. val (blockHash block))

-- | Delete a block after the specified 'BlockId'. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeAfter :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeAfter (BlockNo blkNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (isJust $ blk ^. BlockEpochNo)
      where_ (blk ^. BlockBlockNo ==. val (fromIntegral blkNo))

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeBlockNo :: MonadIO m => BlockNo -> ReaderT SqlBackend m Bool
deleteCascadeBlockNo (BlockNo blkNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockBlockNo ==. val (fromIntegral blkNo))

-- | Delete a block if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteCascadeSlotNo :: MonadIO m => SlotNo -> ReaderT SqlBackend m Bool
deleteCascadeSlotNo (SlotNo slotNo) = do
  isNonZero <$$>
    deleteCount $ do
      blk <- from $ table @Block
      where_ (blk ^. BlockSlotNo ==. just (val slotNo))

-- | Delete a delisted pool if it exists. Returns 'True' if it did exist and has been
-- deleted and 'False' if it did not exist.
deleteDelistedPool :: MonadIO m => ByteString -> ReaderT SqlBackend m Bool
deleteDelistedPool poolHash = do
  isNonZero <$$>
    deleteCount $ do
      delisted <- from $ table @DelistedPool
      where_ (delisted ^. DelistedPoolHashRaw ==. val poolHash)

-- -------------------------------------------------------------------------------------------------

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

isNonZero :: Int64 -> Bool
isNonZero = (> 0)
