module Test.Cardano.Db.Mock.UnifiedApi where

import           Control.Monad.Class.MonadSTM.Strict

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Ledger.Core as Core

import           Ouroboros.Consensus.Cardano.Block (AlonzoEra, ShelleyEra, StandardCrypto)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter
import           Cardano.Mock.Forging.Types

import           Control.Monad

forgeNextAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> MockBlock -> IO CardanoBlock
forgeNextAndSubmit inter mockServer testBlock = do
    blk <- forgeNext inter testBlock
    atomically $ addBlock mockServer blk
    pure blk

forgeNextFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> [TxEra] -> IO CardanoBlock
forgeNextFindLeaderAndSubmit interpreter mockServer txs'  = do
    blk <- forgeNextFindLeader interpreter txs'
    atomically $ addBlock mockServer blk
    pure blk

forgeAndSubmitBlocks :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
forgeAndSubmitBlocks interpreter mockServer blocksToCreate = do
    forM [1..blocksToCreate] $ \_ -> forgeNextFindLeaderAndSubmit interpreter mockServer []

withAlonzoFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock
                              -> (LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
                                 -> Either ForgingError [Core.Tx (AlonzoEra StandardCrypto)])
                              -> IO CardanoBlock
withAlonzoFindLeaderAndSubmit interpreter mockServer mkTxs = do
    alTxs <- withAlonzoLedgerState interpreter mkTxs
    forgeNextFindLeaderAndSubmit interpreter mockServer (TxAlonzo <$> alTxs)

withAlonzoFindLeaderAndSubmitTx :: Interpreter -> ServerHandle IO CardanoBlock
                                -> (LedgerState (ShelleyBlock (AlonzoEra StandardCrypto))
                                   -> Either ForgingError (Core.Tx (AlonzoEra StandardCrypto)))
                                -> IO CardanoBlock
withAlonzoFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
    withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx <- mkTxs st
        pure [tx]

withShelleyFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock
                              -> (LedgerState (ShelleyBlock (ShelleyEra StandardCrypto))
                                 -> Either ForgingError [Core.Tx (ShelleyEra StandardCrypto)])
                              -> IO CardanoBlock
withShelleyFindLeaderAndSubmit interpreter mockServer mkTxs = do
    alTxs <- withShelleyLedgerState interpreter mkTxs
    forgeNextFindLeaderAndSubmit interpreter mockServer (TxShelley <$> alTxs)

withShelleyFindLeaderAndSubmitTx :: Interpreter -> ServerHandle IO CardanoBlock
                                 -> (LedgerState (ShelleyBlock (ShelleyEra StandardCrypto))
                                    -> Either ForgingError (Core.Tx (ShelleyEra StandardCrypto)))
                                 -> IO CardanoBlock
withShelleyFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
    withShelleyFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx <- mkTxs st
        pure [tx]

getAlonzoLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock (AlonzoEra StandardCrypto)))
getAlonzoLedgerState interpreter = withAlonzoLedgerState interpreter Right

skipUntilNextEpoch :: Interpreter -> ServerHandle IO CardanoBlock -> [TxEra] -> IO CardanoBlock
skipUntilNextEpoch interpreter mockServer txsEra = do
    slot <- getCurrentSlot interpreter
    let skipSlots = 500 - mod (unSlotNo slot) 500
    blk <- forgeNextAfter interpreter skipSlots txsEra
    atomically $ addBlock mockServer blk
    pure blk

-- First block of next epoch is also submited
fillUntilNextEpoch :: Interpreter -> ServerHandle IO CardanoBlock -> IO [CardanoBlock]
fillUntilNextEpoch interpreter mockServer = do
    startingEpochNo <- getCurrentEpoch interpreter
    let
      go n blks = do
        blk <- forgeNextFindLeader interpreter []
        atomically $ addBlock mockServer blk
        epochNo' <- getCurrentEpoch interpreter
        if epochNo' == startingEpochNo
        then go (n + 1) (blk : blks)
        else pure $ reverse (blk : blks)
    go (0 :: Int) []

-- | Returns number of blocks submitted
fillEpochs :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
fillEpochs interpreter mockServer epochs = do
    blks <- replicateM epochs $ fillUntilNextEpoch interpreter mockServer
    pure $ concat blks

-- Proviging 30 in percentage will create number of blocks that aproximately fill 30% of epoch.
-- | Returns number of blocks submitted
fillEpochPercentage :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
fillEpochPercentage interpreter mockServer percentage = do
    let blocksToCreate = div (percentage * blocksPerEpoch) 100
    replicateM blocksToCreate $forgeNextFindLeaderAndSubmit interpreter mockServer []

registerAllStakeCreds :: Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
registerAllStakeCreds interpreter mockServer = do
    blk <- forgeWithStakeCreds interpreter
    atomically $ addBlock mockServer blk
    pure blk

-- Expected number. This should be taken by the parameters, instead of hardcoded.
blocksPerEpoch :: Int
blocksPerEpoch = 100
