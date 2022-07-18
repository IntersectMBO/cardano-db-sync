module Test.Cardano.Db.Mock.UnifiedApi
  ( forgeNextAndSubmit
  , forgeNextFindLeaderAndSubmit
  , forgeNextSkipSlotsFindLeaderAndSubmit
  , forgeAndSubmitBlocks
  , withAlonzoFindLeaderAndSubmit
  , withBabbageFindLeaderAndSubmit
  , withAlonzoFindLeaderAndSubmitTx
  , withBabbageFindLeaderAndSubmitTx
  , withShelleyFindLeaderAndSubmit
  , withShelleyFindLeaderAndSubmitTx
  , getAlonzoLedgerState
  , getBabbageLedgerState
  , skipUntilNextEpoch
  , fillUntilNextEpoch
  , fillEpochs
  , fillEpochPercentage
  , registerAllStakeCreds
  ) where

import           Data.Word (Word64)

import           Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Core as Core

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter
import           Cardano.Mock.Forging.Types

import           Cardano.Slotting.Slot (SlotNo (..))

import           Control.Monad (forM, replicateM)
import           Control.Monad.Class.MonadSTM.Strict (atomically)

import           Ouroboros.Consensus.Cardano.Block (ShelleyEra, StandardAlonzo, StandardBabbage,
                   StandardCrypto)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)



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

forgeNextSkipSlotsFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> Word64 -> [TxEra] -> IO CardanoBlock
forgeNextSkipSlotsFindLeaderAndSubmit interpreter mockServer skipSlots txs'  = do
    blk <- forgeNextAfter interpreter skipSlots txs'
    atomically $ addBlock mockServer blk
    pure blk

forgeAndSubmitBlocks :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
forgeAndSubmitBlocks interpreter mockServer blocksToCreate =
    forM [1..blocksToCreate] $ \_ -> forgeNextFindLeaderAndSubmit interpreter mockServer []

withAlonzoFindLeaderAndSubmit
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock TPraosStandard StandardAlonzo)
    -> Either ForgingError [Core.Tx (AlonzoEra StandardCrypto)])
    -> IO CardanoBlock
withAlonzoFindLeaderAndSubmit interpreter mockServer mkTxs = do
    alTxs <- withAlonzoLedgerState interpreter mkTxs
    forgeNextFindLeaderAndSubmit interpreter mockServer (TxAlonzo <$> alTxs)

withBabbageFindLeaderAndSubmit
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock PraosStandard StandardBabbage) -> Either ForgingError [Core.Tx StandardBabbage])
    -> IO CardanoBlock
withBabbageFindLeaderAndSubmit interpreter mockServer mkTxs = do
    alTxs <- withBabbageLedgerState interpreter mkTxs
    forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> alTxs)

withAlonzoFindLeaderAndSubmitTx
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock TPraosStandard StandardAlonzo)
    -> Either ForgingError (Core.Tx (AlonzoEra StandardCrypto)))
    -> IO CardanoBlock
withAlonzoFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
    withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx <- mkTxs st
        pure [tx]

withBabbageFindLeaderAndSubmitTx
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock PraosStandard StandardBabbage) -> Either ForgingError (Core.Tx StandardBabbage))
    -> IO CardanoBlock
withBabbageFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
    withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
        tx <- mkTxs st
        pure [tx]

withShelleyFindLeaderAndSubmit
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock TPraosStandard (ShelleyEra StandardCrypto))
    -> Either ForgingError [Core.Tx (ShelleyEra StandardCrypto)])
    -> IO CardanoBlock
withShelleyFindLeaderAndSubmit interpreter mockServer mkTxs = do
    alTxs <- withShelleyLedgerState interpreter mkTxs
    forgeNextFindLeaderAndSubmit interpreter mockServer (TxShelley <$> alTxs)

withShelleyFindLeaderAndSubmitTx
    :: Interpreter -> ServerHandle IO CardanoBlock
    -> (LedgerState (ShelleyBlock TPraosStandard (ShelleyEra StandardCrypto))
    -> Either ForgingError (Core.Tx (ShelleyEra StandardCrypto)))
    -> IO CardanoBlock
withShelleyFindLeaderAndSubmitTx interpreter mockServer mkTxs =
  withShelleyFindLeaderAndSubmit interpreter mockServer $ \st -> do
    tx <- mkTxs st
    pure [tx]

getAlonzoLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock TPraosStandard StandardAlonzo))
getAlonzoLedgerState interpreter = withAlonzoLedgerState interpreter Right

getBabbageLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock PraosStandard StandardBabbage))
getBabbageLedgerState interpreter = withBabbageLedgerState interpreter Right

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
    go :: Int -> [CardanoBlock] -> IO [CardanoBlock]
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
fillEpochs interpreter mockServer epochs =
  concat <$> replicateM epochs (fillUntilNextEpoch interpreter mockServer)

-- | Providing 30 in percentage will create blocks that approximately fill 30% of epoch.
-- Returns number of blocks submitted
fillEpochPercentage :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
fillEpochPercentage interpreter mockServer percentage = do
  let blocksToCreate = div (percentage * blocksPerEpoch) 100
  replicateM blocksToCreate $forgeNextFindLeaderAndSubmit interpreter mockServer []

registerAllStakeCreds :: Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
registerAllStakeCreds interpreter mockServer = do
  blk <- forgeWithStakeCreds interpreter
  atomically $ addBlock mockServer blk
  pure blk

-- Expected number. This should be taken from the parameters, instead of hardcoded.
blocksPerEpoch :: Int
blocksPerEpoch = 100
