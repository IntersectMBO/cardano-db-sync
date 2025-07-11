module Test.Cardano.Db.Mock.UnifiedApi (
  forgeNextAndSubmit,
  forgeNextFindLeaderAndSubmit,
  forgeNextSkipSlotsFindLeaderAndSubmit,
  forgeAndSubmitBlocks,
  withAlonzoFindLeaderAndSubmit,
  withBabbageFindLeaderAndSubmit,
  withConwayFindLeaderAndSubmit,
  withAlonzoFindLeaderAndSubmitTx,
  withBabbageFindLeaderAndSubmitTx,
  withConwayFindLeaderAndSubmitTx,
  withShelleyFindLeaderAndSubmit,
  withShelleyFindLeaderAndSubmitTx,
  getShelleyLedgerState,
  getAlonzoLedgerState,
  getBabbageLedgerState,
  getConwayLedgerState,
  skipUntilNextEpoch,
  fillUntilNextEpoch,
  fillEpochs,
  fillEpochPercentage,
  rollbackTo,
  registerAllStakeCreds,
  registerDRepsAndDelegateVotes,
  registerCommitteeCreds,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Mock.ChainSync.Server
import Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Conway.Scenarios as Conway
import Cardano.Mock.Forging.Types
import Cardano.Slotting.Slot (SlotNo (..))
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (replicateM)
import Data.Word (Word64)
import Ouroboros.Consensus.Cardano.Block (
  BabbageEra,
  ConwayEra,
  ShelleyEra,
 )
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

forgeNextAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> MockBlock -> IO CardanoBlock
forgeNextAndSubmit inter mockServer testBlock = do
  blk <- forgeNext inter testBlock
  atomically $ addBlock mockServer blk
  pure blk

forgeNextFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> [TxEra] -> IO CardanoBlock
forgeNextFindLeaderAndSubmit interpreter mockServer txs' = do
  blk <- forgeNextFindLeader interpreter txs'
  atomically $ addBlock mockServer blk
  pure blk

forgeNextSkipSlotsFindLeaderAndSubmit :: Interpreter -> ServerHandle IO CardanoBlock -> Word64 -> [TxEra] -> IO CardanoBlock
forgeNextSkipSlotsFindLeaderAndSubmit interpreter mockServer skipSlots txs' = do
  blk <- forgeNextAfter interpreter skipSlots txs'
  atomically $ addBlock mockServer blk
  pure blk

forgeAndSubmitBlocks :: Interpreter -> ServerHandle IO CardanoBlock -> Int -> IO [CardanoBlock]
forgeAndSubmitBlocks interpreter mockServer blocksToCreate =
  replicateM blocksToCreate (forgeNextFindLeaderAndSubmit interpreter mockServer [])

withAlonzoFindLeaderAndSubmit ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  ( LedgerState (ShelleyBlock TPraosStandard AlonzoEra) ->
    Either ForgingError [Core.Tx AlonzoEra]
  ) ->
  IO CardanoBlock
withAlonzoFindLeaderAndSubmit interpreter mockServer mkTxs = do
  alTxs <- withAlonzoLedgerState interpreter mkTxs
  forgeNextFindLeaderAndSubmit interpreter mockServer (TxAlonzo <$> alTxs)

withBabbageFindLeaderAndSubmit ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  (LedgerState (ShelleyBlock PraosStandard BabbageEra) -> Either ForgingError [Core.Tx BabbageEra]) ->
  IO CardanoBlock
withBabbageFindLeaderAndSubmit interpreter mockServer mkTxs = do
  alTxs <- withBabbageLedgerState interpreter mkTxs
  forgeNextFindLeaderAndSubmit interpreter mockServer (TxBabbage <$> alTxs)

withConwayFindLeaderAndSubmit ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  (LedgerState (ShelleyBlock PraosStandard ConwayEra) -> Either ForgingError [Core.Tx ConwayEra]) ->
  IO CardanoBlock
withConwayFindLeaderAndSubmit interpreter mockServer mkTxs = do
  txs' <- withConwayLedgerState interpreter mkTxs
  forgeNextFindLeaderAndSubmit interpreter mockServer (TxConway <$> txs')

withAlonzoFindLeaderAndSubmitTx ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  ( LedgerState (ShelleyBlock TPraosStandard AlonzoEra) ->
    Either ForgingError (Core.Tx AlonzoEra)
  ) ->
  IO CardanoBlock
withAlonzoFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
  withAlonzoFindLeaderAndSubmit interpreter mockServer $ \st -> do
    tx <- mkTxs st
    pure [tx]

withBabbageFindLeaderAndSubmitTx ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  (LedgerState (ShelleyBlock PraosStandard BabbageEra) -> Either ForgingError (Core.Tx BabbageEra)) ->
  IO CardanoBlock
withBabbageFindLeaderAndSubmitTx interpreter mockServer mkTxs = do
  withBabbageFindLeaderAndSubmit interpreter mockServer $ \st -> do
    tx <- mkTxs st
    pure [tx]

withConwayFindLeaderAndSubmitTx ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  (LedgerState (ShelleyBlock PraosStandard ConwayEra) -> Either ForgingError (Core.Tx ConwayEra)) ->
  IO CardanoBlock
withConwayFindLeaderAndSubmitTx interpreter mockServer mkTx =
  withConwayFindLeaderAndSubmit interpreter mockServer $ \st -> do
    tx <- mkTx st
    pure [tx]

withShelleyFindLeaderAndSubmit ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  ( LedgerState (ShelleyBlock TPraosStandard ShelleyEra) ->
    Either ForgingError [Core.Tx ShelleyEra]
  ) ->
  IO CardanoBlock
withShelleyFindLeaderAndSubmit interpreter mockServer mkTxs = do
  alTxs <- withShelleyLedgerState interpreter mkTxs
  forgeNextFindLeaderAndSubmit interpreter mockServer (TxShelley <$> alTxs)

withShelleyFindLeaderAndSubmitTx ::
  Interpreter ->
  ServerHandle IO CardanoBlock ->
  ( LedgerState (ShelleyBlock TPraosStandard ShelleyEra) ->
    Either ForgingError (Core.Tx ShelleyEra)
  ) ->
  IO CardanoBlock
withShelleyFindLeaderAndSubmitTx interpreter mockServer mkTxs =
  withShelleyFindLeaderAndSubmit interpreter mockServer $ \st -> do
    tx <- mkTxs st
    pure [tx]

getShelleyLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock TPraosStandard ShelleyEra))
getShelleyLedgerState interpreter = withShelleyLedgerState interpreter Right

getAlonzoLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock TPraosStandard AlonzoEra))
getAlonzoLedgerState interpreter = withAlonzoLedgerState interpreter Right

getBabbageLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock PraosStandard BabbageEra))
getBabbageLedgerState interpreter = withBabbageLedgerState interpreter Right

getConwayLedgerState :: Interpreter -> IO (LedgerState (ShelleyBlock PraosStandard ConwayEra))
getConwayLedgerState interpreter = withConwayLedgerState interpreter Right

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
  replicateM blocksToCreate $ forgeNextFindLeaderAndSubmit interpreter mockServer []

rollbackTo :: Interpreter -> ServerHandle IO CardanoBlock -> CardanoPoint -> IO ()
rollbackTo interpreter mockServer point = do
  rollbackInterpreter interpreter point
  atomically $ rollback mockServer point

registerAllStakeCreds :: Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
registerAllStakeCreds interpreter mockServer = do
  blk <- forgeWithStakeCreds interpreter
  atomically $ addBlock mockServer blk
  pure blk

registerDRepsAndDelegateVotes :: Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
registerDRepsAndDelegateVotes interpreter mockServer = do
  blk <- Conway.registerDRepsAndDelegateVotes interpreter
  atomically (addBlock mockServer blk)
  pure blk

registerCommitteeCreds :: Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
registerCommitteeCreds interpreter mockServer = do
  blk <- Conway.registerCommitteeCreds interpreter
  atomically (addBlock mockServer blk)
  pure blk

-- Expected number. This should be taken from the parameters, instead of hardcoded.
blocksPerEpoch :: Int
blocksPerEpoch = 100
