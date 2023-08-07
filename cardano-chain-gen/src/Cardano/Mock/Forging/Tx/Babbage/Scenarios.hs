module Cardano.Mock.Forging.Tx.Babbage.Scenarios (
  delegateAndSendBlocks,
) where

import Cardano.Ledger.Mary.Value
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.TxCert
import Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Babbage as Babbage
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude hiding (length, (.))
import Data.List.Extra

delegateAndSendBlocks :: Int -> Interpreter -> IO [CardanoBlock]
delegateAndSendBlocks n interpreter = do
  addrFrom <- withBabbageLedgerState interpreter $ resolveAddress (UTxOIndex 0)
  registerBlocks <- forM (chunksOf 500 creds) $ \blockCreds -> do
    blockTxs <- withBabbageLedgerState interpreter $ \_st ->
      forM (chunksOf 10 blockCreds) $ \txCreds ->
        -- 10 per tx
        Babbage.mkDCertTx (fmap (ShelleyTxCertDelegCert . ShelleyRegCert) txCreds) (Withdrawals mempty) Nothing
    forgeNextFindLeader interpreter (TxBabbage <$> blockTxs)

  delegateBlocks <- forM (chunksOf 500 creds) $ \blockCreds -> do
    blockTxs <- withBabbageLedgerState interpreter $ \st ->
      forM (chunksOf 10 blockCreds) $ \txCreds ->
        -- do -- 10 per tx
        Babbage.mkDCertTx
          ( fmap
              (\(poolIx, cred) -> ShelleyTxCertDelegCert $ ShelleyDelegCert cred (resolvePool (PoolIndex poolIx) st))
              (zip (cycle [0, 1, 2]) txCreds)
          )
          (Withdrawals mempty)
          Nothing
    forgeNextFindLeader interpreter (TxBabbage <$> blockTxs)

  let utxoIndex = UTxOAddress addrFrom
  sendBlocks <- forM (chunksOf 500 addresses) $ \blockAddresses -> do
    blockTxs <- withBabbageLedgerState interpreter $ \st ->
      forM (chunksOf 10 blockAddresses) $ \txAddresses ->
        Babbage.mkPaymentTx' utxoIndex (fmap (\addr -> (UTxOAddress addr, MaryValue 1 mempty)) txAddresses) st
    forgeNextFindLeader interpreter (TxBabbage <$> blockTxs)
  pure $ registerBlocks <> delegateBlocks <> sendBlocks
  where
    creds = createStakeCredentials n
    pcreds = createPaymentCredentials n
    addresses = fmap (\(pcred, cred) -> Addr Testnet pcred (StakeRefBase cred)) (zip pcreds creds)
