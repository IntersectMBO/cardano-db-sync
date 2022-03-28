module Cardano.Mock.Forging.Examples where

import           Cardano.Prelude hiding (length, (.))

import           Data.List.Extra

import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley.API
import           Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

delegateAndSendBlocks :: Int -> Interpreter -> IO [CardanoBlock]
delegateAndSendBlocks n interpreter = do
    addrFrom <- withAlonzoLedgerState interpreter $ resolveAddress (UTxOIndex 0)
    registerBlocks <- forM (chunksOf 500 creds) $ \blockCreds -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \_st ->
        forM (chunksOf 10 blockCreds) $ \txCreds -> -- 10 per tx
          Alonzo.mkDCertTx (fmap (DCertDeleg . RegKey) txCreds) (Wdrl mempty)
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)

    delegateBlocks <- forM (chunksOf 500 creds) $ \blockCreds -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (chunksOf 10 blockCreds) $ \txCreds -> --do -- 10 per tx
          Alonzo.mkDCertTx
            (fmap (\ (poolIx, cred) -> DCertDeleg $ Delegate $ Delegation cred (resolvePool (PoolIndex poolIx) st))
                  (zip (cycle [0,1,2]) txCreds))
            (Wdrl mempty)
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)

    let utxoIndex = UTxOAddress addrFrom
    sendBlocks <- forM (chunksOf 500 addresses) $ \blockAddresses -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (chunksOf 10 blockAddresses) $ \txAddresses ->
          Alonzo.mkPaymentTx' utxoIndex (fmap (\addr -> (UTxOAddress addr, Value 1 mempty)) txAddresses) st
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
    pure $ registerBlocks <> delegateBlocks <> sendBlocks
  where
    creds = createStakeCredentials n
    pcreds = createPaymentCredentials n
    addresses = fmap (\(pcred, cred) -> Addr Testnet pcred (StakeRefBase cred)) (zip pcreds creds)
