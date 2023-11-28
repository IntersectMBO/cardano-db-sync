{-# LANGUAGE TypeFamilies #-}

module Cardano.Mock.Forging.Tx.Conway.Scenarios (
  delegateAndSendBlocks,
) where

import Cardano.Ledger.Address (Addr (..), Withdrawals (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Core (Tx ())
import Cardano.Ledger.Credential (StakeCredential (), StakeReference (..))
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Tx.Generic
import Cardano.Mock.Forging.Types
import Cardano.Prelude
import Data.List.Extra (chunksOf)
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Cardano.Block (LedgerState (..))
import Ouroboros.Consensus.Shelley.Eras (StandardConway ())
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock ())
import Prelude ()

newtype ShelleyLedgerState era = ShelleyLedgerState
  {unState :: LedgerState (ShelleyBlock PraosStandard era)}

delegateAndSendBlocks :: Int -> Interpreter -> IO [CardanoBlock]
delegateAndSendBlocks n interpreter = do
  addrFrom <- withConwayLedgerState interpreter (resolveAddress $ UTxOIndex 0)

  registerBlocks <- mkRegisterBlocks stakeCreds interpreter
  delegateBlocks <- mkDelegateBlocks stakeCreds interpreter
  sendBlocks <- mkPaymentBlocks (UTxOAddress addrFrom) addresses interpreter

  pure (registerBlocks <> delegateBlocks <> sendBlocks)
  where
    stakeCreds = createStakeCredentials n
    payCreds = createPaymentCredentials n
    addresses =
      map
        (\(payCred, stakeCred) -> Addr Testnet payCred (StakeRefBase stakeCred))
        (zip payCreds stakeCreds)

mkRegisterBlocks :: [StakeCredential StandardCrypto] -> Interpreter -> IO [CardanoBlock]
mkRegisterBlocks creds interpreter = forgeBlocksChunked interpreter creds $ \txCreds _ ->
  Conway.mkDCertTx
    (Conway.mkRegTxCert SNothing <$> txCreds)
    (Withdrawals mempty)
    Nothing

mkDelegateBlocks :: [StakeCredential StandardCrypto] -> Interpreter -> IO [CardanoBlock]
mkDelegateBlocks creds interpreter = forgeBlocksChunked interpreter creds $ \txCreds state' ->
  Conway.mkDCertTx
    (map (mkDelegCert state') $ zip (cycle [0, 1, 2]) txCreds)
    (Withdrawals mempty)
    Nothing
  where
    mkDelegCert state' (poolIx, cred) =
      Conway.mkDelegTxCert
        (DelegStake $ resolvePool (PoolIndex poolIx) (unState state'))
        cred

mkPaymentBlocks :: UTxOIndex StandardConway -> [Addr StandardCrypto] -> Interpreter -> IO [CardanoBlock]
mkPaymentBlocks utxoIx addresses interpreter =
  forgeBlocksChunked interpreter addresses $ \txAddrs ->
    Conway.mkPaymentTx' utxoIx (map mkUTxOAddress txAddrs) 0 . unState
  where
    mkUTxOAddress addr = (UTxOAddress addr, MaryValue (Coin 1) mempty)

-- | Forge blocks in chunks of 500 txs
forgeBlocksChunked ::
  Interpreter ->
  [a] ->
  ([a] -> ShelleyLedgerState StandardConway -> Either ForgingError (Tx StandardConway)) ->
  IO [CardanoBlock]
forgeBlocksChunked interpreter vs f = forM (chunksOf 500 vs) $ \blockCreds -> do
  blockTxs <- withConwayLedgerState interpreter $ \state' ->
    forM (chunksOf 10 blockCreds) $ \txCreds ->
      f txCreds (ShelleyLedgerState state')
  forgeNextFindLeader interpreter (TxConway <$> blockTxs)
