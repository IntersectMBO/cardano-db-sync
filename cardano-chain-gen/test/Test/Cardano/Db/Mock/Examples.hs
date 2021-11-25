module Test.Cardano.Db.Mock.Examples where

import           Cardano.Ledger.Alonzo.Tx
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Shelley.TxBody hiding (TxBody)
import           Cardano.Ledger.ShelleyMA.Timelocks

import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra, StandardCrypto)

import           Cardano.Mock.Forging.Interpreter

mockBlock0 :: MockBlock
mockBlock0 = MockBlock
  { txs = []
  , node = NodeId 0
  }

mockBlock1 :: MockBlock
mockBlock1 = MockBlock
  { txs = []
  , node = NodeId 1
  }

mockBlock2 :: MockBlock
mockBlock2 = MockBlock
  { txs = []
  , node = NodeId 2
  }

mockBlock3 :: MockBlock
mockBlock3 = MockBlock
  { txs = [emptyTx]
  , node = NodeId 0
  }

emptyTx :: ValidatedTx (AlonzoEra StandardCrypto)
emptyTx = ValidatedTx
  { body = emptyTxBody
  , wits = mempty
  , isValid = IsValid True
  , auxiliaryData = maybeToStrictMaybe Nothing
  }

emptyTxBody :: TxBody (AlonzoEra StandardCrypto)
emptyTxBody = TxBody
  mempty
  mempty
  mempty
  mempty
  (Wdrl mempty)
  (Coin 0)
  (ValidityInterval (maybeToStrictMaybe Nothing) (maybeToStrictMaybe Nothing))
  (maybeToStrictMaybe Nothing)
  mempty
  mempty
  (maybeToStrictMaybe Nothing)
  (maybeToStrictMaybe Nothing)
  (maybeToStrictMaybe $ Just $ Testnet)


