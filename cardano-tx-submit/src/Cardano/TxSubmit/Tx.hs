{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.TxSubmit.Tx
  ( TxSubmitVar (..)
  , newTxSubmitVar
  , readTxSubmit
  , writeTxSubmitResponse
  , submitTx
  ) where

import           Cardano.Prelude hiding (atomically)

import           Control.Monad.Class.MonadSTM.Strict (StrictTMVar,
                    atomically, newEmptyTMVarM, putTMVar, readTMVar)

import           Ouroboros.Consensus.Ledger.Byron (ByronBlock (..), GenTx)

-- The type of 'reject' (determined by ouroboros-network) is currently 'Maybe String'.
-- Hopefully that will be fixed to make it a concrete type.
-- See: https://github.com/input-output-hk/ouroboros-network/issues/1335
data TxSubmitVar reject = TxSubmitVar
  { txSubmit :: !(StrictTMVar IO (GenTx ByronBlock))
  , txRespond :: !(StrictTMVar IO reject)
  }

newTxSubmitVar :: IO (TxSubmitVar reject)
newTxSubmitVar =
  TxSubmitVar <$> newEmptyTMVarM <*> newEmptyTMVarM

-- | Read a previously submitted tx from the TMVar.
readTxSubmit :: TxSubmitVar reject -> IO (GenTx ByronBlock)
readTxSubmit tsv =
  atomically $ readTMVar (txSubmit tsv)

-- | Write the response recieved when tx has been submitted.
writeTxSubmitResponse :: TxSubmitVar reject -> reject -> IO ()
writeTxSubmitResponse tsv r =
  atomically $ putTMVar (txRespond tsv) r

-- | Submit a tx and wait for the response. This is done as a pair of atomic
-- operations, to allow the tx to be read in one operation, submmited and then
-- the response written as a second operation. Doing this as a single atmomic
-- operation would not work as the other end of the submit/response pair need
-- to be operated on independently.
submitTx :: TxSubmitVar reject -> GenTx ByronBlock -> IO reject
submitTx tsv tx = do
  atomically $ putTMVar (txSubmit tsv) tx
  atomically $ readTMVar (txRespond tsv)
