{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Threads.TxInResolve where

import Cardano.BM.Trace (logInfo)
import qualified Cardano.Db as DB
import Cardano.DbSync.Api
import Cardano.DbSync.Api.Types
import Cardano.DbSync.Era.Shelley.Generic.Block
import Cardano.DbSync.Era.Shelley.Generic.Tx.Shelley
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types
import Cardano.DbSync.Era.Universal.Insert.Grouped
import Cardano.DbSync.Types
import Cardano.DbSync.Util
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Prelude hiding (atomically)
import Control.Concurrent.Async
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.Strict.TBQueue as TBQ
import qualified Data.Set as Set
import Database.Persist.Postgresql
import Lens.Micro
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)

-- | This is actually more than one thread.
runTxInResolve :: SyncEnv -> IO ()
runTxInResolve syncEnv = do
  logInfo trce "Running TxIn thread"
  _ <- logException trce "runStakeThread: " loopWithPool
  logInfo trce "Shutting TxIn thread"
  where
    nPool = 5
    trce = getTrace syncEnv
    loopWithPool =
      DB.runIohkLogging trce $
        withPostgresqlPool (envConnectionString syncEnv) nPool (forever . loopAction)

    loopAction pool = do
      action <- liftIO $ atomically $ TBQ.readTBQueue (pTxInQueue (envPrefetch syncEnv))
      case action of
        PrefetchTxIdBlock cblock -> prefetchBlock cblock
        PrefetchTxIdBlocks cblocks -> mapM_ prefetchBlock cblocks
      where
        prefetchBlock cblock =
          liftIO $ mapConcurrently_ resolver (groupByModulo nPool $ concat (getTxIns cblock))

        -- There are nPool connections in pool and nPool threads, so each thread can keep a single
        -- pool for all queries it performs.
        resolver ls = runSqlPoolNoTransaction (queries ls) pool Nothing
        queries = mapM_ $ \(isValid, txIn) ->
          resolveTxInputsPrefetch syncEnv (not isValid) txIn

getTxIns :: CardanoBlock -> [[(Bool, TxInKey)]]
getTxIns cblock = case cblock of
  BlockByron _ -> [] -- Impossible
  BlockShelley blk -> mkTxInsShelley blk
  BlockAllegra blk -> mkTxInsShelley blk
  BlockMary blk -> mkTxInsShelley blk
  BlockAlonzo blk -> mkTxInsAlonzo blk
  BlockBabbage blk -> mkTxInsAlonzo blk
  BlockConway blk -> mkTxInsAlonzo blk

mkTxInsShelley ::
  (Core.EraSegWits era, Core.EraCrypto era ~ StandardCrypto) =>
  ShelleyBlock p era ->
  [[(Bool, TxInKey)]]
mkTxInsShelley blk = map (map ((True,) . txInKey) . mkTxIn . getTxBody . snd) (getTxs blk)
  where
    getTxBody tx = tx ^. Core.bodyTxL

mkTxInsAlonzo ::
  (Core.EraSegWits era, Core.EraCrypto era ~ StandardCrypto, Core.Tx era ~ Alonzo.AlonzoTx era, Alonzo.AlonzoEraTxBody era) =>
  ShelleyBlock p era ->
  [[(Bool, TxInKey)]]
mkTxInsAlonzo blk = map (pairWithAll . getTxInsAlonzo . snd) (getTxs blk)
  where
    pairWithAll (bl, s) = map ((bl,) . toTxInKey) (Set.toList s)
    getTxInsAlonzo tx = case Alonzo.isValid tx of
      Alonzo.IsValid True -> (True, tx ^. (Core.bodyTxL . Core.inputsTxBodyL))
      Alonzo.IsValid False -> (False, tx ^. (Core.bodyTxL . Alonzo.collateralInputsTxBodyL))

groupByModulo :: Int -> [a] -> [[a]]
groupByModulo n xs = [[x | (x, i) <- indexed, i `mod` n == m] | m <- [0 .. n - 1]]
  where
    indexed = zip xs [0 ..]
