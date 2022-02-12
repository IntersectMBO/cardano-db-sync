module Cardano.Db.Bench where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Class.MonadSTM.Strict
import qualified Data.Text.Encoding as Text
import           Data.List.Split
import qualified Data.Map as Map
import           Data.Text (Text)

import           Ouroboros.Network.Block (Point (..))

import           Cardano.Slotting.Slot

import           Cardano.Ledger.Address
import           Cardano.Ledger.BaseTypes
import           Cardano.Ledger.Credential
import           Cardano.Ledger.Mary.Value
import           Cardano.Ledger.Shelley.TxBody

import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Db.Config hiding (withFullConfig)
import qualified Cardano.Mock.Db.Config as Config
import           Cardano.Mock.Db.Validate
import           Cardano.Mock.Forging.Interpreter
import qualified Cardano.Mock.Forging.Tx.Alonzo as Alonzo
import           Cardano.Mock.Forging.Tx.Generic
import           Cardano.Mock.Forging.Types

import           Criterion

benchmark :: IOManager -> [(Text, Text)] -> Benchmark
benchmark iom knownMigrations =
    bgroup "bench"
      [ bgroup "empty blocks"
          [ bnch 3 "empty blocks 10" $ emptyBlocks 10
          , bnch 3 "empty blocks 10" $ emptyBlocks 50
          , bnch 3 "empty blocks 10" $ emptyBlocks 100
          , longBnch "empty blocks 10" $ emptyBlocks 500
          ]
      , bgroup "register addresses 1000 per block"
          [ bnch 3 "1 block" $ registerAddressess 1
          , bnch 3 "10 block" $ registerAddressess 10
          , bnch 3 "100 block" $ registerAddressess 100
          , bnch 3 "200 block" $ registerAddressess 200
          ]
      , bgroup "create UTxO. 200 per block"
          [ bnch 3 "1 block" $ createUTXO 1
          , bnch 3 "10 block" $ createUTXO 10
          , longBnch "100 block" $ createUTXO 100
          , longBnch "100 block" $ createUTXO 1000
          ]
      , bgroup "create UTxO. 1000 per block"
          [ bnch 3 "1 block" $ createUTXO' 1
          , bnch 3 "10 block" $ createUTXO' 10
          , longBnch "100 block" $ createUTXO' 100
          , longBnch "1000 block" $ createUTXO' 1000
          ]
      , bgroup "create multiasssets."
          [ bnch 3 "1 block" $ createMaTxOut 1
          , bnch 3 "10 block" $ createMaTxOut 10
          , longBnch "100 block" $ createMaTxOut 100
          ]
      , bgroup "delegate and send funds"
          [ bnch 3 "3 block" $ delegateAndSend 1
          , bnch 3 "30 block" $ delegateAndSend 10
          , longBnch "300 block" $ delegateAndSend 100
          , longBnch "1200 block" $ delegateAndSend 400
          ]
      , bgroup "rollback multiassets"
          [ bnch 3 "1 block" $ rollbackMaTxOut 1
          , bnch 3 "10 block" $ rollbackMaTxOut 10
          , longBnch "100 block" $ rollbackMaTxOut 100
          , longBnch "500 block" $ rollbackMaTxOut 500
          ]
      , bgroup "delegate and send funds"
          [ bnch 3 "3 block" $ delegateAndSend 1
          , bnch 3 "30 block" $ delegateAndSend 10
          , longBnch "300 block" $ createMaTxOut 100
          , longBnch "1200 block" $ delegateAndSend 400
          ]
      ]
  where
    _bnch' :: String -> (IOManager -> [(Text, Text)] -> Benchmarkable) -> Benchmark
    _bnch' str action = bench str (action iom knownMigrations)

    bnch :: Int -> String  -> (IOManager -> [(Text, Text)] -> Benchmarkable) -> Benchmark
    bnch n str action = bench str (fixIterations n $ action iom knownMigrations)

    longBnch :: String -> (IOManager -> [(Text, Text)] -> Benchmarkable) -> Benchmark
    longBnch str = bnch 1 str

data BenchEnv = BenchEnv Interpreter (ServerHandle IO CardanoBlock) DBSyncEnv [CardanoBlock]

instance NFData BenchEnv where
    -- We don't really use many feautures of criterion. 'NFData' is not one of them.
    rnf _ = ()

defaultConfigDir ::  FilePath
defaultConfigDir = "config"

rootTestDir :: FilePath
rootTestDir = "bench/benchfiles"

withFullConfig :: FilePath -> FilePath
               -> (Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> IO ())
               -> IOManager -> [(Text, Text)] -> IO ()
withFullConfig = Config.withFullConfig rootTestDir

benchmarkSyncing :: FilePath -> FilePath -> FilePath
                 -> (Interpreter -> IO [CardanoBlock])
                 -> IOManager -> [(Text, Text)]
                 -> Benchmarkable
benchmarkSyncing rootDir config testLabel mkBlocks iom mig =
    perRunEnvWithCleanup createEnv cleanupEnv runBench
  where
    createEnv :: IO BenchEnv
    createEnv = do
      (interpreter, mockServer, dbSync) <- mkFullConfig rootDir config testLabel iom mig
      -- first block server and then start db-sync during env creation, so that
      -- schema migrations doesn't affect benchmarking results.\
      atomically $ blockServing mockServer
      startDBSync dbSync
      blks <- mkBlocks interpreter
      _ <- forM blks $ atomically . addBlock mockServer
      -- This is here to wait for all migration to run before running the benchmark
      assertBlocksCount dbSync 2
      pure $ BenchEnv interpreter mockServer dbSync blks

    cleanupEnv (BenchEnv interpreter mockServer dbSync _blks) = do
      cleanFullConfig (interpreter, mockServer, dbSync)

    runBench (BenchEnv _interpreter mockServer dbSync blks) = do
      -- unblock the server and wait for the blocks in db.
      atomically $ unBlockServing mockServer
      assertBlockNo dbSync (Just $ length blks - 1) [1,1..]


benchmarkRollback :: FilePath -> FilePath -> FilePath
                  -> (Interpreter -> IO [CardanoBlock])
                  -> IOManager -> [(Text, Text)]
                  -> Benchmarkable
benchmarkRollback rootDir config testLabel mkBlocks iom mig =
    perRunEnvWithCleanup createEnv cleanupEnv runBench
  where
    createEnv :: IO BenchEnv
    createEnv = do
      (interpreter, mockServer, dbSync) <- mkFullConfig rootDir config testLabel iom mig
      startDBSync dbSync
      blks <- mkBlocks interpreter
      _ <- forM blks $ atomically . addBlock mockServer
      -- Sync all blocks in db-sync
      assertBlockNoBackoff dbSync (length blks - 1)
      pure $ BenchEnv interpreter mockServer dbSync blks

    cleanupEnv (BenchEnv interpreter mockServer dbSync _blks) = do
      cleanFullConfig (interpreter, mockServer, dbSync)

    runBench (BenchEnv _interpreter mockServer dbSync _blks) = do
      -- unblock the server and wait for the blocks in db.
      atomically $ rollback mockServer (Point Origin)
      assertBlockNo dbSync Nothing [1,1..]


emptyBlocks :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
emptyBlocks n =
    benchmarkSyncing rootTestDir defaultConfigDir testLabel $ \interpreter ->
      replicateM n $ forgeNextFindLeader interpreter []
  where
    testLabel = "emptyBlock_" <> show n

registerAddressess :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
registerAddressess n =
    benchmarkSyncing rootTestDir defaultConfigDir testLabel $
      registerAddressesBlocks n
  where
    testLabel = "registerAddressess_" <> show n

registerAddressesBlocks :: Int -> Interpreter -> IO [CardanoBlock]
registerAddressesBlocks n interpreter = do
    forM (chunksOf 1000 creds) $ \blockCreds -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \_st ->
        forM (chunksOf 10 blockCreds) $ \txCreds -> -- 10 per tx
          Alonzo.mkDCertTx (fmap (DCertDeleg . RegKey) txCreds) (Wdrl mempty)
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
  where
    creds = createStakeCredentials (1000 * n)

createUTXO :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
createUTXO n =
    benchmarkSyncing rootTestDir defaultConfigDir testLabel $
      createUTXOBlocks n
  where
    testLabel = "createUTXO_" <> show n

-- 200 txs per block. 1 outputs per tx
createUTXOBlocks :: Int -> Interpreter -> IO [CardanoBlock]
createUTXOBlocks n interpreter = do
    addr <- withAlonzoLedgerState interpreter $ resolveAddress (UTxOIndex 0)
    -- we use the change output to create the next transaction.
    let utxoIndex = UTxOAddress addr
    forM (chunksOf 200 addresses) $ \blockAddresses -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM blockAddresses $ \sendAddr ->
          Alonzo.mkPaymentTx utxoIndex (UTxOAddress sendAddr) 1 0 st
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
  where
    addresses = fmap (\addr -> Addr Testnet addr StakeRefNull) (createPaymentCredentials (200 * n))

createUTXO' :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
createUTXO' n =
    benchmarkSyncing rootTestDir defaultConfigDir testLabel $
      createUTXOBlocks' n
  where
    testLabel = "createUTXO'_" <> show n

-- 100 txs per block. 10 outputs per tx
createUTXOBlocks' :: Int -> Interpreter -> IO [CardanoBlock]
createUTXOBlocks' n interpreter = do
    addrFrom <- withAlonzoLedgerState interpreter $ resolveAddress (UTxOIndex 0)
    -- we use the change output to create the next transaction.
    let utxoIndex = UTxOAddress addrFrom
    forM (chunksOf 1000 addresses) $ \blockAddresses -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (chunksOf 10 blockAddresses) $ \txAddresses ->
          Alonzo.mkPaymentTx' utxoIndex (fmap (\addr -> (UTxOAddress addr, Value 1 mempty)) txAddresses) st
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
  where
    addresses = fmap (\addr -> Addr Testnet addr StakeRefNull) (createPaymentCredentials (1000 * n))

createMaTxOut :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
createMaTxOut n =
    benchmarkSyncing rootTestDir defaultConfigDir testLabel $
      createMaTxOutBlocks n
  where
    testLabel = "createMaTxOut_" <> show n

rollbackMaTxOut :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
rollbackMaTxOut n =
    benchmarkRollback rootTestDir defaultConfigDir testLabel $
      createMaTxOutBlocks n
  where
    testLabel = "rollbackMaTxOut_" <> show n

createMaTxOutBlocks :: Int -> Interpreter -> IO [CardanoBlock]
createMaTxOutBlocks n interpreter = do
    addrFrom <- withAlonzoLedgerState interpreter $ resolveAddress (UTxOIndex 0)
    -- we use the change output to create the next transaction.
    let utxoIndex = UTxOAddress addrFrom
    forM (zip [1..n] $ chunksOf 1000 addresses) $ \(_blockId, blockAddresses) -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (zip [1..100] $ chunksOf 10 blockAddresses) $ \(txId, txAddresses) ->
          let maMap = Map.fromList $ flip fmap [0..9] $ \maIndex ->
                let assets = Map.fromList $ flip fmap [0..9] $ \assetIx ->
                        (AssetName $ Text.encodeUtf8 $ textShow (100 * assetIx + maIndex), 1)
                in (PolicyID (mkDummyScriptHash $ 10 * maIndex + txId `mod` 10), assets)
          in Alonzo.mkPaymentTx' utxoIndex (fmap (\addr -> (UTxOAddress addr, Value 1 maMap)) txAddresses) st
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
  where
    addresses = fmap (\addr -> Addr Testnet addr StakeRefNull) (createPaymentCredentials (1000 * n))

delegateAndSend :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
delegateAndSend n =
  benchmarkSyncing rootTestDir defaultConfigDir testLabel $
    delegateAndSendBlocks n
  where
    testLabel = "delegateAndSend_" <> show n

rollbackDelegateAndSend :: Int -> IOManager -> [(Text, Text)] -> Benchmarkable
rollbackDelegateAndSend n =
  benchmarkRollback rootTestDir defaultConfigDir testLabel $
    delegateAndSendBlocks n
  where
    testLabel = "rollbackDelegateAndSend_" <> show n

delegateAndSendBlocks :: Int -> Interpreter -> IO [CardanoBlock]
delegateAndSendBlocks n interpreter = do
    addrFrom <- withAlonzoLedgerState interpreter $ resolveAddress (UTxOIndex 0)
    registerBlocks <- forM (chunksOf 1000 creds) $ \blockCreds -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \_st ->
        forM (chunksOf 10 blockCreds) $ \txCreds -> -- 10 per tx
          Alonzo.mkDCertTx (fmap (DCertDeleg . RegKey) txCreds) (Wdrl mempty)
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)

    delegateBlocks <- forM (chunksOf 1000 creds) $ \blockCreds -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (chunksOf 10 blockCreds) $ \txCreds -> --do -- 10 per tx
          Alonzo.mkDCertTx
            (fmap (\ (poolIx, cred) -> DCertDeleg $ Delegate $ Delegation cred (resolvePool (PoolIndex poolIx) st))
                  (zip (cycle [0,1,2]) txCreds))
            (Wdrl mempty)
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)

    let utxoIndex = UTxOAddress addrFrom
    sendBlocks <- forM (chunksOf 1000 addresses) $ \blockAddresses -> do
      blockTxs <- withAlonzoLedgerState interpreter $ \st ->
        forM (chunksOf 10 blockAddresses) $ \txAddresses ->
          Alonzo.mkPaymentTx' utxoIndex (fmap (\addr -> (UTxOAddress addr, Value 1 mempty)) txAddresses) st
      forgeNextFindLeader interpreter (TxAlonzo <$> blockTxs)
    pure $ registerBlocks <> delegateBlocks <> sendBlocks
  where
    creds = createStakeCredentials (1000 * n)
    pcreds = createPaymentCredentials (1000 * n)
    addresses = fmap (\(pcred, cred) -> Addr Testnet pcred (StakeRefBase cred)) (zip pcreds creds)
