{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IO.Cardano.Db.Transaction (
  tests,
) where

import Cardano.Db
import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Word (Word64)
import Test.IO.Cardano.Db.Util
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Transaction"
    [ testCase "An action returning Left is rolled back" rollsBackOnLeft
    , testCase "An action returning Right is committed" commitsOnRight
    , testCase "An action that throws is rolled back" rollsBackOnException
    ]

-- | Insert a block, then finish the way the insert path does: by returning 'Left',
-- with the Postgres transaction still perfectly healthy. Only the runner can roll that
-- back, since the error is a value rather than an exception.
insertBlockThen :: DbM (Either Text ()) -> DbM (Either Text ())
insertBlockThen finish = do
  slid <- insertSlotLeader testSlotLeader
  void $ insertCheckUniqueBlock (mkBlock 0 slid)
  finish

-- | Blocks left in the database once @runner@ has had its turn on an empty table.
blocksAfter :: (DbEnv -> IO ()) -> IO Word64
blocksAfter runner =
  withDbEnvStandalone PGPassDefaultEnv $ \dbEnv -> do
    runDbTransSilent dbEnv RepeatableRead deleteAllBlocks
    runner dbEnv
    runDbTransSilent dbEnv RepeatableRead queryBlockCount

rollsBackOnLeft :: IO ()
rollsBackOnLeft = do
  count <-
    blocksAfter $ \dbEnv ->
      void $
        runDbTransSilentEither dbEnv RepeatableRead $
          insertBlockThen (pure (Left "application error"))
  assertBool
    ("Block count after a Left is " ++ show count ++ ", so the transaction was committed")
    (count == 0)

commitsOnRight :: IO ()
commitsOnRight = do
  count <-
    blocksAfter $ \dbEnv ->
      void $
        runDbTransSilentEither dbEnv RepeatableRead $
          insertBlockThen (pure (Right ()))
  assertBool
    ("Block count after a Right is " ++ show count ++ " but should be 1")
    (count == 1)

rollsBackOnException :: IO ()
rollsBackOnException = do
  count <-
    blocksAfter $ \dbEnv -> do
      let thrower =
            runDbTransSilentEither dbEnv RepeatableRead $
              insertBlockThen (liftIO $ ioError (userError "boom"))
      result <- try thrower :: IO (Either IOException (Either Text ()))
      void (pure result)
  assertBool
    ("Block count after an exception is " ++ show count ++ ", so the transaction was committed")
    (count == 0)
