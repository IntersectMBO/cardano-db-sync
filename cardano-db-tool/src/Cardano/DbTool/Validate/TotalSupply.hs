{-# LANGUAGE StrictData #-}

module Cardano.DbTool.Validate.TotalSupply (
  validateTotalSupplyDecreasing,
) where

import Cardano.Db
import Cardano.DbTool.UtxoSet (utxoSetSum)
import Cardano.DbTool.Validate.Util
import Data.Word (Word64)
import System.Random (randomRIO)

data Accounting = Accounting
  { accFees :: Ada
  , accDeposit :: Ada
  , accWithdrawals :: Ada
  , accSupply :: Ada
  }

data TestParams = TestParams
  { testBlockNo :: Word64
  , genesisSupply :: Ada
  }

genTestParameters :: TxOutTableType -> IO TestParams
genTestParameters txOutTableType = do
  mlatest <- runDbNoLoggingEnv queryLatestBlockNo
  case mlatest of
    Nothing -> error "Cardano.DbTool.Validation: Empty database"
    Just latest ->
      TestParams
        <$> randomRIO (1, latest - 1)
        <*> runDbNoLoggingEnv (queryGenesisSupply txOutTableType)

queryInitialSupply :: TxOutTableType -> Word64 -> IO Accounting
queryInitialSupply txOutTableType blkNo =
  -- Run all queries in a single transaction.
  runDbNoLoggingEnv $
    Accounting
      <$> queryFeesUpToBlockNo blkNo
      <*> queryDepositUpToBlockNo blkNo
      <*> queryWithdrawalsUpToBlockNo blkNo
      <*> fmap2 utxoSetSum (queryUtxoAtBlockNo txOutTableType) blkNo

-- | Validate that the total supply is decreasing.
-- This is only true for the Byron error where transaction fees are burnt.
validateTotalSupplyDecreasing :: TxOutTableType -> IO ()
validateTotalSupplyDecreasing txOutTableType = do
  test <- genTestParameters txOutTableType

  putStrF $
    "Total supply + fees + deposit - withdrawals at block "
      ++ show (testBlockNo test)
      ++ " is same as genesis supply: "

  accounting <- queryInitialSupply txOutTableType (testBlockNo test)

  let total = accSupply accounting + accFees accounting + accDeposit accounting - accWithdrawals accounting

  if genesisSupply test == total
    then putStrLn $ greenText "ok"
    else error $ redText (show (genesisSupply test) ++ " /= " ++ show total)
