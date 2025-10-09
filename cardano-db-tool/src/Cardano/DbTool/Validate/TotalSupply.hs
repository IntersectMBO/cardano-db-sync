{-# LANGUAGE StrictData #-}

module Cardano.DbTool.Validate.TotalSupply (
  validateTotalSupplyDecreasing,
) where

import qualified Cardano.Db as DB
import Cardano.DbTool.UtxoSet (utxoSetSum)
import Cardano.DbTool.Validate.Util
import Data.Word (Word64)
import System.Random (randomRIO)

data Accounting = Accounting
  { accFees :: DB.Ada
  , accDeposit :: DB.Ada
  , accWithdrawals :: DB.Ada
  , accSupply :: DB.Ada
  }

data TestParams = TestParams
  { testBlockNo :: Word64
  , genesisSupply :: DB.Ada
  }

genTestParameters :: DB.TxOutVariantType -> IO TestParams
genTestParameters txOutVariantType = do
  mlatest <- DB.runDbStandaloneSilent DB.queryLatestBlockNo
  case mlatest of
    Nothing -> error "Cardano.DbTool.Validation: Empty database"
    Just latest ->
      TestParams
        <$> randomRIO (1, latest - 1)
        <*> DB.runDbStandaloneSilent (DB.queryGenesisSupply txOutVariantType)

queryInitialSupply :: DB.TxOutVariantType -> Word64 -> IO Accounting
queryInitialSupply txOutVariantType blkNo =
  -- Run all queries in a single transaction.
  DB.runDbStandaloneSilent $
    Accounting
      <$> DB.queryFeesUpToBlockNo blkNo
      <*> DB.queryDepositUpToBlockNo blkNo
      <*> DB.queryWithdrawalsUpToBlockNo blkNo
      <*> fmap2 utxoSetSum (DB.queryUtxoAtBlockId txOutVariantType) (DB.BlockId $ fromIntegral blkNo)

-- | Validate that the total supply is decreasing.
-- This is only true for the Byron error where transaction fees are burnt.
validateTotalSupplyDecreasing :: DB.TxOutVariantType -> IO ()
validateTotalSupplyDecreasing txOutVariantType = do
  test <- genTestParameters txOutVariantType

  putStrF $
    "Total supply + fees + deposit - withdrawals at block "
      ++ show (testBlockNo test)
      ++ " is same as genesis supply: "

  accounting <- queryInitialSupply txOutVariantType (testBlockNo test)

  let total = accSupply accounting + accFees accounting + accDeposit accounting - accWithdrawals accounting

  if genesisSupply test == total
    then putStrLn $ greenText "ok"
    else error $ redText (show (genesisSupply test) ++ " /= " ++ show total)
