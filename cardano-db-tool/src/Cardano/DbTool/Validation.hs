module Cardano.DbTool.Validation (
  LedgerValidationParams (..),
  runDbValidation,
  runLedgerValidation,
) where

import Cardano.Db (TxOutVariantType)
import Cardano.DbTool.Validate.AdaPots (validateSumAdaPots)
import Cardano.DbTool.Validate.BlockProperties (validateBlockProperties)
import Cardano.DbTool.Validate.BlockTxs (validateEpochBlockTxs)
import Cardano.DbTool.Validate.EpochTable (validateEpochTable)
import Cardano.DbTool.Validate.Ledger (LedgerValidationParams (..), validateLedger)
import Cardano.DbTool.Validate.PoolOwner (validateAllPoolsHaveOwners)
import Cardano.DbTool.Validate.TotalSupply (validateTotalSupplyDecreasing)
import Cardano.DbTool.Validate.TxAccounting (validateTxAccounting)
import Cardano.DbTool.Validate.Withdrawal (validateWithdrawals)

runDbValidation :: TxOutVariantType -> IO ()
runDbValidation txOutTableType = do
  fastValidations
  slowValidations txOutTableType

runLedgerValidation :: LedgerValidationParams -> TxOutVariantType -> IO ()
runLedgerValidation =
  validateLedger

-- -------------------------------------------------------------------------------------------------

fastValidations :: IO ()
fastValidations = do
  validateAllPoolsHaveOwners
  validateBlockProperties
  validateSumAdaPots

slowValidations :: TxOutVariantType -> IO ()
slowValidations txOutTableType = do
  validateTxAccounting txOutTableType
  validateWithdrawals
  validateEpochTable
  validateEpochBlockTxs
  validateTotalSupplyDecreasing txOutTableType
