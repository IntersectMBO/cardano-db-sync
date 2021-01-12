module Cardano.Db.Tool.Validate.Ledger
  ( LedgerValidationParams (..)
  , validateLedger
  ) where

import           Control.Monad (when)
import           Control.Monad.Trans.Except.Exit (orDie)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Prelude

import           Cardano.Db (TxOut(txOutAddress, txOutValue), queryUtxoAtSlotNo, runDbNoLogging)
import qualified Cardano.Db as DB
import           Cardano.Db.Tool.Validate.Balance (ledgerAddrBalance)
import           Cardano.Db.Tool.Validate.Util
import           Cardano.DbSync.Config
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Tracing.ToObjectOrphans ()
import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Cardano.Node ()
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Network.NodeToClient (withIOManager)

data LedgerValidationParams = LedgerValidationParams
  { vpConfigFile :: !ConfigFile
  , vpLedgerStateDir :: !LedgerStateDir
  , vpAddressUtxo :: Maybe Text
  }

validateLedger :: LedgerValidationParams -> IO ()
validateLedger params = do
  withIOManager $ \ _ -> do
    enc <- readDbSyncNodeConfig (vpConfigFile params)
    ledgerFiles <- listLedgerStateFilesOrdered (vpLedgerStateDir params)
    slotNo <- SlotNo <$> DB.runDbNoLogging DB.queryLatestSlotNo
    genCfg <- orDie renderDbSyncNodeError $ readCardanoGenesisConfig enc
    validate params genCfg slotNo ledgerFiles
  return ()

validate :: LedgerValidationParams -> GenesisConfig -> SlotNo -> [LedgerStateFile] -> IO ()
validate params genCfg slotNo ledgerFiles =
  go ledgerFiles True
    where
      go :: [LedgerStateFile] -> Bool -> IO ()
      go [] _ = putStrLn $ redText "No ledger found"
      go (ledgerFile : rest) logFailure = do
        let ledgerSlot = lsfSlotNo ledgerFile
        if ledgerSlot <= slotNo
        then do
          putStrLn $ concat
            ["Ledger is at ", show ledgerSlot, " and DB is at ", show slotNo,
            ". Comparing the states at ", show ledgerSlot]
          Just state <- getLedgerFromFile genCfg ledgerFile
          case vpAddressUtxo params of
            Nothing -> return ()
            Just addr -> validateBalance ledgerSlot addr state
        else do
          when logFailure $ putStrLn $ redText "Ledger is newer than DB. Trying an older ledger."
          go rest False

validateBalance :: SlotNo -> Text -> CardanoLedgerState -> IO ()
validateBalance slotNo addr st = do
  utxo <- fmap fst <$> runDbNoLogging (queryUtxoAtSlotNo (unSlotNo slotNo))
  -- todo: move more functionality in the db.
  let balanceDB = sum $ DB.unDbLovelace . txOutValue <$> filter ((==addr) . txOutAddress) utxo
  let eiBalanceLedger = ledgerAddrBalance addr $ ledgerState $ clsState st
  case eiBalanceLedger of
    Left str -> putStrLn $ redText $ Text.unpack str
    Right balanceLedger -> if balanceDB == balanceLedger
      then do
        putStrF $ concat
          ["Ledger and DB balance for address ", show addr, " match and is ", show balanceLedger, ": "]
        putStrLn $ greenText "ok"
      else error $ redText $ concat
        ["failed: Ledger and DB balance for address ", show addr, "don't match: ", show balanceDB, " /= ", show balanceLedger]
