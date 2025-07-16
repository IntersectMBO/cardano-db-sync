module Cardano.DbTool.Validate.Ledger (
  LedgerValidationParams (..),
  validateLedger,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config
import Cardano.DbSync.Config.Cardano
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.State
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), LedgerStateFile (..))
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbTool.Validate.Balance (ledgerAddrBalance)
import Cardano.DbTool.Validate.Util
import Control.Monad (when)
import Control.Monad.Trans.Except (runExceptT)
import Control.Tracer (nullTracer)
import Data.Text (Text)
import qualified Data.Text as Text
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Network.Block
import Ouroboros.Network.NodeToClient (withIOManager)
import Prelude

data LedgerValidationParams = LedgerValidationParams
  { vpConfigFile :: !ConfigFile
  , vpLedgerStateDir :: !LedgerStateDir
  , vpAddressUtxo :: !Text
  }

validateLedger :: LedgerValidationParams -> DB.TxOutVariantType -> IO ()
validateLedger params txOutTableType =
  withIOManager $ \_ -> do
    enc <- readSyncNodeConfig (vpConfigFile params)
    genCfg <- runOrThrowIO $ runExceptT $ readCardanoGenesisConfig enc
    ledgerFiles <- listLedgerStateFilesOrdered (vpLedgerStateDir params)
    slotNo <- SlotNo <$> DB.runDbNoLoggingEnv DB.queryLatestSlotNo
    validate params txOutTableType genCfg slotNo ledgerFiles

validate :: LedgerValidationParams -> DB.TxOutVariantType -> GenesisConfig -> SlotNo -> [LedgerStateFile] -> IO ()
validate params txOutTableType genCfg slotNo ledgerFiles =
  go ledgerFiles True
  where
    go :: [LedgerStateFile] -> Bool -> IO ()
    go [] _ = putStrLn $ redText "No ledger found"
    go (ledgerFile : rest) logFailure = do
      let ledgerSlot = lsfSlotNo ledgerFile
      if ledgerSlot <= slotNo
        then do
          -- TODO fix GenesisPoint. This is only used for logging
          Right state <- loadLedgerStateFromFile nullTracer (mkTopLevelConfig genCfg) False GenesisPoint ledgerFile
          validateBalance txOutTableType ledgerSlot (vpAddressUtxo params) state
        else do
          when logFailure . putStrLn $ redText "Ledger is newer than DB. Trying an older ledger."
          go rest False

validateBalance :: DB.TxOutVariantType -> SlotNo -> Text -> CardanoLedgerState -> IO ()
validateBalance txOutTableType slotNo addr st = do
  balanceDB <- DB.runDbNoLoggingEnv $ DB.queryAddressBalanceAtSlot txOutTableType addr (unSlotNo slotNo)
  let eiBalanceLedger = DB.word64ToAda <$> ledgerAddrBalance addr (ledgerState $ clsState st)
  case eiBalanceLedger of
    Left str -> putStrLn $ redText $ show str
    Right balanceLedger ->
      if balanceDB == balanceLedger
        then
          putStrF $
            concat
              [ "DB and Ledger balance for address "
              , Text.unpack addr
              , " at slot "
              , show (unSlotNo slotNo)
              , " match ("
              , show balanceLedger
              , " ada) : "
              , greenText "ok"
              , "\n"
              ]
        else
          error . redText $
            concat
              [ "failed: DB and Ledger balances for address "
              , Text.unpack addr
              , " don't match. "
              , "DB value ("
              , show balanceDB
              , ") /= ledger value ("
              , show balanceLedger
              , ")."
              ]
