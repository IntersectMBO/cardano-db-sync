module Cardano.DbTool.Validate.Ledger (
  LedgerValidationParams (..),
  validateLedger,
) where

import qualified Cardano.Db as DB
import Cardano.DbSync.Config
import Cardano.DbSync.Error
import Cardano.DbSync.Ledger.Types (CardanoLedgerState (..))
import Cardano.DbSync.Tracing.ToObjectOrphans ()
import Cardano.DbTool.Validate.Balance (ledgerAddrBalance)
import Cardano.DbTool.Validate.Util
import Cardano.Network.NodeToClient (withIOManager)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (defaultListSnapshots)
import Ouroboros.Network.Block
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import Prelude

data LedgerValidationParams = LedgerValidationParams
  { vpConfigFile :: !ConfigFile
  , vpLedgerStateDir :: !LedgerStateDir
  , vpAddressUtxo :: !Text
  }

validateLedger :: LedgerValidationParams -> DB.TxOutVariantType -> IO ()
validateLedger params _txOutVariantType =
  withIOManager $ \_ -> do
    -- TODO: Reimplement using consensus snapshot loading APIs (openStateRefFromSnapshot).
    _enc <- readSyncNodeConfig (vpConfigFile params)
    _genCfg <- runOrThrowIO (runExceptT (readCardanoGenesisConfig _enc))
    let someHasFS = SomeHasFS (ioHasFS (MountPoint (unLedgerStateDir (vpLedgerStateDir params))))
    snapshots <- defaultListSnapshots someHasFS
    _slotNo <- SlotNo <$> DB.runDbStandaloneSilent DB.queryLatestSlotNo
    putStrLn ("Found " <> show (length snapshots) <> " snapshots. Ledger validation not yet reimplemented for consensus snapshot format.")

-- TODO: Reimplement using DiskSnapshot and consensus APIs
-- _validate :: LedgerValidationParams -> DB.TxOutVariantType -> GenesisConfig -> SlotNo -> [LedgerStateFile] -> IO ()
-- _validate params txOutVariantType genCfg slotNo ledgerFiles =
--    go ledgerFiles True
--    where
--      go :: [LedgerStateFile] -> Bool -> IO ()
--      go [] _ = putStrLn $ redText "No ledger found"
--      go (ledgerFile : rest) logFailure = do
--        let ledgerSlot = lsfSlotNo ledgerFile
--        if ledgerSlot <= slotNo
--          then do
--            Right state <- loadLedgerStateFromFile nullTracer (mkTopLevelConfig genCfg) False GenesisPoint ledgerFile
--            validateBalance txOutVariantType ledgerSlot (vpAddressUtxo params) state
--          else do
--            when logFailure . putStrLn $ redText "Ledger is newer than DB. Trying an older ledger."
--            go rest False

_validateBalance :: DB.TxOutVariantType -> SlotNo -> Text -> CardanoLedgerState -> IO ()
_validateBalance txOutVariantType slotNo addr st = do
  balanceDB <- DB.runDbStandaloneSilent $ DB.queryAddressBalanceAtSlot txOutVariantType addr (unSlotNo slotNo)
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
