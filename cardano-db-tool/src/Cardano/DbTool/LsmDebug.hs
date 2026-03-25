module Cardano.DbTool.LsmDebug
  ( LsmDebugParams (..)
  , runLsmDebug
  ) where

import           Cardano.DbSync.Config
import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Types (LedgerBackend (..), LedgerStateDir (..))
import           Cardano.DbSync.Error
import           Cardano.DbSync.Ledger.State (listLedgerStateFilesOrdered, loadLedgerStateFromFile,
                   mkHandleFromValues)
import           Cardano.DbSync.Ledger.Types (CardanoLedgerState (..), LedgerStateFile (..))

import           Cardano.Crypto.Hash.Class (hashFromTextAsHex)
import           Cardano.Ledger.BaseTypes (TxIx (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import           Cardano.Ledger.TxIn (TxId (..), TxIn (..))

import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.ResourceRegistry (withRegistry)
import           Control.Tracer (nullTracer)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Proxy (Proxy (..))
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Data.Word (Word16)
import qualified Database.LSMTree as LSM
import           Numeric (showHex)
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Vector.Primitive as VP

import           Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState)
import           Ouroboros.Consensus.Ledger.Tables.Utils (emptyLedgerTables)
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LSM
                   (SomeHasFSAndBlockIO (..), TxInBytes (..), TxOutBytes (..),
                    fromTxOutBytes, stdMkBlockIOFS, toTxInBytes)

import           System.FilePath (dropExtension, takeFileName, (</>))
import           System.FS.API (mkFsPath)

import           Prelude

data LsmDebugParams = LsmDebugParams
  { ldpConfigFile     :: !ConfigFile
  , ldpLedgerStateDir :: !LedgerStateDir
  , ldpTxHash         :: !Text
  , ldpTxIndex        :: !Word16
  , ldpSnapshot       :: !(Maybe Text)
  }

runLsmDebug :: LsmDebugParams -> IO ()
runLsmDebug params = do
  enc    <- readSyncNodeConfig (ldpConfigFile params)
  genCfg <- runOrThrowIO $ runExceptT $ readCardanoGenesisConfig enc
  let cfg    = mkTopLevelConfig genCfg
      lsmDir = unLedgerStateDir (ldpLedgerStateDir params) </> "lsm"
  ledgerFiles <- listLedgerStateFilesOrdered (ldpLedgerStateDir params)
  case ledgerFiles of
    [] -> putStrLn "No ledger state files found"
    (lf : _) -> do
      putStrLn $ "Using ledger state file: " <> lsfFilePath lf
      -- Load era context from the state file (empty InMemory handle for era context only)
      eState <- loadLedgerStateFromFile
        (\_ _ -> mkHandleFromValues emptyLedgerTables)
        Nothing
        (LedgerBackendLSM Nothing)
        nullTracer cfg False
        (lsfOriginPoint lf)
        lf
      case eState of
        Left err -> putStrLn $ "Failed to load ledger state: " <> Text.unpack err
        Right cls -> do
          let snapshotName = case ldpSnapshot params of
                Just s  -> Text.unpack s
                Nothing -> dropExtension $ takeFileName $ lsfFilePath lf
          lookupEntry lsmDir snapshotName (clsState cls) params

lookupEntry
  :: FilePath
  -> String
  -> ExtLedgerState CardanoBlock EmptyMK
  -> LsmDebugParams
  -> IO ()
lookupEntry lsmDir snapshotName st params = do
  txIn <- parseTxIn (ldpTxHash params) (ldpTxIndex params)
  let key = toTxInBytes (Proxy @(ExtLedgerState CardanoBlock)) txIn
  putStrLn $ "Looking up TxIn in snapshot: " <> snapshotName
  withRegistry $ \reg -> do
    (_, SomeHasFSAndBlockIO hasFS blockIO) <- stdMkBlockIOFS lsmDir reg
    (_, session) <- allocate reg
      (\_ -> LSM.openSession nullTracer hasFS blockIO 0 (mkFsPath []))
      LSM.closeSession
    (_, table) <- allocate reg
      (\_ -> LSM.openTableFromSnapshot
               session
               (fromString snapshotName)
               (LSM.SnapshotLabel "UTxO table"))
      LSM.closeTable
    results <- LSM.lookups table (V.singleton key)
    case V.head results of
      LSM.NotFound ->
        putStrLn "TxIn not found in this snapshot"
      LSM.Found txOutBytes -> do
        putStrLn $ "Found! Raw bytes (hex): " <> toHex (toBS txOutBytes)
        result <- try (evaluate (fromTxOutBytes st key txOutBytes))
          :: IO (Either SomeException (TxOut (ExtLedgerState CardanoBlock)))
        case result of
          Left  err -> putStrLn $ "Decode FAILED: " <> show err
          Right _   -> putStrLn   "Decode OK"
      LSM.FoundWithBlob{} ->
        putStrLn "Unexpected blob entry"

parseTxIn :: Text -> Word16 -> IO (TxIn StandardCrypto)
parseTxIn hashHex idx =
  case hashFromTextAsHex (Text.unpack hashHex) of
    Nothing -> fail $ "Invalid tx hash: " <> Text.unpack hashHex
    Just h  -> pure $ TxIn (TxId (unsafeMakeSafeHash h)) (TxIx (fromIntegral idx))

toBS :: TxOutBytes -> ByteString
toBS (TxOutBytes (LSM.RawBytes (VP.Vector off len barr))) =
  BS.pack [PBA.indexByteArray barr (off + i) | i <- [0 .. len - 1]]

toHex :: ByteString -> String
toHex = BS.foldr' (\b acc -> pad (showHex b "") <> acc) ""
  where
    pad s = replicate (2 - length s) '0' <> s
