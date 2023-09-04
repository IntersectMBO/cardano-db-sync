{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Error (
  SyncInvariant (..),
  SyncNodeError (..),
  NodeConfigError (..),
  annotateInvariantTx,
  bsBase16Encode,
  dbSyncNodeError,
  dbSyncInvariant,
  renderSyncInvariant,
  runOrThrowIO,
  fromEitherSTM,
  logAndThrowIO,
  shouldAbortOnPanic,
  hasAbortOnPanicEnv,
) where

import Cardano.BM.Trace (Trace, logError)
import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.DbSync.Util
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString.Base16 as Base16
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Environment (lookupEnv)
import System.Posix.Process (exitImmediately)
import qualified Text.Show as Show

data SyncInvariant
  = EInvInOut !Word64 !Word64
  | EInvTxInOut !Byron.Tx !Word64 !Word64

data SyncNodeError
  = SNErrDefault !Text
  | SNErrInvariant !Text !SyncInvariant
  | SNEErrBlockMismatch !Word64 !ByteString !ByteString
  | SNErrIgnoreShelleyInitiation
  | SNErrByronConfig !FilePath !Byron.ConfigurationError
  | SNErrShelleyConfig !FilePath !Text
  | SNErrAlonzoConfig !FilePath !Text
  | SNErrCardanoConfig !Text
  | SNErrInsertGenesis !String
  | SNErrLedgerState !String
  | SNErrNodeConfig NodeConfigError
  | SNErrLocalStateQuery !String
  | SNErrByronGenesis !String
  | SNErrExtraMigration !String
  | SNErrDatabaseRollBackLedger !String
  | SNErrDatabaseValConstLevel !String

instance Exception SyncNodeError

instance Show SyncNodeError where
  show =
    \case
      SNErrDefault t -> "Error SNErrDefault: " <> show t
      SNErrInvariant loc i -> "Error SNErrInvariant: " <> Show.show loc <> ": " <> show (renderSyncInvariant i)
      SNEErrBlockMismatch blkNo hashDb hashBlk ->
        mconcat
          [ "Error SNEErrBlockMismatch: "
          , "Block mismatch for block number "
          , show blkNo
          , ", db has "
          , show $ bsBase16Encode hashDb
          , " but chain provided "
          , show $ bsBase16Encode hashBlk
          ]
      SNErrIgnoreShelleyInitiation ->
        mconcat
          [ "Error SNErrIgnoreShelleyInitiation: "
          , "Node configs that don't fork to Shelley directly and initiate"
          , " funds or stakes in Shelley Genesis are not supported."
          ]
      SNErrByronConfig fp ce ->
        mconcat
          [ "Error SNErrByronConfig: "
          , "Failed reading Byron genesis file "
          , show fp
          , ": "
          , show ce
          ]
      SNErrShelleyConfig fp txt ->
        mconcat
          [ "Error SNErrShelleyConfig: "
          , "Failed reading Shelley genesis file "
          , show fp
          , ": "
          , show txt
          ]
      SNErrAlonzoConfig fp txt ->
        mconcat
          [ "Error SNErrAlonzoConfig: "
          , "Failed reading Alonzo genesis file "
          , show fp
          , ": "
          , show txt
          ]
      SNErrCardanoConfig err ->
        mconcat
          [ "Error SNErrCardanoConfig: "
          , "With Cardano protocol, Byron/Shelley config mismatch:\n"
          , "   "
          , show err
          ]
      SNErrInsertGenesis err -> "Error SNErrInsertGenesis: " <> err
      SNErrLedgerState err -> "Error SNErrLedgerState: " <> err
      SNErrNodeConfig err -> "Error SNErrNodeConfig: " <> show err
      SNErrLocalStateQuery err -> "Error SNErrLocalStateQuery: " <> show err
      SNErrByronGenesis err -> "Error SNErrByronGenesis:" <> show err
      SNErrExtraMigration err -> "Error SNErrExtraMigration: " <> show err
      SNErrDatabaseRollBackLedger err -> "Error SNErrDatabase Rollback Ledger: " <> show err
      SNErrDatabaseValConstLevel err -> "Error SNErrDatabase Validate Consistent Level: " <> show err

data NodeConfigError
  = NodeConfigParseError !String
  | ParseSyncPreConfigError !String
  | ReadByteStringFromFileError !String

instance Exception NodeConfigError

instance Show NodeConfigError where
  show =
    \case
      NodeConfigParseError err -> "NodeConfigParseError - " <> err
      ParseSyncPreConfigError err -> "ParseSyncPreConfigError - " <> err
      ReadByteStringFromFileError err -> "ReadByteStringFromFileError - " <> err

annotateInvariantTx :: Byron.Tx -> SyncInvariant -> SyncInvariant
annotateInvariantTx tx ei =
  case ei of
    EInvInOut inval outval -> EInvTxInOut tx inval outval
    _other -> ei

dbSyncNodeError :: Monad m => Text -> ExceptT SyncNodeError m a
dbSyncNodeError = left . SNErrDefault

dbSyncInvariant :: Monad m => Text -> SyncInvariant -> ExceptT SyncNodeError m a
dbSyncInvariant loc = left . SNErrInvariant loc

renderSyncInvariant :: SyncInvariant -> Text
renderSyncInvariant ei =
  case ei of
    EInvInOut inval outval ->
      mconcat ["input value ", textShow inval, " < output value ", textShow outval]
    EInvTxInOut tx inval outval ->
      mconcat
        [ "tx "
        , bsBase16Encode (Byron.unTxHash $ Crypto.serializeCborHash tx)
        , " : input value "
        , textShow inval
        , " < output value "
        , textShow outval
        , "\n"
        , textShow tx
        ]

fromEitherSTM :: (Exception e) => Either e a -> STM a
fromEitherSTM = either throwSTM return

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ Show.show bs
    Right txt -> txt

runOrThrowIO :: forall e a m. (MonadIO m) => Exception e => m (Either e a) -> m a
runOrThrowIO ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a

logAndThrowIO :: Trace IO Text -> SyncNodeError -> IO ()
logAndThrowIO tracer err = do
  logError tracer $ show err
  throwIO err

-- The network code catches all execptions and retries them, even exceptions generated by the
-- 'error' or 'panic' function. To actually force the termination of 'db-sync' we therefore
-- need a custom panic function that is guaranteed to abort when we want it to.
-- However, we may not want to abort in production, so we make it optional by use of an
-- environment variable.
shouldAbortOnPanic :: Text -> IO ()
shouldAbortOnPanic msg = do
  whenM hasAbortOnPanicEnv $ do
    threadDelay 100000 -- 0.1 seconds
    mapM_ putStrLn ["DbSyncAbortOnPanic: ", msg]
    exitImmediately (ExitFailure 1)

hasAbortOnPanicEnv :: IO Bool
hasAbortOnPanicEnv = isJust <$> lookupEnv "DbSyncAbortOnPanic"
