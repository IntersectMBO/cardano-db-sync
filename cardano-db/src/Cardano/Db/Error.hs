{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Error (
  -- AsDbError (..),
  CallSite (..),
  DbError (..),
  runOrThrowIODb,
  runOrThrowIO,
  logAndThrowIO,
  base16encode,
) where

import Cardano.BM.Trace (Trace, logError)
import Cardano.Prelude (MonadIO, throwIO)
import Control.Exception (Exception)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import qualified Hasql.Session as HsqlSes

data DbError = DbError
  { dbErrorCallSite :: !CallSite
  , dbErrorMessage :: !Text
  , dbErrorCause :: !(Maybe HsqlSes.SessionError) -- Now a Maybe
  }
  deriving (Show, Eq)

instance Exception DbError

data CallSite = CallSite
  { csModule :: !Text
  , csFile :: !Text
  , csLine :: !Int
  }
  deriving (Show, Eq)

base16encode :: ByteString -> Text
base16encode = Text.decodeUtf8 . Base16.encode

runOrThrowIODb :: forall e a. Exception e => IO (Either e a) -> IO a
runOrThrowIODb ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a

runOrThrowIO :: forall e a m. (MonadIO m) => (Exception e) => m (Either e a) -> m a
runOrThrowIO ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a

logAndThrowIO :: Trace IO Text -> Text -> IO a
logAndThrowIO tracer msg = do
  logError tracer msg
  throwIO $ userError $ show msg

-- data LookupContext
--   = BlockHashContext !ByteString
--   | BlockIdContext !Word64
--   | MessageContext !Text
--   | TxHashContext !ByteString
--   | TxOutPairContext !ByteString !Word16
--   | EpochNoContext !Word64
--   | SlotNoContext !Word64
--   | GovActionPairContext !TxId !Word64
--   | MetaEmptyContext
--   | MetaMultipleRowsContext
--   | MultipleGenesisContext
--   | ExtraMigrationContext !String
--   | PruneConsumedContext !String
--   | RJsonbInSchemaContext !String
--   | TxOutVariantContext !String
--   deriving (Show, Eq, Generic)

-- instance Exception LookupContext

-- catchDbError :: String -> HsqlT.Transaction a -> HsqlT.Transaction a
-- catchDbError context action =
--   action `catch` \e ->
--     throwError $ DbError $ context ++ ": " ++ show e

-- instance Show LookupFail where
--   show =
--     \case
--       DbLookupBlockHash h -> "The block hash " <> show (base16encode h) <> " is missing from the DB."
--       DbLookupBlockId blkid -> "block id " <> show blkid
--       DbLookupMessage txt -> show txt
--       DbLookupTxHash h -> "tx hash " <> show (base16encode h)
--       DbLookupTxOutPair h i -> concat ["tx out pair (", show $ base16encode h, ", ", show i, ")"]
--       DbLookupEpochNo e -> "epoch number " ++ show e
--       DbLookupSlotNo s -> "slot number " ++ show s
--       DbLookupGovActionPair txId index -> concat ["missing GovAction (", show txId, ", ", show index, ")"]
--       DbMetaEmpty -> "Meta table is empty"
--       DbMetaMultipleRows -> "Multiple rows in Meta table which should only contain one"
--       DBMultipleGenesis -> "Multiple Genesis blocks found. These are blocks without an EpochNo"
--       DBExtraMigration e -> "DBExtraMigration : " <> e
--       DBPruneConsumed e -> "DBExtraMigration" <> e
--       DBRJsonbInSchema e -> "DBRJsonbInSchema" <> e
--       DBTxOutVariant e -> "DbTxOutVariant" <> e
