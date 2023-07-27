{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Db.Error (
  LookupFail (..),
  runOrThrowIODb,
) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Char8 (ByteString)
import qualified Data.Text.Encoding as Text
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Data.Text (Text)

data LookupFail
  = DbLookupBlockHash !ByteString
  | DbLookupBlockId !Word64
  | DbLookupMessage !Text
  | DbLookupTxHash !ByteString
  | DbLookupTxOutPair !ByteString !Word16
  | DbLookupEpochNo !Word64
  | DbLookupSlotNo !Word64
  | DbMetaEmpty
  | DbMetaMultipleRows
  | DBMultipleGenesis
  deriving (Eq, Generic)

instance Exception LookupFail

instance Show LookupFail where
  show =
    \case
      DbLookupBlockHash h -> "The block hash " <> show (base16encode h) <> " is missing from the DB."
      DbLookupBlockId blkid -> "block id " <> show blkid
      DbLookupMessage txt -> show txt
      DbLookupTxHash h -> "tx hash " <> show (base16encode h)
      DbLookupTxOutPair h i -> concat ["tx out pair (", show $ base16encode h, ", ", show i, ")"]
      DbLookupEpochNo e -> "epoch number " ++ show e
      DbLookupSlotNo s -> "slot number " ++ show s
      DbMetaEmpty -> "Meta table is empty"
      DbMetaMultipleRows -> "Multiple rows in Meta table which should only contain one"
      DBMultipleGenesis ->
        "Multiple Genesis blocks found. These are blocks without an EpochNo"

base16encode :: ByteString -> Text
base16encode = Text.decodeUtf8 . Base16.encode

runOrThrowIODb :: forall e a. Exception e => IO (Either e a) -> IO a
runOrThrowIODb ioEither = do
  et <- ioEither
  case et of
    Left err -> throwIO err
    Right a -> pure a
