{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Cardano.Db.Error
  ( LookupFail (..)
  , renderLookupFail
  ) where


import           Data.Aeson (ToJSON (..))
import qualified Data.ByteString.Base16 as Base16
import           Data.ByteString.Char8 (ByteString)
import           GHC.Generics
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word16, Word64)

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
  deriving (Eq, Show, Generic)

instance ToJSON LookupFail where
    toJSON _ = undefined -- _failure@(DbLookupBlockHash _hash) = undefined

renderLookupFail :: LookupFail -> Text
renderLookupFail lf =
  case lf of
    DbLookupBlockHash h -> "The block hash " <> base16encode h <> " is missing from the DB."
    DbLookupBlockId blkid -> "block id " <> textShow blkid
    DbLookupMessage txt -> txt
    DbLookupTxHash h -> "tx hash " <> base16encode h
    DbLookupTxOutPair h i ->
        Text.concat [ "tx out pair (", base16encode h, ", ", textShow i, ")" ]
    DbLookupEpochNo e ->
        Text.concat [ "epoch number ", textShow e ]
    DbLookupSlotNo s ->
        Text.concat [ "slot number ", textShow s ]
    DbMetaEmpty -> "Meta table is empty"
    DbMetaMultipleRows -> "Multiple rows in Meta table which should only contain one"
    DBMultipleGenesis ->
        "Multiple Genesis blocks found. These are blocks without an EpochNo"

base16encode :: ByteString -> Text
base16encode = Text.decodeUtf8 . Base16.encode

textShow :: Show a => a -> Text
textShow = Text.pack . show
