{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbSync.Error.Types (
  SyncInvariant (..),
  SyncNodeError (..),
  NodeConfigError (..),
  bsBase16Encode,
) where

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Cardano.Crypto as Crypto (serializeCborHash)
import qualified Cardano.DbSync.Era.Byron.Util as Byron
import Cardano.Prelude
import qualified Data.ByteString.Base16 as Base16
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
  | SNErrConwayConfig !FilePath !Text
  | SNErrCardanoConfig !Text
  | SNErrInsertGenesis !Text
  | SNErrValidateGenesis !Text
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
      SNErrConwayConfig fp txt ->
        mconcat
          [ "Error SNErrConwayConfig: "
          , "Failed reading Conway genesis file "
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
      SNErrInsertGenesis err -> "Error SNErrInsertGenesis: " <> show err
      SNErrValidateGenesis err -> "Error SNErrValidateGenesis: " <> show err
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

bsBase16Encode :: ByteString -> Text
bsBase16Encode bs =
  case Text.decodeUtf8' (Base16.encode bs) of
    Left _ -> Text.pack $ "UTF-8 decode failed for " ++ Show.show bs
    Right txt -> txt

textShow :: (Show a) => a -> Text
textShow = Text.pack . show
