{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema.Core.TxOut where

import Cardano.Db.Schema.BaseSchema (DatumId, MultiAssetId, ScriptId, StakeAddressId, TxId)
import Cardano.Db.Types (DbLovelace, DbWord64)
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import Database.Persist.Documentation (deriveShowFields, document, (#), (--^))
import Database.Persist.EntityDef.Internal (EntityDef (..))
import Database.Persist.TH

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateCoreTxOutCardanoDb"
  , mkEntityDefList "entityDefsTxOutCore"
  , deriveShowFields
  ]
  [persistLowerCase|
----------------------------------------------
-- Core TxOut
----------------------------------------------
  TxOut
    address             Text
    addressHasScript    Bool
    dataHash            ByteString Maybe    sqltype=hash32type
    consumedByTxId      TxId Maybe          noreference
    index               Word64              sqltype=txindex
    inlineDatumId       DatumId Maybe       noreference
    paymentCred         ByteString Maybe    sqltype=hash28type
    referenceScriptId   ScriptId Maybe      noreference
    stakeAddressId      StakeAddressId Maybe noreference
    txId                TxId                noreference
    value               DbLovelace          sqltype=lovelace
    UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.

----------------------------------------------
-- Core CollateralTxOut
----------------------------------------------
  CollateralTxOut
    txId                TxId                noreference     -- This type is the primary key for the 'tx' table.
    index               Word64              sqltype=txindex
    address             Text
    addressHasScript    Bool
    paymentCred         ByteString Maybe    sqltype=hash28type
    stakeAddressId      StakeAddressId Maybe noreference
    value               DbLovelace          sqltype=lovelace
    dataHash            ByteString Maybe    sqltype=hash32type
    multiAssetsDescr    Text
    inlineDatumId       DatumId Maybe       noreference
    referenceScriptId   ScriptId Maybe      noreference
    deriving Show

----------------------------------------------
-- MultiAsset
----------------------------------------------
  MaTxOut
    ident               MultiAssetId        noreference
    quantity            DbWord64            sqltype=word64type
    txOutId             TxOutId             noreference
    deriving Show

|]

schemaDocsTxOutCore :: [EntityDef]
schemaDocsTxOutCore =
  document entityDefsTxOutCore $ do
    TxOut --^ do
      "A table for transaction outputs."
      TxOutAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
      TxOutAddressHasScript # "Flag which shows if this address is locked by a script."
      TxOutConsumedByTxId # "The Tx table index of the transaction that consumes this transaction output. Not populated by default, can be activated via tx-out configs."
      TxOutDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
      TxOutIndex # "The index of this transaction output with the transaction."
      TxOutInlineDatumId # "The inline datum of the output, if it has one. New in v13."
      TxOutPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
      TxOutReferenceScriptId # "The reference script of the output, if it has one. New in v13."
      TxOutStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
      TxOutValue # "The output value (in Lovelace) of the transaction output."

      TxOutTxId # "The Tx table index of the transaction that contains this transaction output."

    CollateralTxOut --^ do
      "A table for transaction collateral outputs. New in v13."
      CollateralTxOutTxId # "The Tx table index of the transaction that contains this transaction output."
      CollateralTxOutIndex # "The index of this transaction output with the transaction."
      CollateralTxOutAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
      CollateralTxOutAddressHasScript # "Flag which shows if this address is locked by a script."
      CollateralTxOutPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
      CollateralTxOutStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
      CollateralTxOutValue # "The output value (in Lovelace) of the transaction output."
      CollateralTxOutDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
      CollateralTxOutMultiAssetsDescr # "This is a description of the multiassets in collateral output. Since the output is not really created, we don't need to add them in separate tables."
      CollateralTxOutInlineDatumId # "The inline datum of the output, if it has one. New in v13."
      CollateralTxOutReferenceScriptId # "The reference script of the output, if it has one. New in v13."

    MaTxOut --^ do
      "A table containing Multi-Asset transaction outputs."
      MaTxOutIdent # "The MultiAsset table index specifying the asset."
      MaTxOutQuantity # "The Multi Asset transaction output amount (denominated in the Multi Asset)."
      MaTxOutTxOutId # "The TxOut table index for the transaction that this Multi Asset transaction output."
