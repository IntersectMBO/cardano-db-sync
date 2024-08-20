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
  , mkEntityDefList "entityDefs"
  , deriveShowFields
  ]
  [persistLowerCase|
----------------------------------------------
-- Bassic Address TxOut
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
-- MultiAsset
----------------------------------------------
  MaTxOut
    ident               MultiAssetId        noreference
    quantity            DbWord64            sqltype=word64type
    txOutId             TxOutId             noreference
    deriving Show

|]

schemaDocs :: [EntityDef]
schemaDocs =
  document entityDefs $ do
    TxOut --^ do
      "A table for transaction outputs."
      TxOutAddress # "The human readable encoding of the output address. Will be Base58 for Byron era addresses and Bech32 for Shelley era."
      TxOutAddressHasScript # "Flag which shows if this address is locked by a script."
      TxOutDataHash # "The hash of the transaction output datum. (NULL for Txs without scripts)."
      TxOutIndex # "The index of this transaction output with the transaction."
      TxOutInlineDatumId # "The inline datum of the output, if it has one. New in v13."
      TxOutPaymentCred # "The payment credential part of the Shelley address. (NULL for Byron addresses). For a script-locked address, this is the script hash."
      TxOutReferenceScriptId # "The reference script of the output, if it has one. New in v13."
      TxOutStakeAddressId # "The StakeAddress table index for the stake address part of the Shelley address. (NULL for Byron addresses)."
      TxOutValue # "The output value (in Lovelace) of the transaction output."

      TxOutTxId # "The Tx table index of the transaction that contains this transaction output."

    MaTxOut --^ do
      "A table containing Multi-Asset transaction outputs."
      MaTxOutIdent # "The MultiAsset table index specifying the asset."
      MaTxOutQuantity # "The Multi Asset transaction output amount (denominated in the Multi Asset)."
      MaTxOutTxOutId # "The TxOut table index for the transaction that this Multi Asset transaction output."
