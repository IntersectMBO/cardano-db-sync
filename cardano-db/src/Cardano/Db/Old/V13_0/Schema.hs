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

module Cardano.Db.Old.V13_0.Schema where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Types (DbLovelace, DbWord64, ScriptType)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word16, Word64)
import Database.Persist.Documentation (deriveShowFields)
import Database.Persist.TH

share
  [ mkPersist sqlSettings
  , mkEntityDefList "entityDefs"
  , deriveShowFields
  ]
  [persistLowerCase|

  Block
    hash                ByteString          sqltype=hash32type
    epochNo             Word64 Maybe        sqltype=word31type
    slotNo              Word64 Maybe        sqltype=word63type
    epochSlotNo         Word64 Maybe        sqltype=word31type
    blockNo             Word64 Maybe        sqltype=word31type
    previousId          BlockId Maybe       OnDeleteCascade
    slotLeaderId        SlotLeaderId        noreference
    size                Word64              sqltype=word31type
    time                UTCTime             sqltype=timestamp
    txCount             Word64
    protoMajor          Word16              sqltype=word31type
    protoMinor          Word16              sqltype=word31type
    -- Shelley specific
    vrfKey              Text Maybe
    opCert              ByteString Maybe    sqltype=hash32type
    opCertCounter       Word64 Maybe        sqltype=word63type
    UniqueBlock         hash

  SlotLeader
    hash                ByteString          sqltype=hash28type
    poolHashId          PoolHashId Maybe    noreference       -- This will be non-null when a block is mined by a pool.
    description         Text                                  -- Description of the Slots leader.
    UniqueSlotLeader    hash

  PoolHash
    hashRaw             ByteString          sqltype=hash28type
    view                Text
    UniquePoolHash      hashRaw

  Tx
    hash                ByteString          sqltype=hash32type
    blockId             BlockId             OnDeleteCascade     -- This type is the primary key for the 'block' table.
    blockIndex          Word64              sqltype=word31type    -- The index of this transaction within the block.
    outSum              DbLovelace          sqltype=lovelace
    fee                 DbLovelace          sqltype=lovelace
    deposit             Int64                                   -- Needs to allow negaitve values.
    size                Word64              sqltype=word31type

    -- New for Allega
    invalidBefore       DbWord64 Maybe      sqltype=word64type
    invalidHereafter    DbWord64 Maybe      sqltype=word64type

    -- New for Alonzo
    validContract       Bool                                    -- False if the contract is invalid, True otherwise.
    scriptSize          Word64              sqltype=word31type
    UniqueTx            hash

  Datum
    hash                ByteString          sqltype=hash32type
    txId                TxId                OnDeleteCascade
    value               Text Maybe          sqltype=jsonb
    bytes               ByteString          sqltype=bytea
    UniqueDatum         hash

  RedeemerData
    hash                ByteString          sqltype=hash32type
    txId                TxId                OnDeleteCascade
    value               Text Maybe          sqltype=jsonb
    bytes               ByteString          sqltype=bytea
    UniqueRedeemerData  hash

  Script
    txId                TxId                noreference
    hash                ByteString          sqltype=hash28type
    type                ScriptType          sqltype=scripttype
    json                Text Maybe          sqltype=jsonb
    bytes               ByteString Maybe    sqltype=bytea
    serialisedSize      Word64 Maybe        sqltype=word31type
    UniqueScript        hash

  |]
