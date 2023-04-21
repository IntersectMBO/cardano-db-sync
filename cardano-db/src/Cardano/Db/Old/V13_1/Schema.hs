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
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Old.V13_0.Schema where

import Cardano.Db.Schema.Orphans ()
import Cardano.Db.Types (DbLovelace, DbWord64)
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


  TxOut
    txId                TxId                noreference
    index               Word64              sqltype=txindex
    address             Text
    addressRaw          ByteString
    addressHasScript    Bool
    paymentCred         ByteString Maybe    sqltype=hash28type
    stakeAddressId      StakeAddressId Maybe noreference
    value               DbLovelace          sqltype=lovelace
    dataHash            ByteString Maybe    sqltype=hash32type
    inlineDatumId       DatumId Maybe       noreference
    referenceScriptId   ScriptId Maybe      noreference
    UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.


  |]
