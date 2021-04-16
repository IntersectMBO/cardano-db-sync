{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.SMASH.DBSync.Db.Error
  ( DBFail (..)
  , renderLookupFail
  ) where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), Value (..), object, (.=))

import           Cardano.SMASH.DBSync.Db.Types


-- | Errors, not exceptions.
data DBFail
  = DbLookupBlockHash !ByteString
  | DbLookupPoolMetadataHash !PoolId !PoolMetadataHash
  | DbMetaEmpty
  | DbMetaMultipleRows
  | PoolMetadataHashMismatch
  | PoolDelisted
  | UnableToEncodePoolMetadataToJSON !Text
  | UnknownError !Text
  | ReservedTickerAlreadyInserted !Text
  | RecordDoesNotExist
  | DbInsertError !Text
  deriving (Eq, Show, Generic)

{-

The example we agreed would be:
```
{
    "code": "ERR_4214",
    "description": "You did something wrong."
}
```

-}

instance ToJSON DBFail where
    toJSON failure@(DbLookupBlockHash _hash) =
        object
            [ "code"            .= String "DbLookupBlockHash"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@(DbLookupPoolMetadataHash _poolId _poolMDHash) =
        object
            [ "code"            .= String "DbLookupPoolMetadataHash"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@DbMetaEmpty =
        object
            [ "code"            .= String "DbMetaEmpty"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@DbMetaMultipleRows =
        object
            [ "code"            .= String "DbMetaMultipleRows"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@PoolMetadataHashMismatch =
        object
            [ "code"            .= String "PoolMetadataHashMismatch"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@PoolDelisted =
        object
            [ "code"            .= String "PoolDelisted"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@(UnableToEncodePoolMetadataToJSON _err) =
        object
            [ "code"            .= String "UnableToEncodePoolMetadataToJSON"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@(UnknownError _err) =
        object
            [ "code"            .= String "UnknownError"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@(ReservedTickerAlreadyInserted _tickerName) =
        object
            [ "code"            .= String "ReservedTickerAlreadyInserted"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@RecordDoesNotExist =
        object
            [ "code"            .= String "RecordDoesNotExist"
            , "description"     .= String (renderLookupFail failure)
            ]
    toJSON failure@(DbInsertError _err) =
        object
            [ "code"            .= String "DbInsertError"
            , "description"     .= String (renderLookupFail failure)
            ]

renderLookupFail :: DBFail -> Text
renderLookupFail lf =
  case lf of
    DbLookupBlockHash hash -> "The block hash " <> decodeUtf8 hash <> " is missing from the DB."
    DbLookupPoolMetadataHash poolId poolMDHash -> "The metadata with hash " <> show poolId <> " for pool " <> show poolMDHash <> " is missing from the DB."
    DbMetaEmpty -> "The metadata table is empty!"
    DbMetaMultipleRows -> "The metadata table contains multiple rows. Error."
    PoolMetadataHashMismatch -> "The pool metadata does not match!"
    PoolDelisted -> "The pool has been delisted!"
    UnableToEncodePoolMetadataToJSON err -> "Unable to encode the content to JSON. " <> err
    UnknownError text -> "Unknown error. Context: " <> text
    ReservedTickerAlreadyInserted tickerName -> "Ticker '" <> tickerName <> "' has already been inserted."
    RecordDoesNotExist -> "The requested record does not exist."
    DbInsertError text -> "The database got an error while trying to insert a record. Error: " <> text

