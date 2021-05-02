{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.SMASH.Db.Error
  ( DBFail (..)
  , renderDBFail
  ) where

import           Cardano.Prelude

import           Data.Aeson (ToJSON (..), Value (..), object, (.=))

import           Cardano.Db (PoolIdentifier, PoolMetaHash)


-- | Errors, not exceptions.
data DBFail
  = DbLookupPoolMetadataHash !PoolIdentifier !PoolMetaHash
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
    toJSON failure@(DbLookupPoolMetadataHash _poolId _poolMDHash) =
        object
            [ "code"            .= String "DbLookupPoolMetadataHash"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@PoolMetadataHashMismatch =
        object
            [ "code"            .= String "PoolMetadataHashMismatch"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@PoolDelisted =
        object
            [ "code"            .= String "PoolDelisted"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@(UnableToEncodePoolMetadataToJSON _err) =
        object
            [ "code"            .= String "UnableToEncodePoolMetadataToJSON"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@(UnknownError _err) =
        object
            [ "code"            .= String "UnknownError"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@(ReservedTickerAlreadyInserted _tickerName) =
        object
            [ "code"            .= String "ReservedTickerAlreadyInserted"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@RecordDoesNotExist =
        object
            [ "code"            .= String "RecordDoesNotExist"
            , "description"     .= String (renderDBFail failure)
            ]
    toJSON failure@(DbInsertError _err) =
        object
            [ "code"            .= String "DbInsertError"
            , "description"     .= String (renderDBFail failure)
            ]

renderDBFail :: DBFail -> Text
renderDBFail lf =
  case lf of
    DbLookupPoolMetadataHash poolId poolMDHash -> "The metadata with hash " <> show poolId <> " for pool " <> show poolMDHash <> " is missing from the DB."
    PoolMetadataHashMismatch -> "The pool metadata does not match!"
    PoolDelisted -> "The pool has been delisted!"
    UnableToEncodePoolMetadataToJSON err -> "Unable to encode the content to JSON. " <> err
    UnknownError text -> "Unknown error. Context: " <> text
    ReservedTickerAlreadyInserted tickerName -> "Ticker '" <> tickerName <> "' has already been inserted."
    RecordDoesNotExist -> "The requested record does not exist."
    DbInsertError text -> "The database got an error while trying to insert a record. Error: " <> text

