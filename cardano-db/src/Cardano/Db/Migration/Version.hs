{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Migration.Version (
  MigrationVersion (..),
  parseMigrationVersionFromFile,
  renderMigrationVersion,
  renderMigrationVersionFile,
) where

import qualified Data.List as List
import qualified Data.List.Extra as List
import Text.Printf (printf)
import Text.Read (readMaybe)

data MigrationVersion = MigrationVersion
  { mvStage :: Int
  , mvVersion :: Int
  , mvDate :: Int
  }
  deriving (Eq, Ord, Show)

parseMigrationVersionFromFile :: String -> Maybe MigrationVersion
parseMigrationVersionFromFile str =
  case List.splitOn "-" (List.takeWhile (/= '.') str) of
    [_, stage, ver, date] ->
      case (readMaybe stage, readMaybe ver, readMaybe date) of
        (Just s, Just v, Just d) -> Just $ MigrationVersion s v d
        _ -> Nothing
    _ -> Nothing

renderMigrationVersion :: MigrationVersion -> String
renderMigrationVersion mv =
  List.intercalate
    "-"
    [ printf "%d" (mvStage mv)
    , printf "%04d" (mvVersion mv)
    , show (mvDate mv)
    ]

renderMigrationVersionFile :: MigrationVersion -> String
renderMigrationVersionFile mv =
  List.concat
    [ "migration-"
    , renderMigrationVersion mv
    , ".sql"
    ]
