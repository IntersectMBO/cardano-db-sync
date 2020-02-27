{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Migration.Version
  ( MigrationVersion (..)
  , parseMigrationVersionFromFile
  , nextMigrationVersion
  , renderMigrationVersion
  , renderMigrationVersionFile
  ) where

import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time

import           Text.Printf (printf)


data MigrationVersion = MigrationVersion
  { mvStage :: Int
  , mvVersion :: Int
  , mvDate :: Int
  } deriving (Eq, Ord, Show)


parseMigrationVersionFromFile :: String -> Maybe MigrationVersion
parseMigrationVersionFromFile str =
  case List.splitOn "-" (List.takeWhile (/= '.') str) of
    [_, stage, ver, date] ->
      case (readMaybe stage, readMaybe ver, readMaybe date) of
        (Just s, Just v, Just d) -> Just $ MigrationVersion s v d
        _ -> Nothing
    _ -> Nothing

nextMigrationVersion :: MigrationVersion -> IO MigrationVersion
nextMigrationVersion (MigrationVersion _stage ver _date) = do
  -- We can ignore the provided 'stage' and 'date' fields, but we do bump the version number.
  -- All new versions have 'stage == 2' because the stage 2 migrations are the Presistent
  -- generated ones. For the date we use today's date.
  (y, m, d) <- Time.toGregorian . Time.utctDay <$> Time.getCurrentTime
  pure $ MigrationVersion 2 (ver + 1) (fromIntegral y * 10000 + m * 100 + d)

renderMigrationVersion :: MigrationVersion -> String
renderMigrationVersion mv =
  List.intercalate "-"
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

-- -----------------------------------------------------------------------------

readMaybe :: Read a => String -> Maybe a
readMaybe str =
  case reads str of
    [(a, "")] -> Just a
    _ -> Nothing
