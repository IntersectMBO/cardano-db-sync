{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Db.PGConfig (
  PGConfig (..),
  PGPassError (..),
  PGPassFile (..),
  PGPassSource (..),
  parsePGConfig,
  readPGPassDefault,
  readPGPass,
  readPGPassFileEnv,
  readPGPassFile,
  readPGPassFileExit,
  toConnectionString,
) where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Database.Persist.Postgresql (ConnectionString)
import System.Environment (lookupEnv, setEnv)
import System.Posix.User (getEffectiveUserName)

data PGPassSource
  = PGPassDefaultEnv
  | PGPassEnv String
  | PGPassCached PGConfig
  deriving (Show)

-- | PGConfig as specified by https://www.postgresql.org/docs/11/libpq-pgpass.html
-- However, this module expects the config data to be on the first line.
data PGConfig = PGConfig
  { pgcHost :: ByteString
  , pgcPort :: ByteString
  , pgcDbname :: ByteString
  , pgcUser :: ByteString
  , pgcPassword :: ByteString
  }
  deriving (Show)

newtype PGPassFile
  = PGPassFile FilePath

toConnectionString :: PGConfig -> ConnectionString
toConnectionString pgc =
  BS.concat
    [ "host="
    , pgcHost pgc
    , " "
    , "port="
    , pgcPort pgc
    , " "
    , "user="
    , pgcUser pgc
    , " "
    , "dbname="
    , pgcDbname pgc
    , " "
    , "password="
    , pgcPassword pgc
    ]

readPGPassDefault :: IO (Either PGPassError PGConfig)
readPGPassDefault = readPGPass PGPassDefaultEnv

-- | Read the PostgreSQL configuration from the file at the location specified by the
-- '$PGPASSFILE' environment variable.
readPGPass :: PGPassSource -> IO (Either PGPassError PGConfig)
readPGPass source = case source of
  PGPassDefaultEnv -> readPGPassFileEnv "PGPASSFILE"
  PGPassEnv name -> readPGPassFileEnv name
  PGPassCached config -> pure $ Right config

readPGPassFileEnv :: String -> IO (Either PGPassError PGConfig)
readPGPassFileEnv name = do
  mpath <- lookupEnv name
  case mpath of
    Just fp -> readPGPassFileExit (PGPassFile fp)
    Nothing -> pure $ Left (EnvVarableNotSet name)

-- | Read the PostgreSQL configuration from the specified file.
readPGPassFile :: PGPassFile -> IO (Either PGPassError PGConfig)
readPGPassFile (PGPassFile fpath) = do
  ebs <- Exception.try $ BS.readFile fpath
  case ebs of
    Left err -> pure $ Left (FailedToReadPGPassFile fpath err)
    Right bs -> extract bs
  where
    extract :: ByteString -> IO (Either PGPassError PGConfig)
    extract bs =
      case BS.lines bs of
        (b : _) -> parsePGConfig b
        _ -> pure $ Left (FailedToParsePGPassConfig bs)

parsePGConfig :: ByteString -> IO (Either PGPassError PGConfig)
parsePGConfig bs =
  case BS.split ':' bs of
    [h, pt, d, u, pwd] -> replaceUser (PGConfig h pt d u pwd)
    _ -> pure $ Left (FailedToParsePGPassConfig bs)
  where
    replaceUser :: PGConfig -> IO (Either PGPassError PGConfig)
    replaceUser pgc
      | pgcUser pgc /= "*" = pure $ Right pgc
      | otherwise = do
          euser <- Exception.try getEffectiveUserName
          case euser of
            Left (err :: IOException) ->
              pure $ Left (UserFailed err)
            Right user ->
              pure $ Right (pgc {pgcUser = BS.pack user})

-- | Read 'PGPassFile' into 'PGConfig'.
-- If it fails it will raise an error.
-- If it succeeds, it will set the 'PGPASSFILE' environment variable.
readPGPassFileExit :: PGPassFile -> IO (Either PGPassError PGConfig)
readPGPassFileExit pgpassfile@(PGPassFile fpath) = do
  epgp <- readPGPassFile pgpassfile
  case epgp of
    Left err -> pure $ Left err
    Right pgc -> do
      setEnv "PGPASSFILE" fpath
      pure $ Right pgc

data PGPassError
  = EnvVarableNotSet String
  | UserFailed IOException
  | FailedToReadPGPassFile FilePath IOException
  | FailedToParsePGPassConfig ByteString

instance Exception.Exception PGPassError

instance Show PGPassError where
  show =
    \case
      EnvVarableNotSet str ->
        mconcat ["Environment variable '", show str, " not set."]
      UserFailed err ->
        mconcat
          [ "readPGPassFile: User in pgpass file was specified as '*' but "
          , "getEffectiveUserName failed with "
          , show err
          ]
      FailedToReadPGPassFile fpath err ->
        mconcat
          [ "Not able to read PGPassFile at "
          , show $ Text.pack fpath
          , "."
          , "Failed with "
          , show err
          ]
      FailedToParsePGPassConfig bs ->
        "Failed to parse config from " <> show bs
