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
  toConnectionSetting,
) where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv)
import System.Posix.User (getEffectiveUserName)
import qualified Hasql.Connection.Setting.Connection as HCS
import qualified Hasql.Connection.Setting.Connection.Param as HCSP
import qualified Hasql.Connection.Setting as HC
import Cardano.Prelude (decodeUtf8)
import Data.Word (Word16)
import qualified Data.Text.Read as Text (decimal)
import Control.Monad.Extra (unless)


data PGPassSource
  = PGPassDefaultEnv
  | PGPassEnv String
  | PGPassCached PGConfig
  deriving (Show)

-- | Preconstructed connection string according to <https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-CONNSTRING the PostgreSQL format>.
data PGConfig = PGConfig
  { pgcHost :: Text.Text
  , pgcPort :: Text.Text
  , pgcDbname :: Text.Text
  , pgcUser :: Text.Text
  , pgcPassword :: Text.Text
  }
  deriving (Show)

newtype PGPassFile
  = PGPassFile FilePath

-- | Convert PGConfig to Hasql connection settings, or return an error message.
toConnectionSetting :: PGConfig -> Either String HC.Setting
toConnectionSetting pgc = do
  -- Convert the port from Text to Word16
  portWord16 <- textToWord16 (pgcPort pgc)
  -- Build the connection settings
  pure $ HC.connection (HCS.params [host, port portWord16 , user, dbname, password])
  where
    host = HCSP.host (pgcHost pgc)
    port = HCSP.port
    user = HCSP.user (pgcUser pgc)
    dbname = HCSP.dbname (pgcDbname pgc)
    password = HCSP.password (pgcPassword pgc)

-- | Convert a Text port to Word16, or return an error message.
textToWord16 :: Text.Text -> Either String Word16
textToWord16 portText =
  case Text.decimal portText of
    Left err ->
      Left $ "Invalid port: '" <> Text.unpack portText <> "'. " <> err
    Right (portInt, remainder) -> do
      -- Check for leftover characters (e.g., "123abc" is invalid)
      unless (Text.null remainder) $
        Left $ "Invalid port: '" <> Text.unpack portText <> "'. Contains non-numeric characters."
      -- Check if the port is within the valid Word16 range (0-65535)
      unless (portInt >= (0 :: Integer) && portInt <= 65535) $
        Left $ "Invalid port: '" <> Text.unpack portText <> "'. Port must be between 0 and 65535."
      -- Convert to Word16
      Right (fromIntegral portInt)

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
        _otherwise -> pure $ Left (FailedToParsePGPassConfig bs)

parsePGConfig :: ByteString -> IO (Either PGPassError PGConfig)
parsePGConfig bs =
  case BS.split ':' bs of
    [h, pt, d, u, pwd] ->
      replaceUser (PGConfig
                    (decodeUtf8 h)
                    (decodeUtf8 pt)
                    (decodeUtf8 d)
                    (decodeUtf8 u)
                    (decodeUtf8 pwd)
                  )
    _otherwise -> pure $ Left (FailedToParsePGPassConfig bs)
  where
    replaceUser :: PGConfig -> IO (Either PGPassError PGConfig)
    replaceUser pgc
      | pgcUser pgc /= Text.pack "*" = pure $ Right pgc
      | otherwise = do
          euser <- Exception.try getEffectiveUserName
          case euser of
            Left (err :: IOException) ->
              pure $ Left (UserFailed err)
            Right user ->
              pure $ Right (pgc {pgcUser = Text.pack user})

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
