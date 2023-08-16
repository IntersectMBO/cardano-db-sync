{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Server.Config (
  ApplicationUser (..),
  ApplicationUsers (..),
  SmashServerConfig (..),
  SmashServerParams (..),
  defaultSmashPort,
  defaultSmashPool,
  paramsToConfig,
) where

import qualified Cardano.BM.Configuration.Model as Logging
import qualified Cardano.BM.Setup as Logging
import Cardano.BM.Trace (Trace)
import Cardano.Db (textShow)
import Cardano.Prelude
import Cardano.SMASH.Server.Types (DBFail (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import System.IO.Error

-- | SMASH Server cli parameters
data SmashServerParams = SmashServerParams
  { sspSmashPort :: !Int
  , sspConfigFile :: !FilePath -- config is only used for the logging parameters.
  , sspAdminUsers :: !(Maybe FilePath)
  , sspSmashPool :: !Int
  }

-- | Default Port for SMASH
defaultSmashPort :: Int
defaultSmashPort = 3100

-- | Default size of the Postgres connection pool
defaultSmashPool :: Int
defaultSmashPool = 5

-- | Convert cli parameters to SMASH configuration
paramsToConfig :: SmashServerParams -> IO SmashServerConfig
paramsToConfig params = do
  appUsers <- readAppUsers $ sspAdminUsers params
  tracer <- configureLogging (sspConfigFile params) "smash-server"

  pure $
    SmashServerConfig
      { sscSmashPort = sspSmashPort params
      , sscTrace = tracer
      , sscAdmins = appUsers
      , sspPsqlPool = sspSmashPool params
      }

-- | SMASH Server configuration
data SmashServerConfig = SmashServerConfig
  { sscSmashPort :: Int
  , sscTrace :: Trace IO Text
  , sscAdmins :: ApplicationUsers
  , sspPsqlPool :: Int
  }

-- | A data type we use to store user credentials.
data ApplicationUser = ApplicationUser
  { username :: !Text
  , password :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ApplicationUser

instance FromJSON ApplicationUser

-- | A list of users with special rights.
newtype ApplicationUsers = ApplicationUsers [ApplicationUser]
  deriving (Eq, Show, Generic)

instance ToJSON ApplicationUsers

instance FromJSON ApplicationUsers

-- Load application users from a file.
readAppUsers :: Maybe FilePath -> IO ApplicationUsers
readAppUsers mPath = case mPath of
  Nothing -> pure $ ApplicationUsers []
  Just path -> do
    userLines <- Text.lines <$> Text.readFile path
    let nonEmptyLines = filter (not . Text.null) userLines
    case mapM parseAppUser nonEmptyLines of
      Right users -> pure $ ApplicationUsers users
      Left err -> throwIO $ userError $ Text.unpack err

-- Parse application user as username,password
parseAppUser :: Text -> Either Text ApplicationUser
parseAppUser line = case Text.breakOn "," line of
  (user, commaPswd)
    | not (Text.null commaPswd)
    , passwd <- Text.tail commaPswd -> -- strip the comma
        Right $ ApplicationUser (prepareCred user) (prepareCred passwd)
  _ -> Left "Credentials need to be supplied in the form: username,password"
  where
    prepareCred name = Text.strip name

configureLogging :: FilePath -> Text -> IO (Trace IO Text)
configureLogging fp loggingName = do
  bs <- readByteString fp "DbSync" -- only uses the db-sync config
  case Yaml.decodeEither' bs of
    Left err -> throwIO $ ConfigError ("readSyncNodeConfig: Error parsing config: " <> textShow err)
    Right representation -> do
      -- Logging.Configuration
      logConfig <- Logging.setupFromRepresentation representation
      liftIO $ Logging.setupTrace (Right logConfig) loggingName

readByteString :: FilePath -> Text -> IO ByteString
readByteString fp cfgType =
  catch (BS.readFile fp) $ \(_ :: IOException) ->
    throwIO $ ConfigError $ mconcat ["Cannot find the ", cfgType, " configuration file at : ", Text.pack fp]
