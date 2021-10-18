{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.SMASH.Server.Run
  ( runSmashServer
  , runAppStubbed
  ) where

import           Cardano.Prelude

import           Servant (Application, BasicAuthCheck (..), BasicAuthData (..),
                   BasicAuthResult (..), Context (..), serveWithContext)

import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Cardano.BM.Trace (Trace, logInfo)

import           Cardano.Db (textShow)

import           Cardano.SMASH.Server.Api
import           Cardano.SMASH.Server.Impl
import           Cardano.SMASH.Server.PoolDataLayer
import           Cardano.SMASH.Server.Types

import           System.IO.Error

runSmashServer :: Trace IO Text -> PoolDataLayer -> Maybe FilePath -> Int -> IO ()
runSmashServer tracer dataLayer mUsersFile port = do

    appUsers <- readAppUsers mUsersFile

    let settings =
          setPort port $
          setBeforeMainLoop
            (logInfo tracer $ "SMASH listening on port " <> textShow port)
          defaultSettings

    runSettings settings =<< mkApp dataLayer appUsers

readAppUsers :: Maybe FilePath -> IO ApplicationUsers
readAppUsers mPath = case mPath of
  Nothing -> pure $ ApplicationUsers []
  Just path -> do
    userLines <- Text.lines <$> Text.readFile path
    case mapM parseAppUser userLines of
      Right users -> pure $ ApplicationUsers users
      Left err -> throwIO $ userError $ Text.unpack err

parseAppUser :: Text -> Either Text ApplicationUser
parseAppUser line = case Text.breakOn "," line of
    (user, commaPswd)
      | not (Text.null commaPswd)
      , passwd <- Text.tail commaPswd -- strip the comma
      -> Right $ ApplicationUser (prepareCred user) (prepareCred passwd)
    _ -> Left "Credentials need to be supplied in the form: username,password"
  where
    prepareCred name = Text.strip name

mkApp :: PoolDataLayer -> ApplicationUsers -> IO Application
mkApp dataLayer appUsers = do

    -- Ugly hack, wait 2s for migrations to run for the admin user to be created.
    -- You can always run the migrations first.
    threadDelay 2_000_000

    pure $ serveWithContext
        fullAPI
        (basicAuthServerContext appUsers)
        (server dataLayer)

-- | We need to supply our handlers with the right Context.
basicAuthServerContext :: ApplicationUsers -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext applicationUsers = authCheck applicationUsers :. EmptyContext
  where
    -- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
    authCheck :: ApplicationUsers -> BasicAuthCheck User
    authCheck applicationUsers' =

        let check' :: BasicAuthData -> IO (BasicAuthResult User)
            check' (BasicAuthData username' password') = do
                let usernameText = decodeUtf8 username'
                let passwordText = decodeUtf8 password'

                let applicationUser  = ApplicationUser usernameText passwordText
                let userAuthValidity = checkIfUserValid applicationUsers' applicationUser

                case userAuthValidity of
                    UserValid user -> pure (Authorized user)
                    UserInvalid    -> pure Unauthorized

        in BasicAuthCheck check'

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
checkIfUserValid :: ApplicationUsers -> ApplicationUser -> UserValidity
checkIfUserValid (ApplicationUsers applicationUsers) applicationUser@(ApplicationUser usernameText _) =
    if applicationUser `elem` applicationUsers
        then UserValid (User usernameText)
        else UserInvalid

-- Stub api

runAppStubbed :: Int -> IO ()
runAppStubbed port = do
    let settings =
          setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("SMASH-stubbed listening on port " ++ show port))
          defaultSettings

    runSettings settings =<< mkAppStubbed

mkAppStubbed :: IO Application
mkAppStubbed = do
    dataLayer <- createCachedPoolDataLayer Nothing

    pure $ serveWithContext
        fullAPI
        (basicAuthServerContext stubbedApplicationUsers)
        (server dataLayer)

stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "user" "password"]
