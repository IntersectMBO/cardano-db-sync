{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.SMASH.Server.Run (
  runSmashServer,
  runAppStubbed,
) where

import Cardano.BM.Trace (Trace, logInfo)
import Cardano.Db (
  PGPassSource (PGPassDefaultEnv),
  readPGPass,
  runOrThrowIODb,
  textShow,
  toConnectionString,
 )
import qualified Cardano.Db as Db
import Cardano.Prelude
import Cardano.SMASH.Server.Api
import Cardano.SMASH.Server.Config
import Cardano.SMASH.Server.Impl
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import Database.Persist.Postgresql (withPostgresqlPool)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant (
  Application,
  BasicAuthCheck (..),
  BasicAuthData (..),
  BasicAuthResult (..),
  Context (..),
  serveWithContext,
 )

runSmashServer :: SmashServerConfig -> IO ()
runSmashServer config = do
  let trce = sscTrace config
  let settings =
        setPort (sscSmashPort config) $
          setBeforeMainLoop
            (logInfo trce $ "SMASH listening on port " <> textShow (sscSmashPort config))
            defaultSettings

  pgconfig <- runOrThrowIODb (readPGPass PGPassDefaultEnv)
  Db.runIohkLogging trce $ withPostgresqlPool (toConnectionString pgconfig) (sscSmashPort config) $ \pool -> do
    let poolDataLayer = postgresqlPoolDataLayer trce pool
    app <- liftIO $ mkApp (sscTrace config) poolDataLayer (sscAdmins config)
    liftIO $ runSettings settings app

mkApp :: Trace IO Text -> PoolDataLayer -> ApplicationUsers -> IO Application
mkApp trce dataLayer appUsers = do
  -- Ugly hack, wait 2s for migrations to run for the admin user to be created.
  -- You can always run the migrations first.
  threadDelay 2_000_000

  pure $
    serveWithContext
      fullAPI
      (basicAuthServerContext appUsers)
      (server $ ServerEnv trce dataLayer)

-- | We need to supply our handlers with the right Context.
basicAuthServerContext :: ApplicationUsers -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext applicationUsers = authCheck applicationUsers :. EmptyContext
  where
    -- \| 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
    authCheck :: ApplicationUsers -> BasicAuthCheck User
    authCheck applicationUsers' =
      let check' :: BasicAuthData -> IO (BasicAuthResult User)
          check' (BasicAuthData username' password') = do
            let usernameText = decodeUtf8 username'
            let passwordText = decodeUtf8 password'

            let applicationUser = ApplicationUser usernameText passwordText
            let userAuthValidity = checkIfUserValid applicationUsers' applicationUser

            case userAuthValidity of
              UserValid user -> pure (Authorized user)
              UserInvalid -> pure Unauthorized
       in BasicAuthCheck check'

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
checkIfUserValid :: ApplicationUsers -> ApplicationUser -> UserValidity
checkIfUserValid (ApplicationUsers applicationUsers) applicationUser@(ApplicationUser usernameText _) =
  if applicationUser `elem` applicationUsers
    then UserValid (User usernameText)
    else UserInvalid

-- Stub api

runAppStubbed :: Trace IO Text -> Int -> IO ()
runAppStubbed trce port = do
  let settings =
        setPort port $
          setBeforeMainLoop
            (hPutStrLn stderr ("SMASH-stubbed listening on port " ++ show port))
            defaultSettings

  runSettings settings =<< mkAppStubbed trce

mkAppStubbed :: Trace IO Text -> IO Application
mkAppStubbed trce = do
  dataLayer <- createCachedPoolDataLayer Nothing

  pure $
    serveWithContext
      fullAPI
      (basicAuthServerContext stubbedApplicationUsers)
      (server $ ServerEnv trce dataLayer)

stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "user" "password"]
