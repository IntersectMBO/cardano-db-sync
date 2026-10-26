{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.SMASH.Server.Run (
  runSmashServer,
  runAppStubbed,
) where

import qualified Cardano.Db as DB
import Cardano.Db.Log (LogMessage, logInfo)
import Cardano.Logging (Trace)
import Cardano.Prelude
import Cardano.SMASH.Server.Api
import Cardano.SMASH.Server.Config
import Cardano.SMASH.Server.Impl
import Cardano.SMASH.Server.PoolDataLayer
import Cardano.SMASH.Server.Types
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as TextError
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant (
  Application,
  BasicAuthCheck (..),
  BasicAuthData (..),
  BasicAuthResult (..),
  Context (..),
  serveWithContext,
 )
import Prelude (userError)

runSmashServer :: SmashServerConfig -> IO ()
runSmashServer config = do
  let trce = sscTrace config
  let settings =
        setPort (sscSmashPort config) $
          setBeforeMainLoop
            (logInfo trce $ "SMASH listening on port " <> textShow (sscSmashPort config))
            defaultSettings

  pgconfig <- DB.runOrThrowIODb (DB.readPGPass DB.PGPassDefaultEnv)
  connSetting <- case DB.toConnectionSetting pgconfig of
    Left err -> throwIO $ userError err
    Right setting -> pure setting

  -- Create the Hasql connection pool
  pool <- DB.createHasqlConnectionPool [connSetting] 4
  -- Setup app with the pool
  app <- mkApp (sscTrace config) (postgresqlPoolDataLayer trce pool) (sscAdmins config)
  -- Run the web server
  runSettings settings app

mkApp :: Trace IO LogMessage -> PoolDataLayer -> ApplicationUsers -> IO Application
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
            -- Leniently, because the strict 'decodeUtf8' throws on invalid UTF-8 in
            -- the Authorization header instead of refusing the request.
            let usernameText = Text.decodeUtf8With TextError.lenientDecode username'
            let passwordText = Text.decodeUtf8With TextError.lenientDecode password'

            let applicationUser = ApplicationUser usernameText passwordText
            let userAuthValidity = checkIfUserValid applicationUsers' applicationUser

            case userAuthValidity of
              UserValid user -> pure (Authorized user)
              UserInvalid -> pure Unauthorized
       in BasicAuthCheck check'

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
checkIfUserValid :: ApplicationUsers -> ApplicationUser -> UserValidity
checkIfUserValid (ApplicationUsers applicationUsers) (ApplicationUser usernameText passwordText) =
  if any matches applicationUsers
    then UserValid (User usernameText)
    else UserInvalid
  where
    matches :: ApplicationUser -> Bool
    matches (ApplicationUser name secret) =
      name == usernameText && constantTimeEq secret passwordText

-- | Compare two secrets without leaking where they first differ. Length is still
-- compared directly, as in any standard digest comparison.
constantTimeEq :: Text -> Text -> Bool
constantTimeEq x y =
  BS.length bx == BS.length by
    && 0 == BS.foldl' (.|.) 0 (BS.pack (BS.zipWith xor bx by))
  where
    bx = Text.encodeUtf8 x
    by = Text.encodeUtf8 y

-- Stub api

runAppStubbed :: Trace IO LogMessage -> Int -> IO ()
runAppStubbed trce port = do
  let settings =
        setPort port $
          setBeforeMainLoop
            (hPutStrLn stderr ("SMASH-stubbed listening on port " ++ show port))
            defaultSettings

  runSettings settings =<< mkAppStubbed trce

mkAppStubbed :: Trace IO LogMessage -> IO Application
mkAppStubbed trce = do
  dataLayer <- createCachedPoolDataLayer Nothing

  pure $
    serveWithContext
      fullAPI
      (basicAuthServerContext stubbedApplicationUsers)
      (server $ ServerEnv trce dataLayer)

stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "user" "password"]
