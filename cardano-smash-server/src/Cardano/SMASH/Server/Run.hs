{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.SMASH.Server.Run where

import           Cardano.Prelude

import           Servant (Application, BasicAuthCheck (..), BasicAuthData (..),
                    BasicAuthResult (..), Context (..), serveWithContext)

import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setBeforeMainLoop, setPort)

import           Cardano.Db

import           Cardano.SMASH.Server.Api
import           Cardano.SMASH.Server.Impl
import           Cardano.SMASH.Server.PoolApi
import           Cardano.SMASH.Server.Types


runApp :: PoolApi -> Int -> IO ()
runApp dataLayer port = do
    let settings =
          setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
          defaultSettings

    runSettings settings =<< (mkApp dataLayer)

runAppStubbed :: Int -> IO ()
runAppStubbed port = do
    let settings =
          setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
          defaultSettings

    runSettings settings =<< mkAppStubbed

mkAppStubbed :: IO Application
mkAppStubbed = do
    dataLayer <- createCachedPoolApi Nothing

    return $ serveWithContext
        fullAPI
        (basicAuthServerContext stubbedApplicationUsers)
        (server dataLayer)

stubbedApplicationUsers :: ApplicationUsers
stubbedApplicationUsers = ApplicationUsers [ApplicationUser "ksaric" "cirask"]


mkApp :: PoolApi -> IO Application
mkApp dataLayer = do

    -- Ugly hack, wait 2s for migrations to run for the admin user to be created.
    -- You can always run the migrations first.
    threadDelay 2_000_000

    -- Fetch the admin users from the DB.
    let getAdminUsers = dlGetAdminUsers dataLayer
    adminUsers <- getAdminUsers

    -- This is pretty close to the top and we can't handle this.
    let adminUsers' =   case adminUsers of
                            Left err -> panic $ "Error with fetching application users! " <> show err
                            Right users -> users

    let applicationUsers = ApplicationUsers $ map convertToAppUsers adminUsers'

    return $ serveWithContext
        fullAPI
        (basicAuthServerContext applicationUsers)
        (server dataLayer)
  where
    convertToAppUsers :: AdminUser -> ApplicationUser
    convertToAppUsers (AdminUser username' password') = ApplicationUser username' password'


-- | We need to supply our handlers with the right Context.
basicAuthServerContext :: ApplicationUsers -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext applicationUsers = (authCheck applicationUsers) :. EmptyContext
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
        then (UserValid (User usernameText))
        else UserInvalid


createAdminUser :: PoolApi -> ApplicationUser -> IO (Either DBFail AdminUser)
createAdminUser dataLayer applicationUser = do
    let addAdminUser = dlAddAdminUser dataLayer
    addAdminUser applicationUser

deleteAdminUser :: PoolApi -> ApplicationUser -> IO (Either DBFail AdminUser)
deleteAdminUser dataLayer applicationUser = do
    let removeAdminUser = dlRemoveAdminUser dataLayer
    removeAdminUser applicationUser


