{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db (PoolMetaHash (..), PoolUrl (..))
import Cardano.DbSync (
  FetchError (..),
  SimplifiedPoolOfflineData (..),
  httpGetPoolOfflineData,
  parsePoolUrl,
 )
import Cardano.DbSync.Error (runOrThrowIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [url] -> runHttpGet (PoolUrl $ Text.pack url) Nothing
    [url, hash] -> runHttpGet (PoolUrl $ Text.pack url) (Just $ parseHash hash)
    _otherwise -> usageExit
  where
    parseHash :: String -> PoolMetaHash
    parseHash str =
      case Base16.decode $ BS.pack str of
        Left err -> error $ "Failed to Base16 decode hash: " ++ err
        Right bs -> PoolMetaHash bs

-- -------------------------------------------------------------------------------------------------

usageExit :: IO ()
usageExit = do
  name <- getProgName
  mapM_
    putStrLn
    [ "\nUsage:"
    , "    " ++ name ++ "<pool metadata utl>"
    , "    " ++ name ++ "<pool metadata utl> <metadata hash in hex>"
    , ""
    , "A debug/test program to debug the pool offline metadata fetch mechanism.\n"
    ]
  exitFailure

-- -------------------------------------------------------------------------------------------------

runHttpGet :: PoolUrl -> Maybe PoolMetaHash -> IO ()
runHttpGet poolUrl mHash =
  reportSuccess =<< runOrThrowIO (runExceptT httpGet)
  where
    httpGet :: ExceptT FetchError IO SimplifiedPoolOfflineData
    httpGet = do
      request <- parsePoolUrl poolUrl
      manager <- liftIO $ Http.newManager tlsManagerSettings
      httpGetPoolOfflineData manager request poolUrl mHash

    reportSuccess :: SimplifiedPoolOfflineData -> IO ()
    reportSuccess spod = do
      case spodContentType spod of
        Nothing -> putStrLn $ orangeText "Warning: No HTTP content-type returned in HTTP response (this should be fixed)."
        Just ct ->
          if "application/json" `BS.isInfixOf` ct
            then putStrLn $ greenText "Success"
            else putStrLn $ orangeText ("Warning: This should be 'application/json'\nContent-type: " ++ BS.unpack ct)
      Text.putStrLn $ spodJson spod

-- ------------------------------------------------------------------------------------------------

codeGreen :: String
codeGreen = setSGRCode [SetColor Foreground Vivid Green]

codeYellow :: String
codeYellow = setSGRCode [SetColor Foreground Vivid Yellow]

codeReset :: String
codeReset = setSGRCode [Reset]

greenText :: String -> String
greenText s = codeGreen ++ s ++ codeReset

orangeText :: String -> String
orangeText s = codeYellow ++ s ++ codeReset
