{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..))
import Cardano.DbSync.Error (bsBase16Encode, runOrThrowIO)
import Cardano.DbSync.OffChain.Http
import Cardano.DbSync.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy (fromStrict)
import qualified Data.List as List
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

{-# ANN module ("HLint: ignore Avoid restricted qualification" :: Text.Text) #-}

main :: IO ()
main = do
  xs <- getArgs
  case cleanOpt xs of
    [url] -> runGet (isItVote xs) (isItGa xs) (isItLink xs) (Text.pack url) Nothing
    [url, hash] -> runGet (isItVote xs) (isItGa xs) (isItLink xs) (Text.pack url) (Just $ parseHash hash)
    _otherwise -> usageExit
  where
    parseHash :: String -> BS.ByteString
    parseHash str =
      case Base16.decode $ BSC.pack str of
        Left err -> error $ "Failed to Base16 decode hash: " ++ err
        Right bs -> bs

    cleanOpt :: [String] -> [String]
    cleanOpt ls = List.delete "ga" $ List.delete "vote" $ List.delete "pool" ls

    isItVote ls = List.elem "vote" ls || isItGa ls
    isItGa = List.elem "ga"
    isItLink = List.elem "url"

    runGet isVote isGa isLink url mhsh
      | isVote && isLink =
          runHttpGetVote (VoteUrl url) (VoteMetaHash <$> mhsh) isGa
      | isVote && not isLink =
          runGetVote url (VoteMetaHash <$> mhsh) isGa
      | otherwise =
          runHttpGetPool (PoolUrl url) (PoolMetaHash <$> mhsh)

-- -------------------------------------------------------------------------------------------------

usageExit :: IO ()
usageExit = do
  name <- getProgName
  mapM_
    putStrLn
    [ "\nUsage:"
    , "    " ++ name ++ "<metadata url>"
    , "    " ++ name ++ "<metadata url> <metadata hash in hex>"
    , "with options [vote], [pool], [ga]"
    , "A debug/test program to debug the offchain metadata fetch mechanism.\n"
    ]
  exitFailure

-- -------------------------------------------------------------------------------------------------

runHttpGetPool :: PoolUrl -> Maybe PoolMetaHash -> IO ()
runHttpGetPool poolUrl mHash =
  reportSuccess =<< runOrThrowIO (runExceptT httpGet)
  where
    httpGet :: ExceptT OffChainFetchError IO SimplifiedOffChainPoolData
    httpGet = do
      request <- parseOffChainUrl $ OffChainPoolUrl poolUrl
      manager <- liftIO $ Http.newManager tlsManagerSettings
      httpGetOffChainPoolData manager request poolUrl mHash

    reportSuccess :: SimplifiedOffChainPoolData -> IO ()
    reportSuccess spod = do
      case spodContentType spod of
        Nothing -> putStrLn $ orangeText "Warning: No HTTP content-type returned in HTTP response (this should be fixed)."
        Just ct ->
          if "application/json" `BSC.isInfixOf` ct
            then putStrLn $ greenText "Success"
            else putStrLn $ orangeText ("Warning: This should be 'application/json'\nContent-type: " ++ BSC.unpack ct)
      Text.putStrLn $ spodJson spod

runHttpGetVote :: VoteUrl -> Maybe VoteMetaHash -> Bool -> IO ()
runHttpGetVote voteUrl mHash isGa =
  reportSuccess =<< runOrThrowIO (runExceptT httpGet)
  where
    httpGet :: ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
    httpGet = do
      request <- parseOffChainUrl $ OffChainVoteUrl voteUrl
      manager <- liftIO $ Http.newManager tlsManagerSettings
      httpGetOffChainVoteData manager request voteUrl mHash isGa

    reportSuccess :: SimplifiedOffChainVoteData -> IO ()
    reportSuccess spod = do
      case sovaContentType spod of
        Nothing -> putStrLn $ orangeText "Warning: No HTTP content-type returned in HTTP response (this should be fixed)."
        Just ct ->
          if "application/json" `BSC.isInfixOf` ct
            then putStrLn $ greenText "Success"
            else putStrLn $ orangeText ("Warning: This should be 'application/json'\nContent-type: " ++ BSC.unpack ct)
      Text.putStrLn $ sovaJson spod

runGetVote :: Text.Text -> Maybe VoteMetaHash -> Bool -> IO ()
runGetVote file mExpectedHash isGa = do
  respBs <- BS.readFile (Text.unpack file)
  let respLBs = fromStrict respBs
  (ocvd, val, hsh, mWarning) <- runOrThrowIO $ runExceptT $ parseAndValidateVoteData respBs respLBs mExpectedHash isGa Nothing
  print ocvd
  print val
  print $ bsBase16Encode hsh
  print mWarning

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
