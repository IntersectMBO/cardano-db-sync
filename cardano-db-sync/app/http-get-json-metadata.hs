{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db (PoolMetaHash (..), PoolUrl (..), VoteMetaHash (..), VoteUrl (..))
import qualified Cardano.Db as DB
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
  let voteType = getVoteType xs
  case cleanOpt xs of
    [url] -> runGet voteType (isItLink xs) (Text.pack url) Nothing
    [url, hash] -> runGet voteType (isItLink xs) (Text.pack url) (Just $ parseHash hash)
    _otherwise -> usageExit
  where
    parseHash :: String -> BS.ByteString
    parseHash str =
      case Base16.decode $ BSC.pack str of
        Left err -> error $ "Failed to Base16 decode hash: " ++ err
        Right bs -> bs

    cleanOpt :: [String] -> [String]
    cleanOpt ls = List.delete "ga" $ List.delete "vote" $ List.delete "drep" $ List.delete "pool" ls

    getVoteType :: [String] -> Maybe OffChainVoteType
    getVoteType ls
      | "ga" `List.elem` ls = Just GovAction
      | "drep" `List.elem` ls = Just DrepReg
      | "other" `List.elem` ls = Just Other
      | "vote" `List.elem` ls = Just Vote
      | "committee_dereg" `List.elem` ls = Just CommitteeDeReg
      | "const" `List.elem` ls = Just Constitution
      | otherwise = Nothing
    isItLink = List.elem "url"

    runGet mvtype isLink url mhsh = case mvtype of
      Just vtype
        | isLink ->
            runHttpGetVote (VoteUrl url) (VoteMetaHash <$> mhsh) (toDBOffChainVoteType vtype)
      Just vtype ->
        runGetVote url (VoteMetaHash <$> mhsh) (toDBOffChainVoteType vtype)
      _ ->
        runHttpGetPool (PoolUrl url) (PoolMetaHash <$> mhsh)

data OffChainVoteType
  = GovAction
  | DrepReg
  | Other
  | Vote
  | CommitteeDeReg
  | Constitution

toDBOffChainVoteType :: OffChainVoteType -> DB.AnchorType
toDBOffChainVoteType = \case
  GovAction -> DB.GovActionAnchor
  DrepReg -> DB.DrepAnchor
  Other -> DB.OtherAnchor
  Vote -> DB.VoteAnchor
  CommitteeDeReg -> DB.CommitteeDeRegAnchor
  Constitution -> DB.ConstitutionAnchor

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

runHttpGetVote :: VoteUrl -> Maybe VoteMetaHash -> DB.AnchorType -> IO ()
runHttpGetVote voteUrl mHash vtype =
  reportSuccess =<< runOrThrowIO (runExceptT httpGet)
  where
    httpGet :: ExceptT OffChainFetchError IO SimplifiedOffChainVoteData
    httpGet = httpGetOffChainVoteData [] voteUrl mHash vtype

    reportSuccess :: SimplifiedOffChainVoteData -> IO ()
    reportSuccess spod = do
      case sovaContentType spod of
        Nothing -> putStrLn $ orangeText "Warning: No HTTP content-type returned in HTTP response (this should be fixed)."
        Just ct ->
          if "application/json" `BSC.isInfixOf` ct
            then putStrLn $ greenText "Success"
            else putStrLn $ orangeText ("Warning: This should be 'application/json'\nContent-type: " ++ BSC.unpack ct)
      Text.putStrLn $ sovaJson spod
      print $ sovaOffChainVoteData spod

runGetVote :: Text.Text -> Maybe VoteMetaHash -> DB.AnchorType -> IO ()
runGetVote file mExpectedHash vtype = do
  respBs <- BS.readFile (Text.unpack file)
  let respLBs = fromStrict respBs
  (ocvd, val, hsh, mWarning) <- runOrThrowIO $ runExceptT $ parseAndValidateVoteData respBs respLBs mExpectedHash vtype Nothing
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
