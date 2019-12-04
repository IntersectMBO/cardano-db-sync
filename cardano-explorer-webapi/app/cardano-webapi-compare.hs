{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async (concurrently)

import           Data.Algorithm.Diff (Diff, PolyDiff (..), getDiff)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.Text.ANSI (green, red)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

import           Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

import           System.Environment (getArgs, getProgName)


-- A quick and dirty tool to submit the same query to the old and new cardano webapis
-- and display the differences between the two.

main :: IO ()
main = do
  uri <- getArgs
  case uri of
    [path] -> compareOutputs path
    _ -> usageError


usageError :: IO ()
usageError = do
  pn <- getProgName
  mapM_ putStrLn
    [ "Usage: \n"
    , "  " ++ pn ++ " <url>\n"
    , "where <url> is something like '/api/blocks/pages'"
    ]

compareOutputs :: String -> IO ()
compareOutputs path = do
  (old, new) <- concurrently
                    (httpLines $ "http://cardanoexplorer.com" ++ path)
                    (httpLines $ "http://127.0.0.1:8100" ++ path)
  let diff = getDiff old new
  if any isDiff diff
    then reportDiff diff
    else do
      Text.putStrLn $ green "Outputs are the same:"
      mapM_ Text.putStrLn old

reportDiff :: [Diff Text] -> IO ()
reportDiff xs = do
    Text.putStrLn $ Text.concat [ "Explorer web API differences: ", green "old", " vs ", red "new", "." ]
    mapM_ display xs
  where
    display :: Diff Text -> IO ()
    display db =
      case db of
        First a -> Text.putStrLn $ green a
        Second a -> Text.putStrLn $ red a
        Both a _ -> Text.putStrLn a

isDiff :: Diff a -> Bool
isDiff d =
  case d of
    First _ -> True
    Second _ -> True
    Both _ _ -> False

httpLines :: String -> IO [Text]
httpLines url = do
  req <- parseRequest url
  -- Remove whitespace, convert to Text and split into lines on commas.
  Text.lines . Text.replace "," "\n,"
    . Text.decodeUtf8 . BS.filter (not . isSpace)
    . getResponseBody <$> httpBS req
