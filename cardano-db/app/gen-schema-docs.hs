{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Db (schemaDocs)

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Version (showVersion)

import           Database.Persist.Documentation (markdownTableRenderer, render)

import           Paths_cardano_db (version)

import           System.Environment (getArgs, getProgName)
import           System.Exit (ExitCode (..))
import           System.IO (IOMode (..), withFile)
import           System.Process (readProcessWithExitCode)


main :: IO ()
main = do
    args <- getArgs
    gitBranch <- readGitBranch
    case args of
      [] -> do
              Text.putStrLn $ docHeader gitBranch
              Text.putStrLn docBody
      [file] -> withFile file WriteMode $ \ h -> do
                  Text.hPutStrLn h $ docHeader gitBranch
                  Text.hPutStrLn h docBody
      _otherwise -> usageExit
  where
    usageExit :: IO ()
    usageExit = do
      pname <- getProgName
      putStrLn $ mconcat
            [ "\nUsage: ", pname, " <filename>\n\n"
            , "If no filename is provided, the output will be printed to stdout.\n"
            ]


docHeader :: Text -> Text
docHeader branchName =
  mconcat
    [ "# Schema Documentation for cardano-db-sync\n\n"
    , "Schema version: ", Text.pack (showVersion version)
    , if "release" `Text.isPrefixOf` branchName
        then mempty
        else mconcat
              [ " (from branch **", branchName
              , "** which may not accurately reflect the version number)"
              ]
    , "\n"
    ]

docBody :: Text
docBody =
  Text.replace "ID:" "Id:"
    . Text.replace "#" "###"
    $ render markdownTableRenderer schemaDocs


readGitBranch :: IO Text
readGitBranch = do
  (exitCode, output, _) <- readProcessWithExitCode "git" ["branch", "--show-current"] ""
  pure $ case exitCode of
            ExitSuccess -> Text.strip (Text.pack output)
            ExitFailure _ -> "unknown"

