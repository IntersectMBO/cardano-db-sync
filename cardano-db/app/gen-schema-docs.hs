{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db (schemaDocs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Database.Persist.Documentation (markdownTableRenderer, render)
import Paths_cardano_db (version)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..))
import System.IO (IOMode (..), withFile)
import System.Process (readProcessWithExitCode)

-- There are a number of reasons why we generate schema documentation like this.
--   * Having the schema docs with the schema definition in the Haskell file means that the schema
--     documentation library will error out if a field is deleted from the schema but not the
--     documentation. If a field is added but not documented, the documentation library will still
--     add it to the generated documentation but with a blank comment.
--   * Schema documentation can be generated at any time, but the updated `doc/schema.md` file
--     should only be committed as part of the release process, so that documentation in the Github
--     matches the schema version people are likley to be running in the field.

main :: IO ()
main = do
  args <- getArgs
  gitBranch <- readGitBranch
  case args of
    [] -> do
      Text.putStrLn $ docHeader gitBranch
      Text.putStrLn docBody
    [file] -> withFile file WriteMode $ \h -> do
      Text.hPutStrLn h $ docHeader gitBranch
      Text.hPutStrLn h docBody
    _otherwise -> usageExit
  where
    usageExit :: IO ()
    usageExit = do
      pname <- getProgName
      putStrLn $
        mconcat
          [ "\nUsage: "
          , pname
          , " <filename>\n\n"
          , "If no filename is provided, the output will be printed to stdout.\n"
          ]

docHeader :: Text -> Text
docHeader branchName =
  mconcat
    [ "# Schema Documentation for cardano-db-sync\n\n"
    , "Schema version: "
    , Text.pack (showVersion version)
    , if "release" `Text.isPrefixOf` branchName
        then mempty
        else
          mconcat
            [ " (from branch **"
            , branchName
            , "** which may not accurately reflect the version number)"
            ]
    , "\n"
    , "**Note:** This file is auto-generated from the documentation in cardano-db/src/Cardano/Db/Schema.hs\
      \ by the command `cabal run -- gen-schema-docs doc/schema.md`. This document should only be updated\
      \ during the release process and updated on the release branch."
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
