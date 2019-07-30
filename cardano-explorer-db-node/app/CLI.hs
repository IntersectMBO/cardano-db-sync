{-# LANGUAGE NoImplicitPrelude   #-}

module CLI (ArgParser(ArgParser), CLI(CLI,cliProtocol,cliCommon), opts) where

import qualified Cardano.Node.CLI
import           Cardano.Shell.Features.Logging                      (LoggingCLIArguments,
                                                                      loggingParser)
import           Options.Applicative                                 (Parser, ParserInfo, info, fullDesc, progDesc, helper, header)
import           Cardano.Prelude                                     hiding (atomically, option)

data CLI = CLI {
  cliProtocol     :: Cardano.Node.CLI.Protocol,
  cliCommon       :: Cardano.Node.CLI.CommonCLI
  }

-- | The product type of all command line arguments
data ArgParser = ArgParser !LoggingCLIArguments !CLI

parseCLI :: Parser CLI
parseCLI = CLI
    <$> Cardano.Node.CLI.parseProtocol
    <*> Cardano.Node.CLI.parseCommonCLI

opts :: ParserInfo ArgParser
opts = info (commandLineParser <**> helper)
  ( fullDesc
  <> progDesc "Cardano wallet node."
  <> header "Demo client to run.")

-- | The product parser for all the CLI arguments.
--
commandLineParser :: Parser ArgParser
commandLineParser = ArgParser <$> loggingParser <*> parseCLI
