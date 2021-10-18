{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude
import           Prelude (read)

import           Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt

import           Cardano.Config.Git.Rev (gitRev)

import           Cardano.SMASH.Server.Config
import           Cardano.SMASH.Server.Run

import           Data.String (String)
import qualified Data.Text as Text
import           Data.Version (showVersion)

import           Paths_cardano_smash_server (version)

import           System.Info (arch, compilerName, compilerVersion, os)

main :: IO ()
main = do
  cmd <- Opt.execParser opts
  case cmd of
    CmdVersion -> runVersionCommand
    CmdRun params -> do
      serverConfig <- paramsToConfig params
      runSmashServer serverConfig

data SmashServerCommand
  = CmdRun !SmashServerParams
  | CmdVersion

opts :: ParserInfo SmashServerCommand
opts =
  Opt.info (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano Smash web server."
    )

pCommandLine :: Parser SmashServerCommand
pCommandLine =
  asum
    [ pVersionCommand
    , CmdRun <$> pSmashServerParams
    ]

pSmashServerParams :: Parser SmashServerParams
pSmashServerParams =
  SmashServerParams
    <$> asum [pSmashPort, pure defaultSmashPort]
    <*> pSmashConfig
    <*> optional pSmashUserFile

pSmashUserFile :: Parser FilePath
pSmashUserFile =
  Opt.strOption
    ( Opt.long "admins"
    <> Opt.help (  "Path to the csv file containing the credentials of smash server admin users."
                ++ "It should include lines in the form of username,password")
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pSmashConfig :: Parser FilePath
pSmashConfig =
  Opt.strOption
    ( Opt.long "config"
    <> Opt.help "Path to the smash or db-sync config file. This is used to parse logging config."
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pSmashPort :: Parser Int
pSmashPort =
  read <$> Opt.strOption
    ( Opt.long "smash-port"
    <> Opt.help "Port for the smash web app server. Default is 3100."
    <> Opt.completer (Opt.bashCompleter "port")
    <> Opt.metavar "PORT"
    )

pVersionCommand :: Parser SmashServerCommand
pVersionCommand =
  asum
    [ Opt.subparser
        ( mconcat
          [ command' "version" "Show the program version" (pure CmdVersion) ]
        )
    , Opt.flag' CmdVersion
        (  Opt.long "version"
        <> Opt.help "Show the program version"
        <> Opt.hidden
        )
    ]

command' :: String -> String -> Parser a -> Opt.Mod Opt.CommandFields a
command' c descr p =
  Opt.command c
    $ Opt.info (p <**> Opt.helper)
    $ mconcat [ Opt.progDesc descr ]

runVersionCommand :: IO ()
runVersionCommand = do
    liftIO . putTextLn $ mconcat
                [ "cardano-smash-server ", renderVersion version
                , " - ", Text.pack os, "-", Text.pack arch
                , " - ", Text.pack compilerName, "-", renderVersion compilerVersion
                , "\ngit revision ", gitRev
                ]
  where
    renderVersion = Text.pack . showVersion
