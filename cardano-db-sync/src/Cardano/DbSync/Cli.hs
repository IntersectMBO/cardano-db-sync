{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.DbSync.Cli (
    command'
  , gitRev
  , opts
  , pCommandLine
  , pConfigFile
  , pLedgerStateDir
  , pMigrationDir
  , pRunDbSyncNode
  , pSlotNo
  , pSocketPath
  , pVersionCommand
  ) where

import           Cardano.DbSync.TH (gitRevFromGit)
import           Cardano.Prelude
import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.Sync.Config.Types (ConfigFile (..), LedgerStateDir (..), MigrationDir (..),
                   SocketPath (..), SyncCommand (..), SyncNodeParams (..))

import           Data.FileEmbed (dummySpaceWith)
import           Data.String (String)
import qualified Data.Text as Text

import           Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt

opts :: ParserInfo SyncCommand
opts =
  Opt.info (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano PostgreSQL sync node."
    )

pCommandLine :: Parser SyncCommand
pCommandLine =
  asum
    [ pVersionCommand
    , CmdRun <$> pRunDbSyncNode
    ]

pRunDbSyncNode :: Parser SyncNodeParams
pRunDbSyncNode =
  SyncNodeParams
    <$> pConfigFile
    <*> pSocketPath
    <*> pLedgerStateDir
    <*> pMigrationDir
    <*> optional pSlotNo

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile <$> Opt.strOption
    ( Opt.long "config"
    <> Opt.help "Path to the db-sync node config file"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pLedgerStateDir :: Parser LedgerStateDir
pLedgerStateDir =
  LedgerStateDir <$> Opt.strOption
    (  Opt.long "state-dir"
    <> Opt.help "The directory for persistung ledger state."
    <> Opt.completer (Opt.bashCompleter "directory")
    <> Opt.metavar "FILEPATH"
    )

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir <$> Opt.strOption
    (  Opt.long "schema-dir"
    <> Opt.help "The directory containing the migrations."
    <> Opt.completer (Opt.bashCompleter "directory")
    <> Opt.metavar "FILEPATH"
    )

pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath <$> Opt.strOption
    ( Opt.long "socket-path"
    <> Opt.help "Path to a cardano-node socket"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pSlotNo :: Parser SlotNo
pSlotNo =
  SlotNo <$> Opt.option Opt.auto
    (  Opt.long "rollback-to-slot"
    <> Opt.help "Force a rollback to the specified slot (mainly for testing and debugging)."
    <> Opt.metavar "WORD"
    )

pVersionCommand :: Parser SyncCommand
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

{-# NOINLINE gitRev #-}
gitRev :: Text
gitRev | gitRevEmbed /= zeroRev = gitRevEmbed
       | Text.null fromGit         = zeroRev
       | otherwise              = fromGit
    where
        -- Git revision embedded after compilation using
        -- Data.FileEmbed.injectWith. If nothing has been injected,
        -- this will be filled with 0 characters.
        gitRevEmbed :: Text
        gitRevEmbed = decodeUtf8 $(dummySpaceWith "gitrev" 40)

        -- Git revision found during compilation by running git. If
        -- git could not be run, then this will be empty.
#if defined(arm_HOST_ARCH)
        -- cross compiling to arm fails; due to a linker bug
        fromGit = ""
#else
        fromGit = Text.strip (Text.pack $(gitRevFromGit))
#endif

zeroRev :: Text
zeroRev = "0000000000000000000000000000000000000000"
