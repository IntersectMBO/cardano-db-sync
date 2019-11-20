{-# LANGUAGE NoImplicitPrelude #-}

import           Cardano.Prelude

import qualified Data.Text as Text

import           Explorer.DB (MigrationDir (..))
import           Explorer.Node (ConfigFile (..), ExplorerNodeParams (..), GenesisFile (..),
                    GenesisHash (..), NetworkName (..), SocketPath (..), runExplorer)

import           Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt

main :: IO ()
main =
  runExplorer =<< Opt.execParser opts

-- -------------------------------------------------------------------------------------------------

opts :: ParserInfo ExplorerNodeParams
opts =
  Opt.info (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano explorer database node."
    )

pCommandLine :: Parser ExplorerNodeParams
pCommandLine =
  ExplorerNodeParams
    <$> pConfigFile
    <*> pNetwork
    <*> pGenesisHash
    <*> pGenesisFile
    <*> pSocketPath
    <*> pMigrationDir

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile <$> Opt.strOption
    ( Opt.long "config"
    <> Opt.help "Path to the explorer node config file"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pGenesisFile :: Parser GenesisFile
pGenesisFile =
  GenesisFile <$> Opt.strOption
    ( Opt.long "genesis-file"
    <> Opt.help "Path to the genesis JSON file"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pGenesisHash :: Parser GenesisHash
pGenesisHash =
  GenesisHash . Text.pack <$> Opt.strOption
    ( Opt.long "genesis-hash"
    <> Opt.help "The hash of the genesis data"
    <> Opt.metavar "GENESIS-HASH"
    )

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir <$> Opt.strOption
    (  Opt.long "schema-dir"
    <> Opt.help "The directory containing the migrations."
    <> Opt.completer (Opt.bashCompleter "directory")
    <> Opt.metavar "FILEPATH"
    )

pNetwork :: Parser NetworkName
pNetwork =
  NetworkName . Text.pack <$> Opt.strOption
    ( Opt.long "network"
    <> Opt.help "The network name eg mainnet/testnet/staging etc"
    <> Opt.metavar "NAME"
    )
pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath <$> Opt.strOption
    ( Opt.long "socket-path"
    <> Opt.help "Path to a cardano-node socket"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )
