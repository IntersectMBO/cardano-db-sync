{-# LANGUAGE NoImplicitPrelude #-}

import           Cardano.Prelude

import qualified Cardano.Common.Parsers as Config
import           Cardano.Shell.Types (CardanoApplication (..))
import qualified Cardano.Shell.Lib as Shell

import qualified Data.Text as Text

import           Explorer.DB (MigrationDir (..))
import           Explorer.Node (ExplorerNodeParams (..), NodeLayer (..), SocketPath (..),
                    initializeAllFeatures)

import           Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as Opt

main :: IO ()
main = do
    logConfig <- Opt.execParser opts
    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig
    Shell.runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)
  where
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication = CardanoApplication . nlRunNode



opts :: ParserInfo ExplorerNodeParams
opts =
  Opt.info (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano explorer database node."
    )

pCommandLine :: Parser ExplorerNodeParams
pCommandLine =
  ExplorerNodeParams
    <$> Config.loggingParser
    <*> pGenesisHash
    <*> pSocketPath
    <*> pMigrationDir

pGenesisHash :: Parser Text
pGenesisHash =
  Text.pack <$> Opt.strOption
    ( Opt.long "genesis-hash"
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

pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath <$> Opt.strOption
    ( Opt.long "socket-path"
    <> Opt.help "path to a cardano-node socket"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )
