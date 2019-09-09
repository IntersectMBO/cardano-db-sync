
import qualified Cardano.Config.CommonCLI as Config

import           Cardano.Prelude

import qualified Cardano.Config.Presets as Config
import qualified Cardano.Common.Parsers as Config
import           Cardano.Config.Types (CardanoEnvironment(NoEnvironment))
import           Cardano.Shell.Types (CardanoApplication (..))
import qualified Cardano.Shell.Lib as Shell

import           Explorer.DB (MigrationDir (..))
import           Explorer.Node (ExplorerNodeParams (..), NodeLayer (..), initializeAllFeatures)

import           Options.Applicative (Parser, ParserInfo, completer, bashCompleter, help, long, strOption)
import qualified Options.Applicative as Opt

main :: IO ()
main = do
    logConfig <- Opt.execParser opts
    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig Config.mainnetConfiguration NoEnvironment
    Shell.runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)
  where
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication layer = CardanoApplication $ (nlRunNode layer)



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
    <*> Config.parseCommonCLI
    <*> parseSocketPath
    <*> pMigrationDir

-- TODO, another PR is adding similar to another repo, switch over to it
parseSocketPath :: Parser FilePath
parseSocketPath = strOption (long "socket-path" <> help "path to a cardano-node socket" <> completer (bashCompleter "file"))

pMigrationDir :: Parser MigrationDir
pMigrationDir =
  MigrationDir <$> Opt.strOption
    (  Opt.long "schema-dir"
    <> Opt.help "The directory containing the migrations."
    <> Opt.completer (Opt.bashCompleter "directory")
    )
