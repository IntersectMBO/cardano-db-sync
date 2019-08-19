
import qualified Cardano.Node.CLI as Node

import           Cardano.Prelude

import qualified Cardano.Shell.Features.Logging as Shell
import           Cardano.Shell.Lib (CardanoApplication (..), ApplicationEnvironment (..))
import qualified Cardano.Shell.Lib as Shell
import qualified Cardano.Shell.Presets as Shell

import           Explorer.Node (ExplorerNodeParams (..), NodeLayer (..), initializeAllFeatures)

import           Options.Applicative (Parser, ParserInfo, completer, bashCompleter, help, long, strOption)
import qualified Options.Applicative as Opt

main :: IO ()
main = do
    cardanoEnvironment <- Shell.initializeCardanoEnvironment
    logConfig <- Opt.execParser opts
    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig Shell.mainnetConfiguration cardanoEnvironment
    Shell.runCardanoApplicationWithFeatures Development cardanoFeatures (cardanoApplication nodeLayer)
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
    <$> Shell.loggingParser
    <*> Node.parseCommonCLI
    <*> parseSocketPath

-- TODO, another PR is adding similar to another repo, switch over to it
parseSocketPath :: Parser FilePath
parseSocketPath = strOption (long "socket-path" <> help "path to a cardano-node socket" <> completer (bashCompleter "file"))
