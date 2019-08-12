
import qualified Cardano.Node.CLI as Node

import           Cardano.Prelude

import qualified Cardano.Shell.Features.Logging as Shell
import           Cardano.Shell.Lib (CardanoApplication (..), ApplicationEnvironment (..))
import qualified Cardano.Shell.Lib as Shell
import qualified Cardano.Shell.Presets as Shell

import           Explorer.Node (ExplorerNodeParams (..), CLI (..), NodeLayer (..), initializeAllFeatures)

import           Options.Applicative (Parser, ParserInfo, execParser, info, fullDesc, progDesc, helper, header)

main :: IO ()
main = do
    cardanoEnvironment <- Shell.initializeCardanoEnvironment
    logConfig <- execParser opts
    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig Shell.mainnetConfiguration cardanoEnvironment
    Shell.runCardanoApplicationWithFeatures Development cardanoFeatures (cardanoApplication nodeLayer)
  where
    cardanoApplication :: NodeLayer -> CardanoApplication
    cardanoApplication layer = CardanoApplication $ (nlRunNode layer)



opts :: ParserInfo ExplorerNodeParams
opts =
  info (pCommandLine <**> helper)
    ( fullDesc
    <> progDesc "Cardano wallet node."
    <> header "Demo client to run."
    )

pCommandLine :: Parser ExplorerNodeParams
pCommandLine =
  ExplorerNodeParams
    <$> Shell.loggingParser
    <*> (CLI <$> Node.parseProtocol <*> Node.parseCommonCLI)

