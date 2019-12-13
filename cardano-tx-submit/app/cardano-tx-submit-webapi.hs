import           Cardano.TxSubmit (ConfigFile (..), GenesisFile (..), SocketPath (..),
                    TxSubmitNodeParams (..), TxSubmitPort (..), runTxSubmitWebapi)

import           Options.Applicative (Parser, ParserInfo, (<**>))
import qualified Options.Applicative as Opt

main :: IO ()
main =
  runTxSubmitWebapi =<< Opt.execParser opts

-- -------------------------------------------------------------------------------------------------

opts :: ParserInfo TxSubmitNodeParams
opts =
  Opt.info (pCommandLine <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano transaction submission webapi."
    )

pCommandLine :: Parser TxSubmitNodeParams
pCommandLine =
  TxSubmitNodeParams
    <$> pConfigFile
    <*> pGenesisFile
    <*> pSocketPath
    <*> pWebPort


pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile <$> Opt.strOption
    ( Opt.long "config"
    <> Opt.help "Path to the tx-submit webapi config file"
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

pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath <$> Opt.strOption
    ( Opt.long "socket-path"
    <> Opt.help "Path to a cardano-node socket"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pWebPort :: Parser TxSubmitPort
pWebPort =
  TxSubmitPort . read <$> Opt.strOption
    ( Opt.long "port"
    <> Opt.help "The port the webapi should listen on"
    <> Opt.metavar "PORT"
    )
