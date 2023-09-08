module Cardano.DbSync.Config.Conway (
  ConwayGenesisError (..),
  readConwayGenesisConfig,
  readGenesis,
) where

import Cardano.Crypto.Hash (hashWith)
import Cardano.DbSync.Config.Types
import Cardano.DbSync.Error (SyncNodeError (..))
import Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import Cardano.Ledger.Crypto (StandardCrypto ())
import Cardano.Prelude
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Prelude ()

type ExceptIO e = ExceptT e IO

data ConwayGenesisError
  = GenesisReadError !FilePath !Text
  | GenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
  | GenesisDecodeError !FilePath !Text
  deriving (Eq, Show)

readConwayGenesisConfig ::
  SyncNodeConfig ->
  ExceptIO SyncNodeError (ConwayGenesis StandardCrypto)
readConwayGenesisConfig = undefined

readGenesis ::
  GenesisFile ->
  Maybe GenesisHashShelley ->
  ExceptIO ConwayGenesisError (ConwayGenesis StandardCrypto)
readGenesis (GenesisFile file) expectedHash = do
  content <- readFile' file
  checkExpectedGenesisHash expectedHash content
  decodeGenesis (GenesisDecodeError file) content

readFile' :: FilePath -> ExceptIO ConwayGenesisError ByteString
readFile' file =
  handleIOExceptT
    (GenesisReadError file . show)
    (ByteString.readFile file)

decodeGenesis :: (Text -> ConwayGenesisError) -> ByteString -> ExceptIO ConwayGenesisError (ConwayGenesis StandardCrypto)
decodeGenesis f =
  firstExceptT (f . Text.pack)
    . hoistEither
    . Aeson.eitherDecodeStrict'

checkExpectedGenesisHash ::
  Maybe GenesisHashShelley ->
  ByteString ->
  ExceptIO ConwayGenesisError ()
checkExpectedGenesisHash Nothing _ = pure ()
checkExpectedGenesisHash (Just expected) content
  | actualHash == expected = pure ()
  | otherwise = left (GenesisHashMismatch actualHash expected)
  where
    actualHash = GenesisHashShelley $ hashWith identity content
