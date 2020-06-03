import           Control.Monad (when)

import           Data.Maybe (isNothing)

import           Test.Tasty (defaultMain, testGroup)

import qualified Test.IO.Cardano.DbSync.BlockSync

import           System.Environment (lookupEnv, setEnv)
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>))

main :: IO ()
main = do
  -- In case we do use an IO test.
  mPgPassFile <- lookupEnv "PGPASSFILE"
  when (isNothing mPgPassFile) $ do
    currentDir <- getCurrentDirectory
    setEnv "PGPASSFILE" (currentDir </> "../config/pgpass")

  defaultMain $
    testGroup "BlockSync"
      [ Test.IO.Cardano.DbSync.BlockSync.tests
      ]

