import           Cardano.Prelude (Text)

import           Prelude

import           Control.Monad ((>=>))

import           MigrationValidations (KnownMigration (..), knownMigrations)

import           Cardano.Mock.ChainSync.Server

import           Test.Tasty

import           Test.Cardano.Db.Mock.Unit

main :: IO ()
main =
    withIOManager $
        tests >=> defaultMain

tests :: IOManager -> IO TestTree
tests iom = do
    pure $
      testGroup
        "cardano-chain-gen"
          [ unitTests iom knownMigrationsPlain
          ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
