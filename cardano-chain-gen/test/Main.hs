import           Cardano.Prelude (Text)


import           Test.Tasty
import           Prelude

import           MigrationValidations (KnownMigration (..), knownMigrations)

import           Cardano.Mock.ChainSync.Server

import           Test.Cardano.Db.Mock.Unit

main :: IO ()
main =
    withIOManager $ \iom ->
        tests iom >>= defaultMain

tests :: IOManager -> IO TestTree
tests iom = do
    return $
      testGroup
        "cardano-chain-gen"
          [ unitTests iom knownMigrationsPlain
          ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
