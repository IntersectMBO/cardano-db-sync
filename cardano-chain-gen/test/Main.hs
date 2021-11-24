import           Cardano.Prelude (Text)

import           Test.Tasty
import           Prelude

import           MigrationValidations (KnownMigration (..), knownMigrations)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests = do
    return $
      testGroup
        "cardano-chain-gen"
          [
          ]
  where
    knownMigrationsPlain :: [(Text, Text)]
    knownMigrationsPlain = (\x -> (hash x, filepath x)) <$> knownMigrations
