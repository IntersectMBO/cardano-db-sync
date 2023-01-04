
import           Cardano.Crypto.Hash
import           Control.Monad (foldM, forM)

import qualified Data.ByteString.Char8 as BS
import           Data.Char (isDigit)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)

import           Distribution.PackageDescription (extraSrcFiles)
import           Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFileEx)
import           Distribution.Verbosity (normal)

import           System.Directory (listDirectory)
import           System.FilePath (pathSeparator, takeDirectory, takeExtension, takeFileName)

import           Text.Read (readMaybe)


main :: IO ()
main = defaultMainWithHooks generateHooks
  where
    generateHooks :: UserHooks
    generateHooks =
      simple { buildHook   = \p l h f -> generate l >> buildHook simple p l h f
             , haddockHook = \p l h f -> generate l >> haddockHook simple p l h f
             , replHook    = \p l h f args -> generate l >> replHook simple p l h f args
             }

    simple = simpleUserHooks

    generate :: LocalBuildInfo -> IO ()
    generate locInfo =
      generateMigrations locInfo "schema" (autogenPackageModulesDir locInfo)


generateMigrations :: LocalBuildInfo -> FilePath -> FilePath -> IO ()
generateMigrations locInfo srcDir outDir = do
    -- Given the migrationDirectories (usually just a single directory)
    collectMigrationSql <-
      forM migrationDirectories $ \path -> do
        fmap (\x -> path <> [pathSeparator] <> x )
          . filter isOfficialMigrationFile
          <$> listDirectory path

    createDirectoryIfMissingVerbose normal True "gen"
    sqls <- forM (concat collectMigrationSql) build
    buildMigrationModule $ List.sortOn snd sqls
  where

    -- Find directories listed in cabal file with *.sql extensions
    -- eg schema/*.sql
    --
    migrationDirectories :: [FilePath]
    migrationDirectories = takeDirectory <$>
      filter ((== ".sql") . takeExtension)
      (extraSrcFiles $ localPkgDescr locInfo)

    hashAs :: ByteString -> Hash Blake2b_256 ByteString
    hashAs = hashWith id

    build :: FilePath -> IO (String, FilePath)
    build filepath = do
      file <- BS.readFile filepath
      pure (hashToStringAsHex . hashAs $ file, takeFileName filepath)


    buildMigrationModule :: [(String, FilePath)] -> IO ()
    buildMigrationModule sqls =
      let buildLine (hashedFile, filepath) = "KnownMigration \"" ++ hashedFile ++ "\" \"" ++ filepath ++ "\"" in

      rewriteFileEx normal "gen/MigrationValidations.hs" $
        unlines
          [ "{-# LANGUAGE OverloadedStrings #-}"
          , "module MigrationValidations"
          , "  ( KnownMigration (..)"
          , "  , knownMigrations"
          , "  ) where"
          , ""
          , "import Prelude"
          , "import Data.Text"
          , ""
          , "data KnownMigration = KnownMigration"
          , "  { hash :: !Text"
          , "  , filepath :: !Text"
          , "  } deriving (Eq, Show)"
          , ""
          , "knownMigrations :: [KnownMigration]"
          , "knownMigrations = "
          , "  [ " ++ (List.intercalate "\n  , " . fmap buildLine $ sqls)
          , "  ]"
          ]

-- We only care about "official" migrations, with a `mvStage` version >=1 and <= 4.
isOfficialMigrationFile :: FilePath -> Bool
isOfficialMigrationFile fn =
    let stage = readStageFromFilename (takeFileName fn)
     in takeExtension fn == ".sql" && stage >= 1 && stage <= 4
  where
    -- Reimplement part of `parseMigrationVersionFromFile` because that function is not avaliable
    -- here. Defaults to a stage value of `0`.
    readStageFromFilename :: String -> Int
    readStageFromFilename str =
      case takeWhile isDigit . drop 1 $ dropWhile (/= '-') str of
        stage -> fromMaybe 0 $ readMaybe stage
