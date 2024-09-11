{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db (gitRev)
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlpha, isSpace)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text.IO as Text
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types (
  Color (..),
  ColorIntensity (..),
  ConsoleLayer (..),
  SGR (..),
 )
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath (joinPath, splitPath)

main :: IO ()
main = do
  topLevel <- joinPath . List.init . splitPath <$> getCurrentDirectory
  -- This is here to force `cardano-db` as a dependency, so that changes in `cardano-db` will force
  -- this test to be recompiled.
  Text.putStrLn $ mconcat ["Git Revision: ", gitRev]
  putStrLn $ "Running in directory: " ++ topLevel
  setCurrentDirectory topLevel
  tables <- findTablesWithBlockNo
  deletes <- findTablesWithDelete
  let difference = tables List.\\ deletes
  if null difference
    then putStrLn $ greenText "Good, no missing delete/rollback statements."
    else do
      putStrLn $ redText ("Tables missing a delete statement: " ++ show difference)
      exitFailure

-- -------------------------------------------------------------------------------------------------
-- Functions to deal with the schema definition.

findTablesWithBlockNo :: IO [ByteString]
findTablesWithBlockNo = do
  xs <- mapMaybe removeCommentsAndEmpty . getSchema <$> BS.readFile "cardano-db/src/Cardano/Db/Schema/BaseSchema.hs"
  when (length xs < 10) $
    error $
      "Expected at least 10 lines of schema definition, but got only " ++ show (length xs)
  pure . map tdName . filter hasBlockNoColumn $ splitTableDefs xs
  where
    -- Extract just the schema defintion of the file.
    getSchema :: ByteString -> [ByteString]
    getSchema =
      List.drop 1
        . List.takeWhile (\x -> not ("|]" `BS.isInfixOf` x))
        . List.dropWhile (\x -> not ("[persistLowerCase|" `BS.isInfixOf` x))
        . BS.lines

data TableDef = TableDef
  { tdName :: ByteString
  , tdColumns :: [ByteString]
  }
  deriving (Show)

splitTableDefs :: [ByteString] -> [TableDef]
splitTableDefs =
  List.sortOn tdName . recurse
  where
    recurse :: [ByteString] -> [TableDef]
    recurse [] = []
    recurse (x : xs) =
      let (h, t) = List.break isTableName xs
       in TableDef (BS.strip x) h : recurse t

    -- Return True if the line is a table name (two leading spaces).
    isTableName :: ByteString -> Bool
    isTableName bs = BS.length bs >= 3 && (not . isSpace . BS.head $ BS.drop 2 bs)

hasBlockNoColumn :: TableDef -> Bool
hasBlockNoColumn =
  List.any isBlockNo . tdColumns
  where
    isBlockNo :: ByteString -> Bool
    isBlockNo bs = "blockNo" `BS.isInfixOf` bs && "Int64" `BS.isInfixOf` bs

-- -------------------------------------------------------------------------------------------------
-- Functions to deal with the delete/rollback code.

findTablesWithDelete :: IO [ByteString]
findTablesWithDelete =
  List.sort
    . mapMaybe getTableName
    . mapMaybe removeCommentsAndEmpty
    . getDeleteAfterBlockNo
    <$> BS.readFile "cardano-db/src/Cardano/Db/Operations/Delete.hs"
  where
    getDeleteAfterBlockNo :: ByteString -> [ByteString]
    getDeleteAfterBlockNo =
      List.takeWhile (not . isNewFunction)
        . List.drop 2
        . List.dropWhile (\x -> not ("deleteAfterBlockNo" `BS.isPrefixOf` x))
        . BS.lines

    isNewFunction :: ByteString -> Bool
    isNewFunction bs = not (BS.null bs) && isAlpha (BS.head bs)

    getTableName :: ByteString -> Maybe ByteString
    getTableName bs =
      case BS.breakSubstring "@" bs of
        (_, "") -> Nothing
        (h, rs) ->
          if "table " `BS.isInfixOf` h
            then Just $ BS.drop 1 (BS.takeWhile (/= ')') rs)
            else Nothing

-- -------------------------------------------------------------------------------------------------

codeGreen :: String
codeGreen = setSGRCode [SetColor Foreground Vivid Green]

codeRed :: String
codeRed = setSGRCode [SetColor Foreground Vivid Red]

codeReset :: String
codeReset = setSGRCode [Reset]

greenText :: String -> String
greenText s = codeGreen ++ s ++ codeReset

redText :: String -> String
redText s = codeRed ++ s ++ codeReset

-- Remove comments and then remove empty lines.
removeCommentsAndEmpty :: ByteString -> Maybe ByteString
removeCommentsAndEmpty bs =
  let start = fst $ BS.breakSubstring "--" bs
   in if BS.null (BS.strip start)
        then Nothing
        else Just start
