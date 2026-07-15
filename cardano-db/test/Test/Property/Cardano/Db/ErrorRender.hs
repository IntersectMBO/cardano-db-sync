{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Cardano.Db.ErrorRender (
  tests,
) where

import Cardano.Db (DbCallStack (..), DbLookupError (..), DbSessionError (..))
import Control.Exception (displayException)
import Data.List (isInfixOf)
import GHC.Stack (SrcLoc (..))
import Hedgehog (Property, discover)
import qualified Hedgehog as H

-- A user-facing error must show its message, not the derived Show of the record
-- (which leaks the DbCallStack / SrcLoc internals).

prop_lookupErrorIsClean :: Property
prop_lookupErrorIsClean =
  H.withTests 1 . H.property $ do
    let rendered = displayException (DbLookupError callStack "Slot not found for slot_no: 5")
    H.assert ("Slot not found for slot_no: 5" `isInfixOf` rendered)
    H.assert (not ("SrcLoc" `isInfixOf` rendered))
    H.assert (not ("DbCallStack" `isInfixOf` rendered))

prop_sessionErrorIsClean :: Property
prop_sessionErrorIsClean =
  H.withTests 1 . H.property $ do
    let rendered = displayException (DbSessionError callStack "ResultError (RowError 0 6 UnexpectedNull)")
    H.assert ("ResultError (RowError 0 6 UnexpectedNull)" `isInfixOf` rendered)
    H.assert (not ("SrcLoc" `isInfixOf` rendered))
    H.assert (not ("DbCallStack" `isInfixOf` rendered))

callStack :: DbCallStack
callStack =
  DbCallStack
    { dbCsFncName = "querySlotUtcTime"
    , dbCsModule = "Cardano.DbTool.UtxoSet"
    , dbCsFile = "src/Cardano/DbTool/UtxoSet.hs"
    , dbCsLine = 93
    , dbCsCallChain = [("caller", srcLoc)]
    }

srcLoc :: SrcLoc
srcLoc =
  SrcLoc
    { srcLocPackage = "pkg"
    , srcLocModule = "Cardano.DbTool.UtxoSet"
    , srcLocFile = "src/Cardano/DbTool/UtxoSet.hs"
    , srcLocStartLine = 93
    , srcLocStartCol = 11
    , srcLocEndLine = 93
    , srcLocEndCol = 30
    }

tests :: IO Bool
tests = H.checkParallel $$discover
