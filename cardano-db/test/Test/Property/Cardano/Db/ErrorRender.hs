{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Property.Cardano.Db.ErrorRender (
  tests,
) where

import Cardano.Db (DbCallStack (..), DbLookupError (..), DbSessionError (..))
import Control.Exception (displayException)
import Data.List (isInfixOf)
import GHC.Stack (SrcLoc (..))
import Hedgehog (Property, discover, (===))
import qualified Hedgehog as H

-- A user-facing error must show its message, not the derived Show of the record
-- (which leaks the DbCallStack / SrcLoc internals as raw record fields).

-- A business lookup error is just its message: no call chain, no internals.
prop_lookupErrorShowsMessageOnly :: Property
prop_lookupErrorShowsMessageOnly =
  H.withTests 1 . H.property $
    displayException (DbLookupError callStack "Slot not found for slot_no: 5")
      === "Slot not found for slot_no: 5"

-- A session (infrastructure) error keeps a readable call chain for diagnostics,
-- but must not dump the derived Show of the DbCallStack / SrcLoc records.
prop_sessionErrorHasNoRawRecordDump :: Property
prop_sessionErrorHasNoRawRecordDump =
  H.withTests 1 . H.property $ do
    let rendered = displayException (DbSessionError callStack "ResultError (RowError 0 6 UnexpectedNull)")
    H.assert ("ResultError (RowError 0 6 UnexpectedNull)" `isInfixOf` rendered)
    H.assert (not ("dbCsFncName" `isInfixOf` rendered))
    H.assert (not ("srcLocFile" `isInfixOf` rendered))

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
