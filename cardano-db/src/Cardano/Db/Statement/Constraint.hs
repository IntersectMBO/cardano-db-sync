{-# LANGUAGE OverloadedStrings #-}

module Cardano.Db.Statement.Constraint (
  -- * Types
  ConstraintNameDB (..),
  FieldNameDB (..),
  AlterTable (..),

  -- * Statement functions
  queryHasConstraintStmt,
  addConstraintStmt,
  dropConstraintStmt,

  -- * Session functions
  queryHasConstraint,
  alterTableAddConstraint,
  alterTableDropConstraint,
) where

import Cardano.Db.Statement.Function.Core (mkCallInfo, runDbSession)
import Cardano.Db.Types (DbAction)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Contravariant ((>$<))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSess
import qualified Hasql.Statement as HsqlStmt

-- | Name of a database constraint
newtype ConstraintNameDB = ConstraintNameDB
  { unConstraintNameDB :: Text.Text
  }
  deriving (Eq, Show)

-- | Name of a database field/column
newtype FieldNameDB = FieldNameDB
  { unFieldNameDB :: Text.Text
  }
  deriving (Eq, Show)

-- | Alter table operations
data AlterTable
  = AddUniqueConstraint ConstraintNameDB [FieldNameDB]
  | DropUniqueConstraint ConstraintNameDB
  deriving (Show)

-- | Helper function for Text parameter encoding
textParam :: HsqlE.Params Text.Text
textParam = HsqlE.param (HsqlE.nonNullable HsqlE.text)

-- | Helper for encoding constraint name
constraintNameParam :: HsqlE.Params ConstraintNameDB
constraintNameParam = HsqlE.param (HsqlE.nonNullable (unConstraintNameDB >$< HsqlE.text))

-- | Helper for encoding field list as comma-separated string
fieldListParam :: HsqlE.Params [FieldNameDB]
fieldListParam = HsqlE.param (HsqlE.nonNullable (fieldListToText >$< HsqlE.text))
  where
    fieldListToText = Text.intercalate "," . map unFieldNameDB

-- | Statement for checking if a constraint exists
queryHasConstraintStmt :: HsqlStmt.Statement ConstraintNameDB Bool
queryHasConstraintStmt =
  HsqlStmt.Statement sql constraintNameParam decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = $1)"
          ]

    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.bool)

-- | Data type for add constraint parameters
data AddConstraintParams = AddConstraintParams
  { acpTableName :: !Text.Text
  , acpConstraintName :: !ConstraintNameDB
  , acpFields :: ![FieldNameDB]
  }

-- | Data type for drop constraint parameters
data DropConstraintParams = DropConstraintParams
  { dcpTableName :: !Text.Text
  , dcpConstraintName :: !ConstraintNameDB
  }

-- | Encoder for AddConstraintParams
addConstraintParamsEncoder :: HsqlE.Params AddConstraintParams
addConstraintParamsEncoder =
  mconcat
    [ acpTableName >$< textParam
    , acpConstraintName >$< constraintNameParam
    , acpFields >$< fieldListParam
    ]

-- | Encoder for DropConstraintParams
dropConstraintParamsEncoder :: HsqlE.Params DropConstraintParams
dropConstraintParamsEncoder =
  mconcat
    [ dcpTableName >$< textParam
    , dcpConstraintName >$< constraintNameParam
    ]

-- | Statement for adding a unique constraint
addConstraintStmt :: HsqlStmt.Statement AddConstraintParams ()
addConstraintStmt =
  HsqlStmt.Statement sql addConstraintParamsEncoder HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "ALTER TABLE $1 ADD CONSTRAINT $2 UNIQUE($3)"
          ]

-- | Statement for dropping a constraint
dropConstraintStmt :: HsqlStmt.Statement DropConstraintParams ()
dropConstraintStmt =
  HsqlStmt.Statement sql dropConstraintParamsEncoder HsqlD.noResult True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2"
          ]

-- | Check if a constraint exists
queryHasConstraint :: MonadIO m => ConstraintNameDB -> DbAction m Bool
queryHasConstraint cname =
  runDbSession (mkCallInfo "queryHasConstraint") $
    HsqlSess.statement cname queryHasConstraintStmt

-- | Add a unique constraint to a table
alterTableAddConstraint ::
  MonadIO m =>
  -- | Table name
  Text.Text ->
  -- | Constraint name
  ConstraintNameDB ->
  -- | Field names
  [FieldNameDB] ->
  DbAction m ()
alterTableAddConstraint tableName cname fields =
  runDbSession (mkCallInfo "alterTableAddConstraint") $
    HsqlSess.statement params addConstraintStmt
  where
    params =
      AddConstraintParams
        { acpTableName = tableName
        , acpConstraintName = cname
        , acpFields = fields
        }

-- | Drop a constraint from a table
alterTableDropConstraint ::
  MonadIO m =>
  -- | Table name
  Text.Text ->
  -- | Constraint name
  ConstraintNameDB ->
  DbAction m ()
alterTableDropConstraint tableName cname =
  runDbSession (mkCallInfo "alterTableDropConstraint") $
    HsqlSess.statement params dropConstraintStmt
  where
    params =
      DropConstraintParams
        { dcpTableName = tableName
        , dcpConstraintName = cname
        }
