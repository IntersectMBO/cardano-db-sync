{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Statement.Constraint where

import Cardano.BM.Data.Trace (Trace)
import Cardano.BM.Trace (logInfo)
import Cardano.Db.Schema.Core.StakeDelegation (EpochStake, Reward)
import Cardano.Prelude (HasCallStack, Proxy (..), liftIO)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEnc
import qualified Hasql.Decoders as HsqlD
import qualified Hasql.Encoders as HsqlE
import qualified Hasql.Session as HsqlSess
import qualified Hasql.Statement as HsqlStmt

import Cardano.Db.Error (mkDbCallStack)
import Cardano.Db.Statement.Function.Core (runSession)
import Cardano.Db.Statement.Types (DbInfo (..))
import Cardano.Db.Types (DbM)

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

-- Constraint names
constraintNameEpochStake :: ConstraintNameDB
constraintNameEpochStake = ConstraintNameDB "unique_epoch_stake"

constraintNameReward :: ConstraintNameDB
constraintNameReward = ConstraintNameDB "unique_reward"

-- | Statement for checking if a constraint exists
queryHasConstraintStmt :: HsqlStmt.Statement Text.Text Bool
queryHasConstraintStmt =
  HsqlStmt.Statement sql encoder decoder True
  where
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "SELECT EXISTS (SELECT 1 FROM pg_constraint WHERE conname = $1)"
          ]
    encoder = HsqlE.param (HsqlE.nonNullable HsqlE.text)
    decoder = HsqlD.singleRow (HsqlD.column $ HsqlD.nonNullable HsqlD.bool)

-- | Statement for adding a unique constraint (no parameters - SQL built dynamically)
addUniqueConstraintStmt :: Text.Text -> Text.Text -> [Text.Text] -> HsqlStmt.Statement () ()
addUniqueConstraintStmt tbName constraintName fields =
  HsqlStmt.Statement sql HsqlE.noParams HsqlD.noResult True
  where
    fieldList = Text.intercalate ", " fields
    sql =
      TextEnc.encodeUtf8 $
        Text.concat
          [ "ALTER TABLE "
          , tbName
          , " ADD CONSTRAINT "
          , constraintName
          , " UNIQUE ("
          , fieldList
          , ")"
          ]

-- | Check if a constraint exists
queryHasConstraint :: HasCallStack => ConstraintNameDB -> DbM Bool
queryHasConstraint (ConstraintNameDB cname) =
  runSession mkDbCallStack $
    HsqlSess.statement cname queryHasConstraintStmt

-- | Generic function to add a unique constraint to any table with DbInfo
alterTableAddUniqueConstraint ::
  forall table.
  (DbInfo table, HasCallStack) =>
  Proxy table ->
  ConstraintNameDB ->
  [FieldNameDB] ->
  DbM ()
alterTableAddUniqueConstraint proxy (ConstraintNameDB cname) fields =
  runSession mkDbCallStack $
    HsqlSess.statement () $
      addUniqueConstraintStmt tbName cname fieldNames
  where
    tbName = tableName proxy
    fieldNames = map unFieldNameDB fields

-- | Data type to track manual constraints
data ManualDbConstraints = ManualDbConstraints
  { dbConstraintRewards :: !Bool
  , dbConstraintEpochStake :: !Bool
  }
  deriving (Show, Eq)

-- | Check if constraints exist
queryRewardAndEpochStakeConstraints :: DbM ManualDbConstraints
queryRewardAndEpochStakeConstraints = do
  epochStake <- queryHasConstraint constraintNameEpochStake
  reward <- queryHasConstraint constraintNameReward
  pure $
    ManualDbConstraints
      { dbConstraintRewards = reward
      , dbConstraintEpochStake = epochStake
      }

-- | Add reward table constraint
addRewardTableConstraint ::
  Trace IO Text.Text ->
  DbM ()
addRewardTableConstraint trce = do
  let proxy = Proxy @Reward
      tbName = tableName proxy

  alterTableAddUniqueConstraint
    proxy
    constraintNameReward
    [ FieldNameDB "addr_id"
    , FieldNameDB "type"
    , FieldNameDB "earned_epoch"
    , FieldNameDB "pool_id"
    ]
  liftIO $ logNewConstraint trce tbName (unConstraintNameDB constraintNameReward)

-- | Add epoch stake table constraint
addEpochStakeTableConstraint ::
  Trace IO Text.Text ->
  DbM ()
addEpochStakeTableConstraint trce = do
  let proxy = Proxy @EpochStake
      tbName = tableName proxy

  alterTableAddUniqueConstraint
    proxy
    constraintNameEpochStake
    [ FieldNameDB "epoch_no"
    , FieldNameDB "addr_id"
    , FieldNameDB "pool_id"
    ]
  liftIO $ logNewConstraint trce tbName (unConstraintNameDB constraintNameEpochStake)

-- | Log new constraint creation
logNewConstraint ::
  Trace IO Text.Text ->
  -- | Table name
  Text.Text ->
  -- | Constraint name
  Text.Text ->
  IO ()
logNewConstraint trce tbName constraintName =
  logInfo trce $
    "The table "
      <> tbName
      <> " was given a new unique constraint called "
      <> constraintName
