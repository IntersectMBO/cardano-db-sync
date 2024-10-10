{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Db.Mock.Property.Property (
  prop_empty_blocks,
) where

import Cardano.Mock.Chain
import Cardano.Mock.ChainSync.Server
import Cardano.Mock.Forging.Interpreter
import Cardano.Mock.Forging.Tx.Babbage
import Cardano.Mock.Forging.Types
import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM (atomically))
import Data.Foldable
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.TreeDiff as T
import GHC.Generics (Generic, Generic1)
import Ouroboros.Network.Block hiding (RollBack)
import Test.Cardano.Db.Mock.Config
import Test.Cardano.Db.Mock.UnifiedApi
import Test.Cardano.Db.Mock.Validate
import Test.QuickCheck (Gen, Property, frequency, noShrinking, withMaxSuccess, (===))
import Test.QuickCheck.Monadic (monadicIO, run)
import Test.StateMachine
import Test.StateMachine.Sequential (runCommands')
import qualified Test.StateMachine.Types.Rank2 as Rank2

data Command r
  = RollForward Int
  | RollBack (Maybe BlockNo)
  | StopDBSync
  | StartDBSync
  | RestartNode
  | AssertBlockNo (Maybe BlockNo)
  deriving stock (Eq, Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance Show (Command Symbolic)

deriving stock instance Show (Command Concrete)

data Model r = Model
  { serverChain :: [Int]
  , dbSyncChain :: [Int]
  , dbSyncMaxBlockNo :: Maybe BlockNo
  , dbSynsIsOn :: Bool
  , dbSynsHasSynced :: Bool -- This is used just to avoid restarting the node too early.
  }
  deriving stock (Generic, Show)

serverTip :: Model r -> Maybe BlockNo
serverTip m = case serverChain m of
  [] -> Nothing
  ls -> Just $ BlockNo $ fromIntegral $ length ls

dbSyncTip :: Model r -> Maybe BlockNo
dbSyncTip m = case dbSyncChain m of
  [] -> Nothing
  ls -> Just $ BlockNo $ fromIntegral $ length ls

advanceNo :: Maybe BlockNo -> Maybe BlockNo
advanceNo Nothing = Just 1
advanceNo (Just n) = Just $ n + 1

rollbackChain :: Maybe BlockNo -> [Int] -> [Int]
rollbackChain Nothing _ = []
rollbackChain (Just blkNo) ls
  | len <- fromIntegral (unBlockNo blkNo)
  , length ls >= len =
      take len ls
rollbackChain _ _ = error "failed to rollback"

instance ToExpr x => CanDiff x where
  type ADiff x = Edit EditExpr
  type AnExpr x = Expr

  toDiff = toExpr
  exprDiff _ = T.exprDiff
  diffToDocCompact _ = ansiWlBgEditExprCompact
  diffToDoc _ = ansiWlBgEditExpr
  exprToDoc _ = ansiWlBgExpr

instance ToExpr (r k) => ToExpr (Reference k r)

instance ToExpr (Opaque a) where
  toExpr _ = App "Opaque" []

instance ToExpr BlockNo

instance ToExpr (Model Symbolic)

instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model [] [] Nothing False False

data Response r
  = NewBlockAdded (Reference (Opaque CardanoBlock) r)
  | Error String
  | Unit ()
  deriving stock (Generic1)
  deriving anyclass (Rank2.Foldable)

deriving stock instance Show (Response Symbolic)

deriving stock instance Read (Response Symbolic)

deriving stock instance Show (Response Concrete)

transition :: Model r -> Command r -> Response r -> Model r
transition m cmd resp = case (cmd, resp) of
  (_, Error msg) -> error msg
  (RollForward n, _) ->
    let serverChain' = serverChain m ++ [n]
        dbSyncMaxBlockNo' = max (dbSyncMaxBlockNo m) (advanceNo $ serverTip m)
     in m
          { serverChain = serverChain'
          , dbSyncMaxBlockNo = dbSyncMaxBlockNo'
          }
  (RollBack blkNo, _) ->
    m {serverChain = rollbackChain blkNo (serverChain m)}
  (StopDBSync, _)
    | dbSynsIsOn m ->
        m {dbSynsIsOn = False}
  (StopDBSync, _) ->
    error "Tried to stop stopped DBSync"
  (StartDBSync, _)
    | dbSynsIsOn m ->
        error "Tried to start started DBSync"
  (StartDBSync, _) ->
    m
      { dbSynsIsOn = True
      , dbSynsHasSynced = False
      }
  (RestartNode, _) ->
    m
  (AssertBlockNo n, _) ->
    m
      { dbSynsHasSynced = True
      , dbSyncChain = if n == dbSyncTip m then dbSyncChain m else serverChain m
      }

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition m cmd = case cmd of
  RollForward _ -> Top
  RollBack n -> n .< serverTip m -- can it be equal?
  StopDBSync -> Boolean $ dbSynsIsOn m && dbSynsHasSynced m
  StartDBSync -> Boolean $ not $ dbSynsIsOn m
  RestartNode -> Boolean $ dbSynsHasSynced m
  AssertBlockNo n | Just n' <- canAssert m -> n .== n'
  _ -> Bot

canAssert :: Model Symbolic -> Maybe (Maybe BlockNo)
canAssert m =
  if stip >= dbtip
    then Just stip
    else Nothing
  where
    dbtip = dbSyncMaxBlockNo m
    stip = serverTip m

postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition _ _ resp = case resp of
  Error msg -> Annotate msg Bot
  _ -> Top

semantics :: Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> Command Concrete -> IO (Response Concrete)
semantics interpreter mockServer dbSync cmd = case cmd of
  RollForward n -> NewBlockAdded . reference . Opaque <$> createBlock n interpreter mockServer
  RollBack Nothing -> Unit <$> rollbackTo interpreter mockServer GenesisPoint
  RollBack (Just blkNo) -> do
    chain <- atomically $ readChain mockServer
    case findFirstPointByBlockNo chain blkNo of
      Nothing -> pure $ Error $ "Failed to find point for " <> show blkNo
      Just pnt -> Unit <$> rollbackTo interpreter mockServer pnt
  StopDBSync -> Unit <$> stopDBSync dbSync
  StartDBSync -> Unit <$> startDBSync dbSync
  RestartNode -> Unit <$> restartServer mockServer
  AssertBlockNo mBlkNo -> runAssert dbSync mBlkNo

runAssert :: DBSyncEnv -> Maybe BlockNo -> IO (Response Concrete)
runAssert dbSync Nothing = Unit <$> assertBlocksCount dbSync 2
runAssert dbSync (Just n) = Unit <$> assertBlockNoBackoff dbSync (fromIntegral $ unBlockNo n)

mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock m cmd = case cmd of
  RollForward _ -> NewBlockAdded <$> genSym
  RollBack Nothing -> pure $ Unit ()
  RollBack mBlkNo | mBlkNo < serverTip m -> pure $ Unit ()
  RollBack (Just blkNo) -> pure $ Error $ "Failed to find point for " <> show blkNo
  StopDBSync -> pure $ Unit ()
  StartDBSync -> pure $ Unit ()
  RestartNode -> pure $ Unit ()
  AssertBlockNo _ -> pure $ Unit ()

createBlock :: Int -> Interpreter -> ServerHandle IO CardanoBlock -> IO CardanoBlock
createBlock n interpreter mockServer = case n of
  0 -> forgeNextFindLeaderAndSubmit interpreter mockServer []
  _ -> do
    nextBlockNo <- getNextBlockNo interpreter
    withBabbageFindLeaderAndSubmitTx interpreter mockServer $ \_ ->
      mkDummyRegisterTx n (fromIntegral $ unBlockNo nextBlockNo)

generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator m =
  Just $
    frequency
      [ (if isOn then 90 else 20, genRollForward m)
      , (if not canRollback then 0 else if isOn then 10 else 20, genRollBack m)
      , (if isOn then 30 else 0, genStopDBSync m)
      , (if isOn then 0 else 60, genStartDBSync m)
      , (if isOn then 20 else 5, genRestartNode m)
      , (if isOn && isJust serverNotBehind then 30 else 0, genAssertBlockNo m)
      ]
  where
    isOn = dbSynsIsOn m
    canRollback = case serverTip m of
      Nothing -> False
      Just 0 -> False
      Just _ -> True

    serverNotBehind = canAssert m

genRollForward :: Model Symbolic -> Gen (Command Symbolic)
genRollForward _ = RollForward <$> frequency [(60, pure 0), (30, pure 1), (20, pure 2)]

genRollBack :: Model Symbolic -> Gen (Command Symbolic)
genRollBack m = case serverTip m of
  Nothing -> pure $ RollBack Nothing
  Just 0 -> pure $ RollBack Nothing -- probably can't happen
  Just 1 -> pure $ RollBack Nothing
  Just srvTip -> RollBack <$> frequency [(10, pure Nothing), (90, Just <$> rollbackPrev srvTip)]
  where
    rollbackPrev srvTip = frequency $ zip [1 .. (fromIntegral (unBlockNo srvTip) - 1)] (pure <$> [1 .. (srvTip - 1)])

genStopDBSync :: Model Symbolic -> Gen (Command Symbolic)
genStopDBSync _ = pure StopDBSync

genStartDBSync :: Model Symbolic -> Gen (Command Symbolic)
genStartDBSync _ = pure StartDBSync

genRestartNode :: Model Symbolic -> Gen (Command Symbolic)
genRestartNode _ = pure RestartNode

genAssertBlockNo :: Model Symbolic -> Gen (Command Symbolic)
genAssertBlockNo m = pure $ AssertBlockNo $ serverTip m

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ (RollForward n) = [RollForward n' | n' <- [0, 1, 2], n' < n]
shrinker m (RollBack mBlockNo) =
  [ RollBack (Just blkNo')
  | serverBlockNo <- toList (serverTip m)
  , blkNo' <- [toNext mBlockNo .. serverBlockNo]
  , blkNo' < serverBlockNo
  ]
  where
    toNext Nothing = BlockNo 1
    toNext (Just n) = n + 1
shrinker _ _ = []

sm :: Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> StateMachine Model Command IO Response
sm interpreter mockServer dbSync =
  StateMachine
    initModel
    transition
    precondition
    postcondition
    Nothing
    generator
    shrinker
    (semantics interpreter mockServer dbSync)
    mock
    noCleanup

prop_empty_blocks :: IOManager -> [(Text, Text)] -> Property
prop_empty_blocks iom knownMigrations = withMaxSuccess 20 $ noShrinking $ forAllCommands smSymbolic (Just 20) $ \cmds -> monadicIO $ do
  (hist, res) <- run $ runAction $ \interpreter mockServer dbSync -> do
    (hist, _model, res) <- runCommands' (sm interpreter mockServer dbSync) cmds
    pure (hist, res)
  prettyCommands smSymbolic hist (checkCommandNames cmds (res === Ok))
  where
    smSymbolic = sm (error "inter") (error "mockServer") (error "dbSync")
    runAction action = withFullConfig' (WithConfigArgs False False False) initCommandLineArgs Nothing "config" "qsm" action iom knownMigrations
