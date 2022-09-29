{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Db.Mock.Property.Property
    ( prop_empty_blocks
    ) where

import           Control.Monad (void, when)
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically))
import           Data.Foldable
import           Data.Text (Text)
import           Data.TreeDiff (defaultExprViaShow)
import           GHC.Generics (Generic, Generic1)

import           Cardano.Mock.Chain
import           Cardano.Mock.ChainSync.Server
import           Cardano.Mock.Forging.Interpreter
import           Cardano.Mock.Forging.Tx.Babbage
import           Cardano.Mock.Forging.Types

import           Test.Cardano.Db.Mock.Config
import           Test.Cardano.Db.Mock.UnifiedApi
import           Test.Cardano.Db.Mock.Validate

import           Ouroboros.Network.Block hiding (RollBack)

import           Test.QuickCheck (Gen, Property, frequency, noShrinking, withMaxSuccess, (===))
import           Test.QuickCheck.Monadic (monadicIO, run)

import           Test.StateMachine
import           Test.StateMachine.Sequential (runCommands')
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
    { serverTip :: Maybe BlockNo
    , dbSyncTip :: Maybe BlockNo
    , dbSynsIsOn :: Bool
    , dbSynsHasSynced :: Bool -- This is used just to avoid restarting the node too early.
    }
    deriving stock (Generic, Show)

instance ToExpr (Model Symbolic)
instance ToExpr (Model Concrete)

initModel :: Model r
initModel = Model Nothing Nothing False False

instance ToExpr BlockNo where
    toExpr = defaultExprViaShow

data Response r =
    NewBlockAdded (Reference (Opaque CardanoBlock) r)
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
    (RollForward _, _) | dbSynsIsOn m ->
        m { serverTip = nextTip $ serverTip m, dbSyncTip = nextTip $ dbSyncTip m}
    (RollForward _, _) ->
        m { serverTip = nextTip $ serverTip m}
    (RollBack blkNo, _) | dbSynsIsOn m ->
        m { serverTip = blkNo, dbSyncTip = blkNo }
    (RollBack blkNo, _) ->
        m { serverTip = blkNo }
    (StopDBSync, _) | dbSynsIsOn m ->
        m { dbSynsIsOn = False }
    (StopDBSync, _) ->
        error "Tried to stop stopped DBSync"
    (StartDBSync, _) | dbSynsIsOn m ->
        error "Tried to start started DBSync"
    (StartDBSync, _) ->
        m { dbSyncTip = serverTip m, dbSynsIsOn = True , dbSynsHasSynced = False }
    (RestartNode, _) ->
        m
    (AssertBlockNo _, _) ->
        m { dbSynsHasSynced = True}
  where
    nextTip Nothing = Just $ BlockNo 1
    nextTip (Just b) = Just $ b + 1

precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition m cmd = case cmd of
  RollForward _ -> Top
  RollBack n -> n .< serverTip m -- can it be equal?
  StopDBSync  -> Boolean $ dbSynsIsOn m && dbSynsHasSynced m
  StartDBSync -> Boolean $ not $ dbSynsIsOn m
  RestartNode -> Boolean $ dbSynsHasSynced m
  AssertBlockNo n -> Boolean $ dbSyncTip m == n

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
generator m = Just $ frequency
  [ (if isOn then 90 else 20, genRollForward m)
  , (if not canRollback then 0 else if isOn then 10 else 20, genRollBack m)
  , (if isOn then 30 else  0, genStopDBSync m)
  , (if isOn then  0 else 60, genStartDBSync m)
  , (if isOn then  20 else 5, genRestartNode m)
  , (if isOn then 30 else 0, genAssertBlockNo m)
  ]
  where
    isOn = dbSynsIsOn m
    canRollback = case serverTip m of
      Nothing -> False
      Just 0 -> False
      Just _ -> True

genRollForward :: Model Symbolic -> Gen (Command Symbolic)
genRollForward _ = RollForward <$> frequency [(60, pure 0), (30, pure 1), (20, pure 2)]

genRollBack :: Model Symbolic -> Gen (Command Symbolic)
genRollBack m = case serverTip m of
    Nothing -> pure $ RollBack Nothing
    Just 0 -> pure $ RollBack Nothing -- probably can't happen
    Just 1 -> pure $ RollBack Nothing
    Just srvTip -> RollBack <$> frequency [(10, pure Nothing), (90, Just <$> rollbackPrev srvTip)]
  where
    rollbackPrev srvTip = frequency $ zip [1..(fromIntegral (unBlockNo srvTip) - 1)] (pure <$> [1.. (srvTip - 1)])

genStopDBSync :: Model Symbolic -> Gen (Command Symbolic)
genStopDBSync _ = pure StopDBSync

genStartDBSync :: Model Symbolic -> Gen (Command Symbolic)
genStartDBSync _ = pure StartDBSync

genRestartNode :: Model Symbolic -> Gen (Command Symbolic)
genRestartNode _ = pure RestartNode

genAssertBlockNo :: Model Symbolic -> Gen (Command Symbolic)
genAssertBlockNo m = pure $ AssertBlockNo $ dbSyncTip m

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ (RollForward n) = [ RollForward n' | n' <- [0,1,2], n' < n ]
shrinker m (RollBack mBlockNo) =
    [ RollBack (Just blkNo')
    | serverBlockNo <- toList (serverTip m)
    , blkNo' <- [toNext mBlockNo..serverBlockNo]
    , blkNo' < serverBlockNo ]
  where
    toNext Nothing = BlockNo 1
    toNext (Just n) = n + 1
shrinker _ _ = []

sm :: Interpreter -> ServerHandle IO CardanoBlock -> DBSyncEnv -> StateMachine Model Command IO Response
sm interpreter mockServer dbSync = StateMachine initModel transition precondition postcondition
         Nothing generator shrinker (semantics interpreter mockServer dbSync) mock noCleanup

prop_empty_blocks :: IOManager -> [(Text, Text)] -> Property
prop_empty_blocks iom knownMigrations = withMaxSuccess 20 $ noShrinking $ forAllCommands smSymbolic (Just 20) $ \cmds -> monadicIO $ do
    (hist, res) <- run $ runAction $ \interpreter mockServer dbSync -> do
      (hist, model, res) <- runCommands' (sm interpreter mockServer dbSync) cmds
      when (dbSynsIsOn model) $
        void $ runAssert dbSync (dbSyncTip model)
      pure (hist, res)
    prettyCommands smSymbolic hist (checkCommandNames cmds (res === Ok))
  where
    smSymbolic = sm (error "inter") (error "mockServer") (error "dbSync")
    runAction action = withFullConfig' False "config" "qsm" action iom knownMigrations

