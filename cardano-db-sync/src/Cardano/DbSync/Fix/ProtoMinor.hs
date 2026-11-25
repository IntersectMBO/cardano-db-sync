{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Repair of @block.proto_minor@ for issue #2190. The column used to be written through a
-- 'Word16' field and a signed @int2@ encoder, so a header minor with bits at or above bit 15
-- lost bits 16 to 31 and had the rest wrapped negative. CIP-0203 packs a software tag into
-- that field, so the lost bits carry real information and the only way to recover them is to
-- read the headers back from the node.
module Cardano.DbSync.Fix.ProtoMinor (
  ProtoMinorFixPlan (..),
  planProtoMinorFix,
  chainSyncClientFixProtoMinor,
) where

import qualified Cardano.Db as DB
import Cardano.Db.Log (logInfo, logWarning)
import Cardano.DbSync.Api (convertToPoint, getTrace)
import Cardano.DbSync.Api.Types (SyncEnv, envNetworkMagic, envOptions, envProtoMinorFixDone, soptFixFromEpoch, soptForceFix)
import qualified Cardano.DbSync.Era.Shelley.Generic.Block as Generic
import Cardano.DbSync.Types (CardanoBlock, CardanoPoint)
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Logging (Trace)
import Cardano.Prelude
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Control.Concurrent.Class.MonadSTM.Strict (readTVarIO, writeTVar)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Network.Block (BlockNo (..), Tip, blockNo, genesisPoint, getTipBlockNo, unBlockNo)
import Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Client

-- | How many epochs before the first visible tag the sweep starts. A stored value only
-- reveals a tag through its surviving low byte, so the boundary derived from the database is
-- a lower bound: a truncated value whose low byte happens to look like a genuine small minor
-- is invisible. The padding covers that case cheaply, because the sweep only writes where the
-- value differs, so genuine blocks in the padded range cost nothing.
padEpochs :: Word64
padEpochs = 3

-- | How often to log progress, in blocks read.
logEvery :: Word64
logEvery = 100000

-- | How long to wait before retrying when the node cannot find the start point, which means
-- it is behind the database: a fresh node, or one rolled back below our tip.
retryDelayMicros :: Int
retryDelayMicros = 180 * 1000 * 1000

-- | The epoch each known network's repair starts from, so an operator does not pay for a
-- sweep of the whole chain. These are the low-byte-scan boundaries from the backfill note,
-- padded back a few epochs; replace a value here once a from-genesis run has confirmed that
-- nothing is hidden before it. 'Nothing' means the network is unknown to this release and the
-- boundary is derived from the database instead.
--
-- The derived boundary is only a lower bound, because a stored value reveals a tag solely
-- through its surviving low byte, so a confirmed constant is strictly better where we have
-- one.
protoMinorFixStartEpoch :: NetworkMagic -> Maybe Word64
protoMinorFixStartEpoch (NetworkMagic magic) =
  case magic of
    764824073 -> Just 640 -- mainnet, first tag seen in epoch 644
    1 -> Just 294 -- preprod, first tag seen in epoch 298
    2 -> Just 1470 -- preview, confirmed: a from-genesis run found the first tagged block in epoch 1473
    6 -> Just 5 -- dijkstra, first tag seen in epoch 9
    164 -> Just 13 -- musashi, first tag seen in epoch 17
    4 -> Just 0 -- sanchonet, no tag seen; the gate only lets this run if one appears
    _ -> Nothing

-- | Run one of the repair's database actions. The repair runs on the chainsync thread, so
-- it must not use the connection the database thread writes blocks on: 'runInTransaction'
-- issues its @BEGIN@ and @COMMIT@ as separate sessions, so the two would interleave and this
-- @COMMIT@ could commit a half-written batch of blocks. db-sync keeps a pool for exactly
-- this; a 'DbEnv' without one (a tool, a test) has no database thread to collide with.
runRepairDb :: Trace IO DB.LogMessage -> DB.DbEnv -> DB.DbM a -> IO a
runRepairDb trce dbEnv action
  | isJust (DB.dbPoolConnection dbEnv) =
      DB.runDbPoolTransLoggedWith (const True) trce dbEnv (Just DB.ReadCommitted) action
  | otherwise = DB.runDbTransSilent dbEnv DB.ReadCommitted action

data ProtoMinorFixPlan = ProtoMinorFixPlan
  { pmfpFrom :: !CardanoPoint
  -- ^ Where to intersect the chain, one block before the padded start epoch.
  , pmfpFromEpoch :: !Word64
  }

-- | Decide whether the repair has to run and where it starts. 'Nothing' means there is
-- nothing to do: either the marker is already recorded, or no tagged block is stored. In the
-- latter case the marker is written so the check is not repeated on every start.
planProtoMinorFix :: SyncEnv -> DB.DbEnv -> IO (Maybe ProtoMinorFixPlan)
planProtoMinorFix syncEnv dbEnv = do
  -- The repair hands over by letting the connection drop, and this runs again on the next
  -- one, so without the in-process flag a forced sweep would restart for as long as the flag
  -- is on the command line and syncing would never resume. A completed automatic run has
  -- written the marker as well, so checking the flag first only saves a query.
  alreadyRun <- readTVarIO (envProtoMinorFixDone syncEnv)
  if alreadyRun
    then pure Nothing
    else case soptFixFromEpoch opts of
      -- An explicit boundary is a deliberate manual run, so it overrides the built-in
      -- boundary, the marker and the "is anything broken" check too: the point of sweeping
      -- from genesis is to find what the stored values cannot reveal.
      Just forced -> do
        logInfo trce $
          mconcat
            [ "--fix-from-epoch "
            , textShow forced
            , " was given, so the block.proto_minor repair runs from that epoch regardless of"
            , " the built-in boundary and of whether it has run before."
            ]
        Just <$> planFrom forced
      Nothing
        -- The same deliberate re-run, but from this network's boundary, so an operator who
        -- suspects the gate below missed something neither guesses an epoch nor pays for a
        -- sweep from genesis.
        | soptForceFix opts -> do
            logInfo trce $
              mconcat
                [ "--force-fix was given, so the block.proto_minor repair runs from this"
                , " network's boundary epoch regardless of the completion marker and of"
                , " whether anything stored looks broken."
                ]
            Just <$> (planFrom =<< boundaryEpoch)
        | otherwise -> planAutomatically
  where
    trce = getTrace syncEnv
    opts = envOptions syncEnv

    runDb :: DB.DbM a -> IO a
    runDb = runRepairDb trce dbEnv

    planFrom :: Word64 -> IO ProtoMinorFixPlan
    planFrom fromEpoch = do
      mPoint <- runDb (DB.queryLastBlockBeforeEpoch fromEpoch)
      let point = case mPoint of
            Just (slot, hsh) -> fromMaybe genesisPoint (convertToPoint (SlotNo slot) hsh)
            Nothing -> genesisPoint
      pure ProtoMinorFixPlan {pmfpFrom = point, pmfpFromEpoch = fromEpoch}

    -- \| Where a run that was not given an explicit epoch starts.
    boundaryEpoch :: IO Word64
    boundaryEpoch =
      case protoMinorFixStartEpoch (envNetworkMagic syncEnv) of
        Just e -> pure e
        Nothing ->
          -- Unknown network: fall back to what the stored values imply.
          runDb DB.queryFirstTaggedProtoMinorEpoch >>= \case
            Just tagEpoch -> pure $ if tagEpoch > padEpochs then tagEpoch - padEpochs else 0
            Nothing -> pure 0

    planAutomatically :: IO (Maybe ProtoMinorFixPlan)
    planAutomatically = do
      done <- runDb DB.queryAllExtraMigrations
      if DB.ProtoMinorFixed `elem` done
        then pure Nothing
        else do
          -- Gate on a truncated row actually being present, so a database synced
          -- entirely by a fixed db-sync never pays for a sweep.
          needed <- runDb DB.queryHasTaggedProtoMinor
          if not needed
            then do
              -- Only a sign-wrapped value is recognisable from the stored data, so a
              -- truncation whose surviving low bits look like a genuine small minor is
              -- invisible here. Widening the test would sweep healthy databases, so the way
              -- out is deliberate rather than automatic.
              logInfo trce $
                mconcat
                  [ "No truncated block.proto_minor found, so no repair is needed."
                  , " If this database was synced by a db-sync older than 13.9.0.0, --force-fix"
                  , " re-reads the headers anyway."
                  ]
              markProtoMinorFixed syncEnv dbEnv
              pure Nothing
            else Just <$> (planFrom =<< boundaryEpoch)

-- | Record the repair as done, in the database and for the rest of this process. The
-- insert is guarded because a forced run ignores the marker, so the row can already be
-- there and @extra_migrations@ has no unique constraint on the token.
markProtoMinorFixed :: SyncEnv -> DB.DbEnv -> IO ()
markProtoMinorFixed syncEnv dbEnv = do
  runRepairDb (getTrace syncEnv) dbEnv $ do
    done <- DB.queryAllExtraMigrations
    unless (DB.ProtoMinorFixed `elem` done) $
      DB.insertExtraMigration DB.ProtoMinorFixed
  atomically $ writeTVar (envProtoMinorFixDone syncEnv) True

-- | Sweep from the plan's start point to the tip, rewriting @proto_minor@ wherever the
-- node's header disagrees with what is stored. The protocol cannot be restarted on the same
-- channel once this client is done, so the caller is expected to let the connection drop and
-- resume normal syncing on the next one.
chainSyncClientFixProtoMinor ::
  SyncEnv ->
  DB.DbEnv ->
  ProtoMinorFixPlan ->
  Client.ChainSyncClient CardanoBlock CardanoPoint (Tip CardanoBlock) IO ()
chainSyncClientFixProtoMinor syncEnv dbEnv plan = Client.ChainSyncClient $ do
  logInfo trce $
    mconcat
      [ "Starting chainsync to repair block.proto_minor from epoch "
      , textShow (pmfpFromEpoch plan)
      , ". This updates already stored blocks (issue #2190)."
      ]
  pure $ Client.SendMsgFindIntersect [pmfpFrom plan] clientStIntersect
  where
    trce = getTrace syncEnv

    clientStIntersect =
      Client.ClientStIntersect
        { Client.recvMsgIntersectFound = \_pnt tip ->
            Client.ChainSyncClient $
              pure $
                Client.SendMsgRequestNext (pure ()) (clientStNext (tipBlockNo tip) 0 0)
        , Client.recvMsgIntersectNotFound = \tip -> Client.ChainSyncClient $ do
            -- The point comes from our own database, so the node is behind it. Waiting is
            -- the right move: returning here would drop the connection, and the
            -- subscription reconnects with no delay, which would spin.
            logWarning trce $
              mconcat
                [ "The node cannot find the block.proto_minor repair start point "
                , textShow (pmfpFrom plan)
                , ", it is at "
                , textShow tip
                , ". Waiting 3 minutes and retrying."
                ]
            liftIO $ threadDelay retryDelayMicros
            pure $ Client.SendMsgFindIntersect [pmfpFrom plan] clientStIntersect
        }

    tipBlockNo tip =
      case getTipBlockNo tip of
        Origin -> BlockNo 0
        At bno -> bno

    clientStNext ::
      BlockNo ->
      Word64 ->
      Word64 ->
      Client.ClientStNext CardanoBlock CardanoPoint (Tip CardanoBlock) IO ()
    clientStNext stopAt seen fixed =
      Client.ClientStNext
        { Client.recvMsgRollForward = \blk _tip -> Client.ChainSyncClient $ do
            changed <- case blockProtoMinor blk of
              Nothing -> pure 0
              -- The old path narrowed to 'Word16' and then encoded as a signed 'int2',
              -- so a true minor below 32768 came through both steps unchanged and the
              -- stored value is already right. Skipping those costs no round trip, so the
              -- sweep is bound by the number of tagged blocks, not by the blocks read.
              Just (_, minorV) | minorV < 0x8000 -> pure 0
              Just (hsh, minorV) ->
                runRepairDb trce dbEnv $
                  DB.updateBlockProtoMinor hsh (fromIntegral minorV)
            let seen' = seen + 1
                fixed' = fixed + fromIntegral (max 0 changed)
            -- The first divergence is the answer a from-genesis run is after, so name it
            -- exactly rather than leaving it inside a progress window.
            when (fixed == 0 && fixed' > 0) $
              logInfo trce $
                mconcat
                  [ "First truncated block.proto_minor is at block "
                  , textShow (unBlockNo (blockNo blk))
                  , ", true value "
                  , textShow (maybe 0 snd (blockProtoMinor blk))
                  ]
            when (seen' `mod` logEvery == 0) $
              logInfo trce $
                mconcat
                  [ "block.proto_minor repair: read "
                  , textShow seen'
                  , " blocks, updated "
                  , textShow fixed'
                  ]
            if blockNo blk >= stopAt
              then do
                logInfo trce $
                  mconcat
                    [ "Finished the block.proto_minor repair: read "
                    , textShow seen'
                    , " blocks, updated "
                    , textShow fixed'
                    , if fixed' == 0
                        then ". Nothing in the swept range needed changing."
                        else "."
                    ]
                markProtoMinorFixed syncEnv dbEnv
                pure $ Client.SendMsgDone ()
              else pure $ Client.SendMsgRequestNext (pure ()) (clientStNext stopAt seen' fixed')
        , Client.recvMsgRollBackward = \_pnt _tip ->
            Client.ChainSyncClient $
              pure $
                Client.SendMsgRequestNext (pure ()) (clientStNext stopAt seen fixed)
        }

-- | The block's hash and the true unsigned protocol minor from its header. Byron headers
-- carry a different, narrow protocol version and predate CIP-0203, so they are skipped.
blockProtoMinor :: CardanoBlock -> Maybe (ByteString, Word32)
blockProtoMinor = \case
  BlockByron _ -> Nothing
  BlockShelley blk -> tpraos blk
  BlockAllegra blk -> tpraos blk
  BlockMary blk -> tpraos blk
  BlockAlonzo blk -> tpraos blk
  BlockBabbage blk -> praos blk
  BlockConway blk -> praos blk
  BlockDijkstra blk -> praos blk
  where
    tpraos blk =
      Just (Generic.blockHash blk, Ledger.pvMinor (Generic.blockProtoVersionTPraos blk))
    praos blk =
      Just (Generic.blockHash blk, Ledger.pvMinor (Generic.blockProtoVersionPraos blk))
