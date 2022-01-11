{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.DbSync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncOptions (..)
  , mkSyncOptions
  , mkSyncEnvFromConfig
  , verifyFilePoints
  , getTrace
  , getLatestPoints
  , getSlotHash
  , getDbLatestBlockInfo
  , getDbTipBlockNo
  ) where

import           Cardano.Prelude hiding ((.))

import           Cardano.BM.Trace (Trace)

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto.ProtocolMagic

import qualified Cardano.Db as DB

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley

import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))

import           Cardano.DbSync.Config.Cardano
import           Cardano.DbSync.Config.Shelley
import           Cardano.DbSync.Config.Types
import           Cardano.DbSync.Error
import           Cardano.DbSync.LedgerState
import           Cardano.DbSync.Types

import           Control.Monad.Trans.Maybe (MaybeT (..))

import           Database.Persist.Sql (SqlBackend)

import           Ouroboros.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (BlockNo (..), Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point


data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envBackend :: !SqlBackend
  , envOptions :: !SyncOptions
  , envLedger :: !LedgerEnv
  }

newtype SyncOptions = SyncOptions
  { soptExtended :: Bool
  }

mkSyncOptions :: Bool -> SyncOptions
mkSyncOptions = SyncOptions

getTrace :: SyncEnv -> Trace IO Text
getTrace env = leTrace (envLedger env)

getSlotHash :: SlotNo -> IO [(SlotNo, ByteString)]
getSlotHash slotNo = DB.runDbNoLogging $ DB.querySlotHash slotNo

getDbLatestBlockInfo :: IO (Maybe TipInfo)
getDbLatestBlockInfo = do
  runMaybeT $ do
    block <- MaybeT $ DB.runDbNoLogging DB.queryLatestBlock
    -- The EpochNo, SlotNo and BlockNo can only be zero for the Byron
    -- era, but we need to make the types match, hence `fromMaybe`.
    pure $ TipInfo
            { bHash = DB.blockHash block
            , bEpochNo = EpochNo . fromMaybe 0 $ DB.blockEpochNo block
            , bSlotNo = SlotNo . fromMaybe 0 $ DB.blockSlotNo block
            , bBlockNo = BlockNo . fromMaybe 0 $ DB.blockBlockNo block
            }

getDbTipBlockNo :: IO (Point.WithOrigin BlockNo)
getDbTipBlockNo = do
  maybeTip <- getDbLatestBlockInfo
  case maybeTip of
    Just tip -> pure $ Point.At (bBlockNo tip)
    Nothing -> pure Point.Origin

mkSyncEnv
    :: Trace IO Text -> SqlBackend -> SyncOptions -> ProtocolInfo IO CardanoBlock -> Ledger.Network
    -> NetworkMagic -> SystemStart -> LedgerStateDir -> EpochSlot
    -> IO SyncEnv
mkSyncEnv trce backend syncOptions protoInfo nw nwMagic systemStart dir stableEpochSlot = do
  ledgerEnv <- mkLedgerEnv trce protoInfo dir nw stableEpochSlot systemStart
  pure $ SyncEnv
          { envProtocol = SyncProtocolCardano
          , envNetworkMagic = nwMagic
          , envSystemStart = systemStart
          , envBackend = backend
          , envOptions = syncOptions
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: Trace IO Text -> SqlBackend -> SyncOptions -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce backend syncOptions dir genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg _
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", DB.textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", DB.textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", DB.textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", DB.textShow (Shelley.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            Right <$> mkSyncEnv trce backend syncOptions (mkProtocolInfoCardano genCfg []) (Shelley.sgNetworkId $ scConfig sCfg)
                        (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                        (SystemStart .Byron.gdStartTime $ Byron.configGenesisData bCfg)
                        dir (calculateStableEpochSlot $ scConfig sCfg)


getLatestPoints :: SyncEnv -> IO [CardanoPoint]
getLatestPoints env = do
    files <- listLedgerStateFilesOrdered $ leDir (envLedger env)
    verifyFilePoints files

verifyFilePoints :: [LedgerStateFile] -> IO [CardanoPoint]
verifyFilePoints files =
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe CardanoPoint)
    validLedgerFileToPoint lsf = do
        hashes <- getSlotHash (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convert (slot, hash)
          _ -> pure Nothing

    convert :: (SlotNo, ByteString) -> Maybe CardanoPoint
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

-- -------------------------------------------------------------------------------------------------
-- This is incredibly suboptimal. It should work, for now, but may break at some future time and
-- when it is wrong then data in `db-sync` will simply be wrong and we do not have any way of
-- detecting that it is wrong.
--
-- An epoch is `10 k / f` long, and the stability window is `3 k / f` so the time from the start
-- of the epoch to start of the stability window is `7 k / f`.
--
-- Hopefully lower level libraries will be able to provide us with something better than this soon.
calculateStableEpochSlot :: Shelley.ShelleyGenesis era -> EpochSlot
calculateStableEpochSlot cfg =
    EpochSlot $ ceiling (7.0 * secParam / actSlotCoeff)
  where
    secParam :: Double
    secParam = fromIntegral $ Shelley.sgSecurityParam cfg

    actSlotCoeff :: Double
    actSlotCoeff = fromRational (Ledger.unboundRational $ Shelley.sgActiveSlotsCoeff cfg)
