{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Sync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncDataLayer (..)
  , mkSyncEnvFromConfig
  , getLatestPoints
  ) where

import           Cardano.Prelude (Proxy (..), catMaybes, find)

import           Cardano.Sync.Config.Cardano
import           Cardano.Sync.Config.Shelley
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Types
import           Cardano.Sync.Util (textShow)

import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto.ProtocolMagic

import           Control.Concurrent.STM.TVar

import           Data.ByteString (ByteString)

import           Ouroboros.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point

import qualified Shelley.Spec.Ledger.BaseTypes as Shelley
import qualified Shelley.Spec.Ledger.Genesis as Shelley

data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envDataLayer :: SyncDataLayer
  , envInterpreter :: TVar (Maybe CardanoInterpreter)
  , envLedger :: LedgerEnv
  }

-- The base @DataLayer@ that contains the functions required for syncing to work.
data SyncDataLayer = SyncDataLayer
  { sdlGetSlotHash :: SlotNo -> IO [(SlotNo, ByteString)]
  , sdlGetLatestBlock :: IO (Maybe Block)
  , sdlGetLatestSlotNo :: IO SlotNo
  }

mkSyncEnv :: SyncDataLayer -> ProtocolInfo IO CardanoBlock -> Shelley.Network -> NetworkMagic -> SystemStart -> LedgerStateDir -> IO SyncEnv
mkSyncEnv dataLayer protocolInfo network networkMagic systemStart dir = do
  latestSlot <- sdlGetLatestSlotNo dataLayer
  ledgerEnv <- mkLedgerEnv protocolInfo dir network latestSlot True
  varInter <- newTVarIO Nothing
  pure $ SyncEnv
          { envProtocol = SyncProtocolCardano
          , envNetworkMagic = networkMagic
          , envSystemStart = systemStart
          , envDataLayer = dataLayer
          , envInterpreter = varInter
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: SyncDataLayer -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig dataLayer dir genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", textShow (Shelley.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise -> Right <$> mkSyncEnv
                         dataLayer
                         (mkProtocolInfoCardano genCfg)
                         (Shelley.sgNetworkId (scConfig sCfg))
                         (NetworkMagic (unProtocolMagicId $ Byron.configProtocolMagicId bCfg))
                         (SystemStart (Byron.gdStartTime $ Byron.configGenesisData bCfg))
                         dir

getLatestPoints :: SyncEnv -> IO [CardanoPoint]
getLatestPoints env = do
    files <- listLedgerStateFilesOrdered $ leDir $ envLedger env
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe CardanoPoint)
    validLedgerFileToPoint lsf = do
        hashes <- sdlGetSlotHash (envDataLayer env) (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convert (slot, hash)
          _ -> pure Nothing

    convert :: (SlotNo, ByteString) -> Maybe CardanoPoint
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)
