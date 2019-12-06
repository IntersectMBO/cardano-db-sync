{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Explorer.Web.Server (runServer) where

import           Explorer.DB (Ada, Block (..),
                    queryTotalSupply, readPGPassFileEnv, toConnectionString)
import           Explorer.Web.Api (ExplorerApi, explorerApi)
import           Explorer.Web.ClientTypes (CBlockEntry (..),  CBlockSummary (..), CHash (..),
                    adaToCCoin)
import           Explorer.Web.Error (ExplorerError (..))
import           Explorer.Web.Query (queryBlockSummary)
import           Explorer.Web.Api.HttpBridge (HttpBridgeApi (..))
import           Explorer.Web.Api.HttpBridge.AddressBalance
import           Explorer.Web.Api.Legacy (ExplorerApiRecord (..))

import           Explorer.Web.Api.Legacy.AddressSummary
import           Explorer.Web.Api.Legacy.BlockAddress
import           Explorer.Web.Api.Legacy.BlockPagesTotal
import           Explorer.Web.Api.Legacy.BlocksPages
import           Explorer.Web.Api.Legacy.BlocksTxs
import           Explorer.Web.Api.Legacy.EpochPage
import           Explorer.Web.Api.Legacy.EpochSlot
import           Explorer.Web.Api.Legacy.GenesisAddress
import           Explorer.Web.Api.Legacy.GenesisPages
import           Explorer.Web.Api.Legacy.GenesisSummary
import           Explorer.Web.Api.Legacy.StatsTxs
import           Explorer.Web.Api.Legacy.TxLast
import           Explorer.Web.Api.Legacy.TxsSummary
import           Explorer.Web.Api.Legacy.Util

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Network.Wai.Handler.Warp    (run)
import           Servant                     (Application, Handler, Server, serve)
import           Servant.API.Generic         (toServant)
import           Servant.Server.Generic      (AsServerT)
import           Servant.API ((:<|>)((:<|>)))

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql (SqlBackend)


runServer :: IO ()
runServer = do
  putStrLn "Running full server on http://localhost:8100/"
  pgconfig <- readPGPassFileEnv
  runStdoutLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      liftIO $ run 8100 (explorerApp backend)

explorerApp :: SqlBackend -> Application
explorerApp backend = serve explorerApi (explorerHandlers backend)

explorerHandlers :: SqlBackend -> Server ExplorerApi
explorerHandlers backend =
    toServant oldHandlers
      :<|> toServant httpBridgeHandlers
  where
    oldHandlers = ExplorerApiRecord
      { _totalAda           = totalAda backend
      , _blocksPages        = blocksPages backend
      , _blocksPagesTotal   = blockPagesTotal backend
      , _blocksSummary      = blocksSummary backend
      , _blocksTxs          = blocksTxs backend
      , _txsLast            = getLastTxs backend
      , _txsSummary         = txsSummary backend
      , _addressSummary     = addressSummary backend
      , _epochPages         = epochPage backend
      , _epochSlots         = epochSlot backend
      , _genesisSummary     = genesisSummary backend
      , _genesisPagesTotal  = genesisPages backend
      , _genesisAddressInfo = genesisAddressInfo backend
      , _statsTxs           = statsTxs backend
      , _blockAddress       = blockAddress backend
      } :: ExplorerApiRecord (AsServerT Handler)

    httpBridgeHandlers = HttpBridgeApi
      { _addressBalance        = addressBalance backend
      } :: HttpBridgeApi (AsServerT Handler)

--------------------------------------------------------------------------------
-- sample data --
--------------------------------------------------------------------------------

totalAda :: SqlBackend -> Handler (Either ExplorerError Ada)
totalAda backend = Right <$> runQuery backend queryTotalSupply

hexToBytestring :: Text -> ExceptT ExplorerError Handler ByteString
hexToBytestring text = do
  case Base16.decode (Text.encodeUtf8 text) of
    (blob, "") -> pure blob
    (_partial, remain) -> throwE $ Internal $ "cant parse " <> Text.decodeUtf8 remain <> " as hex"

blocksSummary
    :: SqlBackend -> CHash
    -> Handler (Either ExplorerError CBlockSummary)
blocksSummary backend (CHash blkHashTxt) = runExceptT $ do
  blkHash <- hexToBytestring blkHashTxt
  liftIO $ print (blkHashTxt, blkHash)
  mBlk <- runQuery backend $ queryBlockSummary blkHash
  case mBlk of
    Just (blk, prevHash, nextHash, txCount, fees, totalOut, slh, mts) ->
      case blockSlotNo blk of
        Just slotno -> do
          let (epoch, slot) = slotno `divMod` slotsPerEpoch
          pure $ CBlockSummary
            { cbsEntry = CBlockEntry
               { cbeEpoch = epoch
               , cbeSlot = fromIntegral slot
               -- Use '0' for EBBs.
               , cbeBlkHeight = maybe 0 fromIntegral $ blockBlockNo blk
               , cbeBlkHash = CHash . bsBase16Encode $ blockHash blk
               , cbeTimeIssued = mts
               , cbeTxNum = txCount
               , cbeTotalSent = adaToCCoin totalOut
               , cbeSize = blockSize blk
               , cbeBlockLead = Just $ bsBase16Encode slh
               , cbeFees = adaToCCoin fees
               }
            , cbsPrevHash = CHash $ bsBase16Encode prevHash
            , cbsNextHash = fmap (CHash . bsBase16Encode) nextHash
            , cbsMerkleRoot = CHash $ maybe "" bsBase16Encode (blockMerkelRoot blk)
            }
        Nothing -> throwE $ Internal "slot missing"
    _ -> throwE $ Internal "No block found"
