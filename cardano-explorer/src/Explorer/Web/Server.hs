{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Explorer.Web.Server (runServer) where

import           Explorer.DB                 (blockBlockNo, blockHash
                                             , blockSize, blockSlotNo
                                             , blockMerkelRoot
                                             , readPGPassFileEnv
                                             , queryTotalSupply
                                             , Ada
                                             , toConnectionString
                                             , blockMerkelRoot
                                             , queryBlockCount
                                             , queryLatestBlockId
                                             , queryMeta
                                             , Meta(metaStartTime, metaSlotDuration, Meta)
                                             , txHash, txOutAddress, txOutValue, txFee
                                             , txOutIndex
                                             , TxOut(TxOut))
import           Explorer.Web.Api            (ExplorerApi, explorerApi)
import           Explorer.Web.ClientTypes    (CAddress (CAddress), CAddressSummary (CAddressSummary, caAddress, caBalance, caTxList, caTxNum, caType),
                                              CAddressType (CPubKeyAddress),
                                              CAddressesFilter (AllAddresses, NonRedeemedAddresses, RedeemedAddresses),
                                              CBlockEntry (CBlockEntry, cbeBlkHash, cbeBlkHeight, cbeBlockLead, cbeEpoch, cbeFees, cbeSize, cbeSlot, cbeTimeIssued, cbeTotalSent, cbeTxNum),
                                              CBlockRange (CBlockRange, cbrBlocks, cbrTransactions),
                                              CBlockSummary (CBlockSummary, cbsEntry, cbsMerkleRoot, cbsNextHash, cbsPrevHash),
                                              CGenesisAddressInfo (CGenesisAddressInfo, cgaiCardanoAddress, cgaiGenesisAmount, cgaiIsRedeemed),
                                              CGenesisSummary (CGenesisSummary, cgsNonRedeemedAmountTotal, cgsNumNotRedeemed, cgsNumRedeemed, cgsNumTotal, cgsRedeemedAmountTotal),
                                              CHash (CHash)
                                              , CCoin
                                              , CTxBrief (CTxBrief, ctbId, ctbInputSum, ctbInputs, ctbOutputSum, ctbOutputs, ctbTimeIssued),
                                              CTxEntry (CTxEntry, cteAmount, cteId, cteTimeIssued),
                                              CTxHash, CTxHash (CTxHash),
                                              CTxSummary (CTxSummary, ctsBlockEpoch, ctsBlockHash, ctsBlockHeight, ctsBlockSlot, ctsBlockTimeIssued, ctsFees, ctsId, ctsInputs, ctsOutputs, ctsRelayedBy, ctsTotalInput, ctsTotalOutput, ctsTxTimeIssued),
                                              CUtxo (CUtxo, cuAddress, cuCoins, cuId, cuOutIndex),
                                              mkCCoin, adaToCCoin)
import           Explorer.Web.Error          (ExplorerError (Internal))
import           Explorer.Web.Query          (queryBlockSummary, queryTxSummary, queryBlockTxs, TxWithInputsOutputs(txwTx, txwInputs, txwOutputs, TxWithInputsOutputs), queryBlockIdFromHeight, queryUtxoSnapshot)
import           Explorer.Web.API1 (ExplorerApi1Record(ExplorerApi1Record,_utxoHeight, _utxoHash), V1Utxo(V1Utxo))
import qualified Explorer.Web.API1 as API1
import           Explorer.Web.LegacyApi      (ExplorerApiRecord (_genesisSummary, _genesisAddressInfo, _genesisPagesTotal, _epochPages, _epochSlots, _statsTxs, _txsSummary, _addressSummary, _addressUtxoBulk, _blocksSummary, _blocksTxs, _txsLast, _dumpBlockRange, _totalAda, _blocksPages, _blocksPagesTotal, ExplorerApiRecord), TxsStats, PageNumber)

import           Cardano.Chain.Slotting      (EpochNumber (EpochNumber))

import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT)
import           Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)
import           Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Char8       as SB8
import qualified Data.Text                   as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time                   (defaultTimeLocale, addUTCTime, parseTimeOrError)
import           Data.Time.Clock.POSIX       (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word                   (Word16, Word64)
import           Data.Int (Int64)
import           Network.Wai.Handler.Warp    (run)
import           Servant                     (Application, Handler, Server, serve)
import           Servant.API.Generic         (toServant)
import           Servant.Server.Generic      (AsServerT)
import           Servant.API ((:<|>)((:<|>)))

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql        (SqlBackend, runSqlConn)

-- TODO, get this from the config somehow
k :: Word64
k = 2160

epochLenght = k * 10

runServer :: IO ()
runServer = do
  putStrLn "running full server on http://localhost:8100/"
  pgconfig <- readPGPassFileEnv
  runStdoutLoggingT .
    withPostgresqlConn (toConnectionString pgconfig) $ \backend ->
      liftIO $ run 8100 (explorerApp backend)

explorerApp :: SqlBackend -> Application
explorerApp backend = serve explorerApi (explorerHandlers backend)

explorerHandlers :: SqlBackend -> Server ExplorerApi
explorerHandlers backend = (toServant oldHandlers) :<|> (toServant newHandlers)
  where
    oldHandlers = ExplorerApiRecord
      { _totalAda           = totalAda backend
      , _dumpBlockRange     = testDumpBlockRange backend
      , _blocksPages        = testBlocksPages backend
      , _blocksPagesTotal   = getBlocksPagesTotal backend
      , _blocksSummary      = blocksSummary backend
      , _blocksTxs          = getBlockTxs backend
      , _txsLast            = getLastTxs backend
      , _txsSummary         = testTxsSummary backend
      , _addressSummary     = testAddressSummary backend
      , _addressUtxoBulk    = testAddressUtxoBulk backend
      , _epochPages         = testEpochPageSearch backend
      , _epochSlots         = testEpochSlotSearch backend
      , _genesisSummary     = testGenesisSummary backend
      , _genesisPagesTotal  = testGenesisPagesTotal backend
      , _genesisAddressInfo = testGenesisAddressInfo backend
      , _statsTxs           = testStatsTxs backend
      } :: ExplorerApiRecord (AsServerT Handler)
    newHandlers = ExplorerApi1Record
      { _utxoHeight         = getUtxoSnapshotHeight backend
      , _utxoHash           = getUtxoSnapshotHash
      } :: ExplorerApi1Record (AsServerT Handler)

--------------------------------------------------------------------------------
-- sample data --
--------------------------------------------------------------------------------
cTxId :: CTxHash
cTxId = CTxHash $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

cTxEntry :: CTxEntry
cTxEntry = CTxEntry
    { cteId         = cTxId
    , cteTimeIssued = Just posixTime
    , cteAmount     = mkCCoin 33333
    }

runQuery :: MonadIO m => SqlBackend -> ReaderT SqlBackend IO a -> m a
runQuery backend query = liftIO $ runSqlConn query backend

totalAda :: SqlBackend -> Handler (Either ExplorerError Ada)
totalAda backend = Right <$> runQuery backend queryTotalSupply

testDumpBlockRange :: SqlBackend -> CHash -> CHash -> Handler (Either ExplorerError CBlockRange)
testDumpBlockRange backend start _ = do
  edummyBlock <- blocksSummary backend start
  edummyTx <- testTxsSummary backend cTxId
  case (edummyBlock,edummyTx) of
    (Right dummyBlock, Right dummyTx) ->
      pure $ Right $ CBlockRange
        { cbrBlocks = [ dummyBlock ]
        , cbrTransactions = [ dummyTx ]
        }
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

testBlocksPages
    :: SqlBackend -> Maybe PageNumber
    -> Maybe Word
    -> Handler (Either ExplorerError (PageNumber, [CBlockEntry]))
testBlocksPages _backend _ _  = pure $ Right (1, [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHeight  = 1564738
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin 0
    }])

divRoundUp :: Integral a => a -> a -> a
divRoundUp a b = (a + b - 1) `div` b

defaultPageSize :: Word
defaultPageSize = 10

toPageSize :: Maybe Word -> Word
toPageSize = fromMaybe defaultPageSize

-- | A pure calculation of the page number.
-- Get total pages from the blocks. And we want the page
-- with the example, the page size 10,
-- to start with 10 + 1 == 11, not with 10 since with
-- 10 we'll have an empty page.
-- Could also be `((blocksTotal - 1) `div` pageSizeInt) + 1`.
roundToBlockPage :: Word -> Word
roundToBlockPage blocksTotal = divRoundUp blocksTotal defaultPageSize

getBlocksPagesTotal
    :: SqlBackend -> Maybe Word
    -> Handler (Either ExplorerError Word)
getBlocksPagesTotal backend mPageSize = do
  blocksTotal <- runQuery backend queryBlockCount
  liftIO $ print blocksTotal
  let pageSize = toPageSize mPageSize
  case (blocksTotal < 1, pageSize < 1) of
    (True, _) -> pure $ Left $ Internal "There are currently no block to display."
    (_, True) -> pure $ Left $ Internal "Page size must be greater than 1 if you want to display blocks."
    _ -> pure $ Right $ roundToBlockPage blocksTotal

hexToBytestring :: T.Text -> ExceptT ExplorerError Handler BS.ByteString
hexToBytestring text = do
  case B16.decode (SB8.pack (T.unpack text)) of
    (blob, "") -> pure blob
    (_partial, remain) -> throwE $ Internal $ "cant parse " <> (decodeUtf8 remain) <> " as hex"

blocksSummary
    :: SqlBackend -> CHash
    -> Handler (Either ExplorerError CBlockSummary)
blocksSummary backend (CHash blkHashTxt) = runExceptT $ do
  blkHash <- hexToBytestring blkHashTxt
  liftIO $ print blkHash
  mBlk <- runQuery backend (queryBlockSummary blkHash)
  case mBlk of
    Just (blk, Just prevHash, nextHash, txCount, fees, totalOut) ->
      case blockSlotNo blk of
        Just slotno -> do
          let
            (epoch, slot) = divMod slotno epochLenght
          pure $ CBlockSummary
            { cbsEntry = CBlockEntry
               { cbeEpoch = epoch
               , cbeSlot = fromIntegral slot
               -- Use '0' for EBBs.
               , cbeBlkHeight = maybe 0 fromIntegral $ blockBlockNo blk
               , cbeBlkHash = (CHash . decodeUtf8 . blockHash) blk
               , cbeTimeIssued = Nothing
               , cbeTxNum = txCount
               , cbeTotalSent = adaToCCoin totalOut
               , cbeSize = blockSize blk
               , cbeBlockLead = Nothing
               , cbeFees = adaToCCoin fees
               }
            , cbsPrevHash = (CHash . decodeUtf8) prevHash
            , cbsNextHash = fmap (CHash . decodeUtf8) nextHash
            , cbsMerkleRoot = CHash $ maybe "" decodeUtf8 (blockMerkelRoot blk)
            }
        Nothing -> throwE $ Internal "slot missing"
    _ -> throwE $ Internal "No block found"

convertTxOut :: TxOut -> (CAddress, CCoin)
convertTxOut TxOut{txOutAddress,txOutValue} = (CAddress txOutAddress, mkCCoin $ fromIntegral txOutValue)

convertInput :: (T.Text, Word64) -> Maybe (CAddress, CCoin)
convertInput (addr, coin) = Just (CAddress $ addr, mkCCoin $ fromIntegral coin)

getBlockTxs
    :: SqlBackend -> CHash
    -> Maybe Int64
    -> Maybe Int64
    -> Handler (Either ExplorerError [CTxBrief])
getBlockTxs backend (CHash blkHashTxt) mLimit mOffset = runExceptT $ do
  let
    limit = fromMaybe 10 mLimit
    offset = fromMaybe 0 mOffset
  blkHash <- hexToBytestring blkHashTxt
  (txList, eMeta, mSlot) <- runQuery backend $ do
    (txList, slot) <- queryBlockTxs blkHash limit offset
    meta <- queryMeta
    pure (txList, meta, slot)
  case eMeta of
    Left _err -> throwE $ Internal "error reading metadata"
    Right (Meta{metaSlotDuration,metaStartTime}) -> do
      let
        txToTxBrief :: TxWithInputsOutputs -> CTxBrief
        txToTxBrief TxWithInputsOutputs{txwTx,txwInputs,txwOutputs} = CTxBrief
          { ctbId = (CTxHash . CHash . decodeUtf8 . txHash) txwTx
          , ctbTimeIssued = (\slot -> utcTimeToPOSIXSeconds $ (0.001 * fromIntegral (slot * metaSlotDuration)) `addUTCTime` metaStartTime) <$> mSlot
          , ctbInputs = map convertInput txwInputs
          , ctbOutputs = map convertTxOut txwOutputs
          , ctbInputSum = (mkCCoin . sum . map (\(_addr,coin) -> fromIntegral  coin)) txwInputs
          , ctbOutputSum = (mkCCoin . sum . map (fromIntegral . txOutValue)) txwOutputs
          }
      pure $ map txToTxBrief txList

getLastTxs :: SqlBackend -> Handler (Either ExplorerError [CTxEntry])
getLastTxs _backend = pure $ Right [cTxEntry]

testTxsSummary
    :: SqlBackend -> CTxHash
    -> Handler (Either ExplorerError CTxSummary)
testTxsSummary backend (CTxHash (CHash cTxHash)) = runExceptT $ do
  blob <- hexToBytestring cTxHash
  liftIO $ print blob
  mTxblk <- runQuery backend $ queryTxSummary blob
  case mTxblk of
    Nothing -> throwE $ Internal "tx not found" -- TODO, give the same error as before?
    Just (tx, blk, inputs, outputs) -> do
      case blockSlotNo blk of
        Just slotno -> do
          let
            (epoch, slot) = divMod slotno epochLenght
          pure $ CTxSummary
            { ctsId              = (CTxHash . CHash . decodeUtf8 . txHash) tx
            , ctsTxTimeIssued    = Just posixTime
            , ctsBlockTimeIssued = Nothing
            , ctsBlockHeight     = fromIntegral <$> blockBlockNo blk
            , ctsBlockEpoch      = Just epoch
            , ctsBlockSlot       = Just $ fromIntegral slot
            , ctsBlockHash       = (Just . CHash . decodeUtf8 . blockHash) blk
            , ctsRelayedBy       = Nothing
            , ctsTotalInput      = (mkCCoin . sum . map (\(_addr,coin) -> fromIntegral  coin)) inputs
            , ctsTotalOutput     = (mkCCoin . sum . map (fromIntegral . txOutValue)) outputs
            , ctsFees            = mkCCoin $ fromIntegral $ txFee tx
            , ctsInputs          = map convertInput inputs
            , ctsOutputs         = map convertTxOut outputs
            }
        Nothing -> throwE $ Internal "cant find slot# of block"

sampleAddressSummary :: CAddressSummary
sampleAddressSummary = CAddressSummary
    { caAddress = CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv"
    , caType    = CPubKeyAddress
    , caTxNum   = 0
    , caBalance = mkCCoin 0
    , caTxList  = []
    }

testAddressSummary
    :: SqlBackend -> CAddress
    -> Handler (Either ExplorerError CAddressSummary)
testAddressSummary _backend _  = pure $ Right sampleAddressSummary

testAddressUtxoBulk
    :: SqlBackend -> [CAddress]
    -> Handler (Either ExplorerError [CUtxo])
testAddressUtxoBulk _backend _  = pure $ Right [CUtxo
    { cuId = CTxHash $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , cuOutIndex = 0
    , cuAddress = CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN"
    , cuCoins = mkCCoin 3
    }]

testEpochSlotSearch
    :: SqlBackend -> EpochNumber
    -> Word16
    -> Handler (Either ExplorerError [CBlockEntry])
-- `?epoch=1&slot=1` returns an empty list
testEpochSlotSearch _backend (EpochNumber 1) 1 =
    pure $ Right []
-- `?epoch=1&slot=2` returns an error
testEpochSlotSearch _backend (EpochNumber 1) 2 =
    pure $ Left $ Internal "Error while searching epoch/slot"
-- all others returns a simple result
testEpochSlotSearch _backend _ _ = pure $ Right [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHeight  = 1564738
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin 0
    }]

testEpochPageSearch
    :: SqlBackend -> EpochNumber
    -> Maybe Int
    -> Handler (Either ExplorerError (Int, [CBlockEntry]))
testEpochPageSearch _backend _ _ = pure $ Right (1, [CBlockEntry
    { cbeEpoch      = 37294
    , cbeSlot       = 10
    , cbeBlkHeight  = 1564738
    , cbeBlkHash    = CHash "75aa93bfa1bf8e6aa913bc5fa64479ab4ffc1373a25c8176b61fa1ab9cbae35d"
    , cbeTimeIssued = Just posixTime
    , cbeTxNum      = 0
    , cbeTotalSent  = mkCCoin 0
    , cbeSize       = 390
    , cbeBlockLead  = Nothing
    , cbeFees       = mkCCoin 0
    }])

testGenesisSummary
    :: SqlBackend -> Handler (Either ExplorerError CGenesisSummary)
testGenesisSummary _backend = pure $ Right CGenesisSummary
    { cgsNumTotal       = 4
    , cgsNumRedeemed    = 3
    , cgsNumNotRedeemed = 1
    , cgsRedeemedAmountTotal    = mkCCoin 300000000
    , cgsNonRedeemedAmountTotal = mkCCoin 100000000
    }

testGenesisPagesTotal
    :: SqlBackend -> Maybe PageNumber
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError PageNumber)
-- number of redeemed addresses pages
testGenesisPagesTotal _backend _ (Just RedeemedAddresses)    = pure $ Right 1
-- number of non redeemed addresses pages
testGenesisPagesTotal _backend _ (Just NonRedeemedAddresses) = pure $ Right 1
-- number of all redeem addresses pages
testGenesisPagesTotal _backend _ _                           = pure $ Right 2

-- mock CGenesisAddressInfo
gAddressInfoA :: CGenesisAddressInfo
gAddressInfoA = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "3mfaPhQ8ewtmyi7tvcxo1TXhGh5piePbjkqgz49Jo2wpV9"
    , cgaiGenesisAmount    = mkCCoin 2225295000000
    , cgaiIsRedeemed       = True
    }

-- mock another CGenesisAddressInfo
gAddressInfoB :: CGenesisAddressInfo
gAddressInfoB = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "3meLwrCDE4C7RofEdkZbUuR75ep3EcTmZv9ebcdjfMtv5H"
    , cgaiGenesisAmount    = mkCCoin 15000000
    , cgaiIsRedeemed       = False
    }

-- mock another CGenesisAddressInfo
gAddressInfoC :: CGenesisAddressInfo
gAddressInfoC = CGenesisAddressInfo
    { cgaiCardanoAddress   = CAddress "LaVWbkFegK1TUNHMc3Fads2cG6ivPb2gJUxXBxNtumLtbG"
    , cgaiGenesisAmount    = mkCCoin 333000000
    , cgaiIsRedeemed       = False
    }

testGenesisAddressInfo
    :: SqlBackend -> Maybe Word
    -> Maybe Word
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError [CGenesisAddressInfo])
-- filter redeemed addresses
testGenesisAddressInfo _backend _ _ (Just RedeemedAddresses)    = pure $ Right [ gAddressInfoA ]
-- filter non-redeemed addresses
testGenesisAddressInfo _backend _ _ (Just NonRedeemedAddresses) = pure $ Right [ gAddressInfoB, gAddressInfoC ]
-- all addresses (w/o filtering) - page 1
testGenesisAddressInfo _backend (Just 1) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoA, gAddressInfoB ]
testGenesisAddressInfo _backend (Just 1) _ Nothing              = pure $ Right [ gAddressInfoA, gAddressInfoB ]
-- all addresses (w/o filtering) - page 2
testGenesisAddressInfo _backend (Just 2) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoC ]
testGenesisAddressInfo _backend (Just 2) _ Nothing              = pure $ Right [ gAddressInfoC ]
-- all others requests will ended up with an error
testGenesisAddressInfo _backend _ _ _ =  pure $ Left $ Internal "Error while pagening genesis addresses"

testStatsTxs
    :: SqlBackend -> Maybe Word
    -> Handler (Either ExplorerError TxsStats)
testStatsTxs _backend _ = pure $ Right (1, [(cTxId, 200)])

getUtxoSnapshotHeight :: SqlBackend -> Maybe Word64 -> Handler (Either ExplorerError [V1Utxo])
getUtxoSnapshotHeight backend mHeight = runExceptT $ do
  liftIO $ putStrLn "getting snapshot by height"
  outputs <- ExceptT <$> runQuery backend $ do
    mBlkid <- case mHeight of
      Just height -> queryBlockIdFromHeight height
      Nothing -> queryLatestBlockId
    case mBlkid of
      Just blkid -> Right <$> queryUtxoSnapshot blkid
      Nothing -> pure $ Left $ Internal "block not found at given height"
  let
    convertRow :: (TxOut, BS.ByteString) -> V1Utxo
    convertRow (txout, txhash) = V1Utxo
      { API1.cuId = (CTxHash . CHash . decodeUtf8) txhash
      , API1.cuOutIndex = txOutIndex txout
      , API1.cuAddress = (CAddress . txOutAddress) txout
      , API1.cuCoins = (mkCCoin . fromIntegral . txOutValue) txout
      }
  pure $ map convertRow outputs

getUtxoSnapshotHash :: Maybe CHash -> Handler (Either ExplorerError [V1Utxo])
getUtxoSnapshotHash _ = runExceptT $ do
  liftIO $ putStrLn "getting snapshot by hash"
  -- queryBlockId
  pure []
