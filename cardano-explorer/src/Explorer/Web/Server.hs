{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Server (runServer) where

import           Explorer.DB                 (blockBlockNo, blockHash,
                                              blockSize, blockSlotNo,
                                              readPGPassFileEnv
                                              , queryTotalSupply
                                              , Ada
                                              , toConnectionString)
import           Explorer.Web.Api            (ExplorerApi, explorerApi)
import           Explorer.Web.ClientTypes    (CAddress (CAddress), CAddressSummary (CAddressSummary, caAddress, caBalance, caTxList, caTxNum, caType),
                                              CAddressType (CPubKeyAddress),
                                              CAddressesFilter (AllAddresses, NonRedeemedAddresses, RedeemedAddresses),
                                              CBlockEntry (CBlockEntry, cbeBlkHash, cbeBlkHeight, cbeBlockLead, cbeEpoch, cbeFees, cbeSize, cbeSlot, cbeTimeIssued, cbeTotalSent, cbeTxNum),
                                              CBlockRange (CBlockRange, cbrBlocks, cbrTransactions),
                                              CBlockSummary (CBlockSummary, cbsEntry, cbsMerkleRoot, cbsNextHash, cbsPrevHash),
                                              CGenesisAddressInfo (CGenesisAddressInfo, cgaiCardanoAddress, cgaiGenesisAmount, cgaiIsRedeemed),
                                              CGenesisSummary (CGenesisSummary, cgsNonRedeemedAmountTotal, cgsNumNotRedeemed, cgsNumRedeemed, cgsNumTotal, cgsRedeemedAmountTotal),
                                              CHash (CHash),
                                              CTxBrief (CTxBrief, ctbId, ctbInputSum, ctbInputs, ctbOutputSum, ctbOutputs, ctbTimeIssued),
                                              CTxEntry (CTxEntry, cteAmount, cteId, cteTimeIssued),
                                              CTxHash, CTxHash (CTxHash),
                                              CTxSummary (CTxSummary, ctsBlockEpoch, ctsBlockHash, ctsBlockHeight, ctsBlockSlot, ctsBlockTimeIssued, ctsFees, ctsId, ctsInputs, ctsOutputs, ctsRelayedBy, ctsTotalInput, ctsTotalOutput, ctsTxTimeIssued),
                                              CUtxo (CUtxo, cuAddress, cuCoins, cuId, cuOutIndex),
                                              mkCCoin)
import           Explorer.Web.Error          (ExplorerError (Internal))
import           Explorer.Web.LegacyApi      (ExplorerApiRecord (..), TxsStats)
import           Explorer.Web.Query          (queryBlockSummary)

import           Cardano.Chain.Slotting      (EpochNumber (EpochNumber))

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStdoutLoggingT)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Char8       as SB8
import qualified Data.Text                   as T
import           Data.Time                   (defaultTimeLocale,
                                              parseTimeOrError)
import           Data.Time.Clock.POSIX       (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word                   (Word16)
import           Network.Wai.Handler.Warp    (run)
import           Servant                     (Application, Handler, Server,
                                              serve)
import           Servant.API.Generic         (toServant)
import           Servant.Server.Generic      (AsServerT)

import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Sql        (SqlBackend, runSqlConn)

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
explorerHandlers backend = toServant (ExplorerApiRecord
  { _totalAda           = totalAda backend
  , _dumpBlockRange     = testDumpBlockRange backend
  , _blocksPages        = testBlocksPages backend
  , _blocksPagesTotal   = testBlocksPagesTotal backend
  , _blocksSummary      = blocksSummary backend
  , _blocksTxs          = testBlocksTxs backend
  , _txsLast            = testTxsLast backend
  , _txsSummary         = testTxsSummary backend
  , _addressSummary     = testAddressSummary backend
  , _addressUtxoBulk    = testAddressUtxoBulk backend
  , _epochPages         = testEpochPageSearch backend
  , _epochSlots         = testEpochSlotSearch backend
  , _genesisSummary     = testGenesisSummary backend
  , _genesisPagesTotal  = testGenesisPagesTotal backend
  , _genesisAddressInfo = testGenesisAddressInfo backend
  , _statsTxs           = testStatsTxs backend
  } :: ExplorerApiRecord (AsServerT Handler))

--------------------------------------------------------------------------------
-- sample data --
--------------------------------------------------------------------------------
cTxId :: CTxHash
cTxId = CTxHash $ CHash "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

posixTime :: POSIXTime
posixTime = utcTimeToPOSIXSeconds (parseTimeOrError True defaultTimeLocale "%F" "2017-12-03")

cTxBrief :: CTxBrief
cTxBrief = CTxBrief
    { ctbId         = cTxId
    , ctbTimeIssued = Just posixTime
    , ctbInputs     = [Just (CAddress "1fi9sA3pRt8bKVibdun57iyWG9VsWZscgQigSik6RHoF5Mv", mkCCoin 33333), Nothing]
    , ctbOutputs    = [(CAddress "1fSCHaQhy6L7Rfjn9xR2Y5H7ZKkzKLMXKYLyZvwWVffQwkQ", mkCCoin 33333)]
    , ctbInputSum   = mkCCoin 33333
    , ctbOutputSum  = mkCCoin 33333
    }

cTxEntry :: CTxEntry
cTxEntry = CTxEntry
    { cteId         = cTxId
    , cteTimeIssued = Just posixTime
    , cteAmount     = mkCCoin 33333
    }

runQuery :: SqlBackend -> ReaderT SqlBackend IO a -> Handler a
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
    :: SqlBackend -> Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError (Integer, [CBlockEntry]))
testBlocksPages backend _ _  = pure $ Right (1, [CBlockEntry
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

testBlocksPagesTotal
    :: SqlBackend -> Maybe Word
    -> Handler (Either ExplorerError Integer)
testBlocksPagesTotal backend _ = pure $ Right 10

blocksSummary
    :: SqlBackend -> CHash
    -> Handler (Either ExplorerError CBlockSummary)
blocksSummary backend (CHash blkHashTxt) =
  case B16.decode (SB8.pack (T.unpack blkHashTxt)) of
              (blob, "") -> do
                liftIO $ print blob
                mBlk <- runQuery backend (queryBlockSummary blob)
                pure $ case mBlk of
                  Just (blk, Just prevHash, nextHash) ->
                    case blockSlotNo blk of
                      Just slotno -> do
                        let
                          (epoch, slot) = divMod slotno 21600 -- TODO, get it from the config
                        Right $ CBlockSummary
                          { cbsEntry = CBlockEntry
                             { cbeEpoch = epoch
                             , cbeSlot = fromIntegral slot
                             , cbeBlkHeight = fromIntegral $ blockBlockNo blk
                             , cbeBlkHash = CHash $ T.pack $ SB8.unpack $ B16.encode $ blockHash blk
                             , cbeTimeIssued = Nothing
                             , cbeTxNum = 0
                             , cbeTotalSent = mkCCoin 0
                             , cbeSize = blockSize blk
                             , cbeBlockLead = Nothing
                             , cbeFees = mkCCoin 0
                             }
                          , cbsPrevHash = CHash $ T.pack $ SB8.unpack $ B16.encode prevHash
                          , cbsNextHash = fmap (CHash . T.pack . SB8.unpack . B16.encode) nextHash
                          , cbsMerkleRoot = CHash ""
                          }
                      Nothing -> Left $ Internal "slot missing"
                  _ -> Left $ Internal "No block found"
              _ -> pure $ Left $ Internal "imposible!"

testBlocksTxs
    :: SqlBackend -> CHash
    -> Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError [CTxBrief])
testBlocksTxs backend _ _ _ = pure $ Right [cTxBrief]

testTxsLast :: SqlBackend -> Handler (Either ExplorerError [CTxEntry])
testTxsLast backend = pure $ Right [cTxEntry]

testTxsSummary
    :: SqlBackend -> CTxHash
    -> Handler (Either ExplorerError CTxSummary)
testTxsSummary backend _       = pure $ Right CTxSummary
    { ctsId              = CTxHash $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , ctsTxTimeIssued    = Just posixTime
    , ctsBlockTimeIssued = Nothing
    , ctsBlockHeight     = Just 13
    , ctsBlockEpoch      = Just 0
    , ctsBlockSlot       = Just 13
    , ctsBlockHash       = Just $ CHash "a9dea19829e80d9064cd0c33dccf5369638e43c62a090848342037e296120a35"
    , ctsRelayedBy       = Nothing
    , ctsTotalInput      = mkCCoin 33333
    , ctsTotalOutput     = mkCCoin 33333
    , ctsFees            = mkCCoin 0
    , ctsInputs          =  [ Just (CAddress "19HxN7PseAPT93RftAh7bBmbnJU5gtH6QzvUyZXnbz9Y1UtYwPDdiCGkB2gwvC8CjBUtHXBij9j9Qb6JYgHPi6LtevDcFQ", mkCCoin 97)
                            , Nothing
                            , Just (CAddress "LaVWPVaMHxNVtqJ1uvVZ8FyQmeRam5avHE1Uv9iwRivCKTN83CUW", mkCCoin 3333)
                            ]
    , ctsOutputs         =  [ (CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN", mkCCoin 94)
                            , (CAddress "1feqWtoyaxFyvKQFWo46vHSc7urynGaRELQE62T74Y3RBs8", mkCCoin 3)
                            ]
    }

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
testAddressSummary backend _  = pure $ Right sampleAddressSummary

testAddressUtxoBulk
    :: SqlBackend -> [CAddress]
    -> Handler (Either ExplorerError [CUtxo])
testAddressUtxoBulk backend _  = pure $ Right [CUtxo
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
testEpochSlotSearch backend (EpochNumber 1) 1 =
    pure $ Right []
-- `?epoch=1&slot=2` returns an error
testEpochSlotSearch backend (EpochNumber 1) 2 =
    pure $ Left $ Internal "Error while searching epoch/slot"
-- all others returns a simple result
testEpochSlotSearch backend _ _ = pure $ Right [CBlockEntry
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
testEpochPageSearch backend _ _ = pure $ Right (1, [CBlockEntry
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
testGenesisSummary backend = pure $ Right CGenesisSummary
    { cgsNumTotal       = 4
    , cgsNumRedeemed    = 3
    , cgsNumNotRedeemed = 1
    , cgsRedeemedAmountTotal    = mkCCoin 300000000
    , cgsNonRedeemedAmountTotal = mkCCoin 100000000
    }

testGenesisPagesTotal
    :: SqlBackend -> Maybe Word
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError Integer)
-- number of redeemed addresses pages
testGenesisPagesTotal backend _ (Just RedeemedAddresses)    = pure $ Right 1
-- number of non redeemed addresses pages
testGenesisPagesTotal backend _ (Just NonRedeemedAddresses) = pure $ Right 1
-- number of all redeem addresses pages
testGenesisPagesTotal backend _ _                           = pure $ Right 2

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
testGenesisAddressInfo backend _ _ (Just RedeemedAddresses)    = pure $ Right [ gAddressInfoA ]
-- filter non-redeemed addresses
testGenesisAddressInfo backend _ _ (Just NonRedeemedAddresses) = pure $ Right [ gAddressInfoB, gAddressInfoC ]
-- all addresses (w/o filtering) - page 1
testGenesisAddressInfo backend (Just 1) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoA, gAddressInfoB ]
testGenesisAddressInfo backend (Just 1) _ Nothing              = pure $ Right [ gAddressInfoA, gAddressInfoB ]
-- all addresses (w/o filtering) - page 2
testGenesisAddressInfo backend (Just 2) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoC ]
testGenesisAddressInfo backend (Just 2) _ Nothing              = pure $ Right [ gAddressInfoC ]
-- all others requests will ended up with an error
testGenesisAddressInfo backend _ _ _ =  pure $ Left $ Internal "Error while pagening genesis addresses"

testStatsTxs
    :: SqlBackend -> Maybe Word
    -> Handler (Either ExplorerError TxsStats)
testStatsTxs backend _ = pure $ Right (1, [(cTxId, 200)])
