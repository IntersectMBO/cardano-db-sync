{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.TestServer where

import           Cardano.Chain.Slotting   (EpochNumber (EpochNumber))
import           Data.Time                (defaultTimeLocale, parseTimeOrError)
import           Data.Time.Clock.POSIX    (POSIXTime, utcTimeToPOSIXSeconds)
import           Data.Word
import           Explorer.Web.Api
import           Explorer.Web.ClientTypes
import           Explorer.Web.Error       (ExplorerError (Internal))
import           Explorer.Web.LegacyApi
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API.Generic      (toServant)
import           Servant.Server.Generic

runMockServer :: IO ()
runMockServer = do
  putStrLn "running on http://localhost:8100/"
  run 8100 explorerApp

explorerApp :: Application
explorerApp = serve explorerApi explorerHandlers

explorerHandlers :: Server ExplorerApi
explorerHandlers = toServant (ExplorerApiRecord
  { _totalAda           = testTotalAda
  , _dumpBlockRange     = testDumpBlockRange
  , _blocksPages        = testBlocksPages
  , _blocksPagesTotal   = testBlocksPagesTotal
  , _blocksSummary      = testBlocksSummary
  , _blocksTxs          = testBlocksTxs
  , _txsLast            = testTxsLast
  , _txsSummary         = testTxsSummary
  , _addressSummary     = testAddressSummary
  , _addressUtxoBulk    = testAddressUtxoBulk
  , _epochPages         = testEpochPageSearch
  , _epochSlots         = testEpochSlotSearch
  , _genesisSummary     = testGenesisSummary
  , _genesisPagesTotal  = testGenesisPagesTotal
  , _genesisAddressInfo = testGenesisAddressInfo
  , _statsTxs           = testStatsTxs
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

testTotalAda :: Handler (Either ExplorerError CAda)
testTotalAda = pure $ Right $ CAda 123.456789

testDumpBlockRange :: CHash -> CHash -> Handler (Either ExplorerError CBlockRange)
testDumpBlockRange start _ = do
  edummyBlock <- testBlocksSummary start
  edummyTx <- testTxsSummary cTxId
  case (edummyBlock,edummyTx) of
    (Right dummyBlock, Right dummyTx) ->
      pure $ Right $ CBlockRange
        { cbrBlocks = [ dummyBlock ]
        , cbrTransactions = [ dummyTx ]
        }
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left err

testBlocksPages
    :: Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError (Integer, [CBlockEntry]))
testBlocksPages _ _  = pure $ Right (1, [CBlockEntry
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
    :: Maybe Word
    -> Handler (Either ExplorerError Integer)
testBlocksPagesTotal _ = pure $ Right 10

testBlocksSummary
    :: CHash
    -> Handler (Either ExplorerError CBlockSummary)
testBlocksSummary _ = pure $ Right CBlockSummary
    { cbsEntry      = CBlockEntry
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
                        }
    , cbsPrevHash   = CHash "d36710c918da4c4a3e0ff42e1049d81cc7bcbacc789c8583ea1c9afd8da3c24e"
    , cbsNextHash   = Just (CHash "d3bb988e57356b706f7b8f1fe29591ab0d1bdfac4aa08836475783973e4cf7c1")
    , cbsMerkleRoot = CHash "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9"
    }

testBlocksTxs
    :: CHash
    -> Maybe Word
    -> Maybe Word
    -> Handler (Either ExplorerError [CTxBrief])
testBlocksTxs _ _ _ = pure $ Right [cTxBrief]

testTxsLast :: Handler (Either ExplorerError [CTxEntry])
testTxsLast = pure $ Right [cTxEntry]

testTxsSummary
    :: CTxHash
    -> Handler (Either ExplorerError CTxSummary)
testTxsSummary _       = pure $ Right CTxSummary
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
    :: CAddress
    -> Handler (Either ExplorerError CAddressSummary)
testAddressSummary _  = pure $ Right sampleAddressSummary

testAddressUtxoBulk
    :: [CAddress]
    -> Handler (Either ExplorerError [CUtxo])
testAddressUtxoBulk _  = pure $ Right [CUtxo
    { cuId = CTxHash $ CHash "8aac4a6b18fafa2783071c66519332157ce96c67e88fc0cc3cb04ba0342d12a1"
    , cuOutIndex = 0
    , cuAddress = CAddress "19F6U1Go5B4KakVoCZfzCtqNAWhUBprxVzL3JsGu74TEwQnXPvAKPUbvG8o4Qe5RaY8Z7WKLfxmNFwBqPV1NQ2hRpKkdEN"
    , cuCoins = mkCCoin 3
    }]

testEpochSlotSearch
    :: EpochNumber
    -> Word16
    -> Handler (Either ExplorerError [CBlockEntry])
-- `?epoch=1&slot=1` returns an empty list
testEpochSlotSearch (EpochNumber 1) 1 =
    pure $ Right []
-- `?epoch=1&slot=2` returns an error
testEpochSlotSearch (EpochNumber 1) 2 =
    pure $ Left $ Internal "Error while searching epoch/slot"
-- all others returns a simple result
testEpochSlotSearch _ _ = pure $ Right [CBlockEntry
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
    :: EpochNumber
    -> Maybe Int
    -> Handler (Either ExplorerError (Int, [CBlockEntry]))
testEpochPageSearch _ _ = pure $ Right (1, [CBlockEntry
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
    :: Handler (Either ExplorerError CGenesisSummary)
testGenesisSummary = pure $ Right CGenesisSummary
    { cgsNumTotal       = 4
    , cgsNumRedeemed    = 3
    , cgsNumNotRedeemed = 1
    , cgsRedeemedAmountTotal    = mkCCoin 300000000
    , cgsNonRedeemedAmountTotal = mkCCoin 100000000
    }

testGenesisPagesTotal
    :: Maybe Word
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError Integer)
-- number of redeemed addresses pages
testGenesisPagesTotal _ (Just RedeemedAddresses)    = pure $ Right 1
-- number of non redeemed addresses pages
testGenesisPagesTotal _ (Just NonRedeemedAddresses) = pure $ Right 1
-- number of all redeem addresses pages
testGenesisPagesTotal _ _                           = pure $ Right 2

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
    :: Maybe Word
    -> Maybe Word
    -> Maybe CAddressesFilter
    -> Handler (Either ExplorerError [CGenesisAddressInfo])
-- filter redeemed addresses
testGenesisAddressInfo _ _ (Just RedeemedAddresses)    = pure $ Right [ gAddressInfoA ]
-- filter non-redeemed addresses
testGenesisAddressInfo _ _ (Just NonRedeemedAddresses) = pure $ Right [ gAddressInfoB, gAddressInfoC ]
-- all addresses (w/o filtering) - page 1
testGenesisAddressInfo (Just 1) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoA, gAddressInfoB ]
testGenesisAddressInfo (Just 1) _ Nothing              = pure $ Right [ gAddressInfoA, gAddressInfoB ]
-- all addresses (w/o filtering) - page 2
testGenesisAddressInfo (Just 2) _ (Just AllAddresses)  = pure $ Right [ gAddressInfoC ]
testGenesisAddressInfo (Just 2) _ Nothing              = pure $ Right [ gAddressInfoC ]
-- all others requests will ended up with an error
testGenesisAddressInfo _ _ _ =  pure $ Left $ Internal "Error while pagening genesis addresses"

testStatsTxs
    :: Maybe Word
    -> Handler (Either ExplorerError TxsStats)
testStatsTxs _ = pure $ Right (1, [(cTxId, 200)])
