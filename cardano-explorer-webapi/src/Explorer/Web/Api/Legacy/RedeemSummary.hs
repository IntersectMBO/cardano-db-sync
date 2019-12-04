{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.Legacy.RedeemSummary
  ( queryRedeemSummary
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Database.Esqueleto (InnerJoin (..), Value (..),
                    (^.), (==.), (&&.), from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), unValue3)

import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, genesisDistributionTxHash)
import           Explorer.Web.ClientTypes (CAddress (..), CAddressSummary (..), CAddressType (..),
                    CCoin, CChainTip (..), CHash (..), CTxAddressBrief (..), CTxBrief (..),
                    CTxHash (..), mkCCoin)
import           Explorer.Web.Error (ExplorerError (..))

-- Example redeem addresses:
--    /api/addresses/summary/Ae2tdPwUPEZAAvxJ9nQ1nx88X9Jq6dskyG9uFUWG69wC6TJ7DKNCp6QCEM2 (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZKvgpCjEe7XvYMFtxYEdvEJRkY1UBkhYFVfW7sxeZtRMeax4A (unspent)
--    /api/addresses/summary/Ae2tdPwUPEZ3N2AuMYpqerifycLiLPtsV8B72VgAsPjPAwrLa7xyfWBUJ2t (spent)
--    /api/addresses/summary/Ae2tdPwUPEZHhgPpDi2g5nE1UhHn6hCqihNxwkhTgxQayQ6FwE3eKEypDZU (spent)


-- | Redeem addresses are sufficiently different to warrant their own query.
queryRedeemSummary :: MonadIO m => CChainTip -> Text -> ReaderT SqlBackend m (Either ExplorerError CAddressSummary)
queryRedeemSummary chainTip addrTxt = do
    -- Find the initial value assigned to this address at Genesis
    rows <- select . from $ \ txOut -> do
              where_ (txOut ^. TxOutAddress ==. val addrTxt)
              pure (txOut ^. TxOutValue)
    case rows of
      [] -> pure $ Left (Internal "queryRedeemSummary: Address not found")
      [value] -> Right <$> queryRedeemed (mkCCoin . fromIntegral $ unValue value)
      _ -> pure $ Left (Internal "queryRedeemSummary: More than one entry")
  where
    queryRedeemed :: MonadIO m => CCoin -> ReaderT SqlBackend m CAddressSummary
    queryRedeemed value = do
      -- Query to see if the Genesis value has been spent.
      -- Will return [] if unspent and otherwise a single row.
      outrows <- select . from $ \ (blk `InnerJoin` tx `InnerJoin` txIn `InnerJoin` txOut0 `InnerJoin` txOut1) -> do
                    on (tx ^. TxId ==. txOut1 ^. TxOutTxId)
                    on (txIn ^. TxInTxOutId ==. txOut0 ^. TxOutTxId
                        &&. txIn ^. TxInTxOutIndex ==. txOut0 ^. TxOutIndex)
                    on (tx ^. TxId ==. txIn ^. TxInTxInId)
                    on (blk ^. BlockId ==. tx ^. TxBlock)
                    where_ (txOut0 ^. TxOutAddress ==. val addrTxt)
                    pure (tx ^. TxHash, blk ^. BlockTime, txOut1 ^. TxOutAddress)
      pure $ maybe (convertUnspent value) (convertSpent value) (unValue3 <$> listToMaybe outrows)

    convertUnspent :: CCoin -> CAddressSummary
    convertUnspent balance =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caChainTip = chainTip
        , caTxNum = 0
        , caBalance = balance
        , caTotalInput = mkCCoin 0
        , caTotalOutput = mkCCoin 0
        , caTotalFee = mkCCoin 0
        , caTxList = []
        }

    convertSpent :: CCoin -> (ByteString, UTCTime, Text) -> CAddressSummary
    convertSpent outval (txhash, utctime, outAddr) =
      CAddressSummary
        { caAddress = CAddress addrTxt
        , caType = CRedeemAddress
        , caChainTip = chainTip
        , caTxNum = 1
        , caBalance = mkCCoin 0
        , caTotalInput = outval
        , caTotalOutput = outval
        , caTotalFee = mkCCoin 0
        , caTxList =
            [ CTxBrief
                { ctbId = CTxHash . CHash $ bsBase16Encode txhash
                , ctbTimeIssued = Just $ utcTimeToPOSIXSeconds utctime
                , ctbInputs =
                    [ CTxAddressBrief
                        { ctaAddress = CAddress addrTxt
                        , ctaAmount = outval
                        , ctaTxHash = genesisDistributionTxHash
                        , ctaTxIndex = 0
                        }
                    ]
                , ctbOutputs =
                    [ CTxAddressBrief
                        { ctaAddress = CAddress outAddr
                        , ctaAmount = outval
                        , ctaTxHash = CTxHash $ CHash (bsBase16Encode txhash)
                        , ctaTxIndex = 0
                        }
                    ]
                , ctbInputSum = outval
                , ctbOutputSum = outval
                , ctbFees = mkCCoin 0
                }
            ]
        }
