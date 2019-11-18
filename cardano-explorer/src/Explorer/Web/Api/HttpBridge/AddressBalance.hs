{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Explorer.Web.Api.HttpBridge.AddressBalance
  ( addressBalance
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.ByteString.Char8 (ByteString)
import           Data.Text (Text)
import           Data.Word (Word16, Word64)

import           Database.Esqueleto (InnerJoin (..), Value (..),
                    (^.), (==.), from, on, select, val, where_)
import           Database.Persist.Sql (SqlBackend)

import           Explorer.DB (EntityField (..), txOutUnspentP, queryNetworkName)
import           Explorer.Web.ClientTypes (CAddress (..), CNetwork (..), CAddressBalance (..),
                    CAddressBalanceError (..))
import           Explorer.Web.Api.Legacy.Util (bsBase16Encode, decodeTextAddress, runQuery)

import           Servant (Handler)

-- This endpoint emulates the Rust cardano-http-bridge endpoint:
--
--  GET: /:network/utxos/:address
--
-- and returns the current Utxo output details:
--
--  [ { "address": "2cWKMJemoBamE3kYCuVLq6pwWwNBJVZmv471Zcb2ok8cH9NjJC4JUkq5rV5ss9ALXWCKN"
--    , "coin": 310025
--    , "index": 0
--    , "txid": "89eb0d6a8a691dae2cd15ed0369931ce0a949ecafa5c3f93f8121833646e15c3"
--    }
--  ]


-- This endpoint always returns a list (which may be empty).
-- There are a number of potential failures and wyat
addressBalance
    :: SqlBackend -> CNetwork -> CAddress
    -> Handler CAddressBalanceError
addressBalance backend (CNetwork networkName) (CAddress addrTxt) =
  -- Currently ignore the 'CNetwork' parameter (eg mainnet, testnet etc) as the explorer only
  -- supports a single network and returns a result for whichever network its running on.
  case decodeTextAddress addrTxt of
    Left _ -> pure $ CABError "Invalid address"
    Right _ -> runQuery backend $ do
                mNetName <- queryNetworkName
                case mNetName of
                  Nothing -> pure $ CABError "Invalid network name"
                  Just name -> if name /= networkName
                                then pure $ CABError "Network name mismatch"
                                else CABValue <$> queryAddressBalance addrTxt

-- -------------------------------------------------------------------------------------------------

queryAddressBalance :: MonadIO m => Text -> ReaderT SqlBackend m [CAddressBalance]
queryAddressBalance addrTxt = do
    rows <- select . from $ \ (tx `InnerJoin` txOut) -> do
              on (tx ^. TxId ==. txOut ^. TxOutTxId)
              txOutUnspentP txOut
              where_ (txOut ^. TxOutAddress ==. val addrTxt)
              pure (txOut ^. TxOutAddress, tx ^. TxHash, txOut ^. TxOutIndex, txOut ^. TxOutValue)
    pure $ map convert rows
  where
    convert :: (Value Text, Value ByteString, Value Word16, Value Word64) -> CAddressBalance
    convert (Value addr, Value txhash, Value index, Value coin) =
      CAddressBalance
        { cuaAddress = addr
        , cuaTxHash = bsBase16Encode txhash
        , cuaIndex = index
        , cuaCoin = coin
        }
