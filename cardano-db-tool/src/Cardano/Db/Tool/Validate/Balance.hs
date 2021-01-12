{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Tool.Validate.Balance
  (ledgerAddrBalance
  ) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Chain.Block as Byron
import           Cardano.Chain.Common (unsafeGetLovelace, sumLovelace, toCompactAddress, decodeAddressBase58)
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Ledger.Compactible
import           Cardano.Ledger.Shelley.Constraints
import           Cardano.Ledger.Val
import           Cardano.Prelude

import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock,
                   LedgerState (..), StandardCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

import           Shelley.Spec.Ledger.CompactAddr (compactAddr)
import           Shelley.Spec.Ledger.LedgerState (esLState, nesEs, _utxo, _utxoState)
import qualified Shelley.Spec.Ledger.TxBody as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Shelley.Spec.Ledger.API (Addr(..), Coin(unCoin))
import Shelley.Spec.Ledger.Address (BootstrapAddress(BootstrapAddress))

-- Given an address, return it's current UTxO balance.
ledgerAddrBalance :: Text -> LedgerState (CardanoBlock StandardCrypto) -> Either Text Word64
ledgerAddrBalance addr lsc =
    case lsc of
      LedgerStateByron st -> getByronBalance addr $ Byron.cvsUtxo $ byronLedgerState st
      LedgerStateShelley st -> getShelleyBalance addr $ getUTxO st
      LedgerStateAllegra st -> getShelleyBalance addr $ getUTxO st
      LedgerStateMary st -> getShelleyBalance addr $ getUTxO st
  where
    getUTxO st = _utxo $ _utxoState $ esLState $ nesEs $ shelleyLedgerState st

getByronBalance :: Text -> Byron.UTxO -> Either Text Word64
getByronBalance addrText utxo = do
    case toCompactAddress <$> decodeAddressBase58 addrText of
      Left err -> Left $ textShow err
      Right caddr -> bimap show unsafeGetLovelace $ sumLovelace $ mapMaybe (compactTxOutValue caddr) $ Map.elems $ Byron.unUTxO utxo
  where
    compactTxOutValue caddr (Byron.CompactTxOut caddr' lovelace) = if caddr == caddr'
        then Just lovelace
        else Nothing

getShelleyBalance :: forall era. (ShelleyBased era)
                  => Text -> Shelley.UTxO era -> Either Text Word64
getShelleyBalance addrText utxo = do
    caddr <- case Api.deserialiseAddress (Api.AsAddress Api.AsShelleyAddr) addrText of
          Nothing -> case decodeAddressBase58 addrText of
            Left err ->  Left $ textShow err
            Right badrr -> Right $ compactAddr (AddrBootstrap $ BootstrapAddress badrr)
          Just (Api.ShelleyAddress n p s) ->
            let addr = Addr n (coerce p) (coerce s)
            in Right $ compactAddr addr
    Right (fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue caddr) (Map.elems $ Shelley.unUTxO utxo))
  where
    compactTxOutValue caddr (Shelley.TxOutCompact caddr' v) = if caddr == caddr'
      then Just $ coin $ fromCompact v
      else Nothing

textShow :: Show a => a -> Text
textShow = Text.pack . show