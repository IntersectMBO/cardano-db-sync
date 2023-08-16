{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Cardano.DbTool.Validate.Balance (
  ledgerAddrBalance,
) where

import qualified Cardano.Api.Shelley as Api
import qualified Cardano.Chain.Block as Byron
import Cardano.Chain.Common (
  CompactAddress,
  Lovelace,
  decodeAddressBase58,
  sumLovelace,
  toCompactAddress,
  unsafeGetLovelace,
 )
import qualified Cardano.Chain.UTxO as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Ledger
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Shelley.API (Coin (..))
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import Cardano.Ledger.Val
import Cardano.Prelude
import qualified Data.Map.Strict as Map
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Data.String (String)
import qualified GHC.Show as S
import Ouroboros.Consensus.Byron.Ledger
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, LedgerState (..), StandardCrypto)
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Ledger

data ValidateBalanceError
  = VBErrByron String
  | VBErrShelley String
  | VBErrAllegra String
  | VBErrMary String
  | VBErrAlonzo String
  | VBErrBabbage String
  | VBErrConway String

instance Exception ValidateBalanceError

instance Show ValidateBalanceError where
  show =
    \case
      VBErrByron err -> vBErr <> "Byron: " <> err
      VBErrShelley err -> vBErr <> "Shelley: " <> err
      VBErrAllegra err -> vBErr <> "Allegra: " <> err
      VBErrMary err -> vBErr <> "Mary: " <> err
      VBErrAlonzo err -> vBErr <> "Alonzo: " <> err
      VBErrBabbage err -> vBErr <> "Babbage: " <> err
      VBErrConway err -> vBErr <> "Conway: " <> err

vBErr :: String
vBErr = "Validation Balance Error - "

-- Given an address, return it's current UTxO balance.
ledgerAddrBalance :: Text -> LedgerState (CardanoBlock StandardCrypto) -> Either ValidateBalanceError Word64
ledgerAddrBalance addr lsc =
  case lsc of
    LedgerStateByron st -> getByronBalance addr $ Byron.cvsUtxo $ byronLedgerState st
    LedgerStateShelley st -> getShelleyBalance addr $ getUTxO st
    LedgerStateAllegra st -> getShelleyBalance addr $ getUTxO st
    LedgerStateMary st -> getShelleyBalance addr $ getUTxO st
    LedgerStateAlonzo st -> getAlonzoBalance addr $ getUTxO st
    LedgerStateBabbage _st -> Left $ VBErrBabbage "undefined Babbage ledgerAddrBalance"
    LedgerStateConway _st -> Left $ VBErrConway "undefined Conway ledgerAddrBalance"
  where
    getUTxO :: LedgerState (ShelleyBlock p era) -> Shelley.UTxO era
    getUTxO = Shelley.utxosUtxo . Shelley.lsUTxOState . Shelley.esLState . Shelley.nesEs . shelleyLedgerState

getByronBalance :: Text -> Byron.UTxO -> Either ValidateBalanceError Word64
getByronBalance addrText utxo = do
  case toCompactAddress <$> decodeAddressBase58 addrText of
    Left err -> Left $ VBErrByron $ show err
    Right caddr -> do
      let utxos = Map.elems $ Byron.unUTxO utxo
          lvlaces = mapMaybe (compactTxOutValue caddr) utxos
      case sumLovelace lvlaces of
        Left err -> Left $ VBErrByron $ show err
        Right sLvlace -> Right $ unsafeGetLovelace sLvlace
  where
    compactTxOutValue :: CompactAddress -> Byron.CompactTxOut -> Maybe Lovelace
    compactTxOutValue caddr (Byron.CompactTxOut bcaddr lovelace) =
      if caddr == bcaddr
        then Just lovelace
        else Nothing

getShelleyBalance ::
  forall era.
  (EraCrypto era ~ StandardCrypto, Ledger.TxOut era ~ Shelley.ShelleyTxOut era) =>
  Val (Ledger.Value era) =>
  Text ->
  Shelley.UTxO era ->
  Either ValidateBalanceError Word64
getShelleyBalance addrText utxo = do
  case covertToCompactAddress addrText of
    Left err -> Left $ VBErrShelley err
    Right cmpAddr -> Right . fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue cmpAddr) (Map.elems $ Shelley.unUTxO utxo)
  where
    compactTxOutValue :: CompactAddr (EraCrypto era) -> Ledger.TxOut era -> Maybe Coin
    compactTxOutValue caddr (Shelley.TxOutCompact scaddr v) =
      if caddr == scaddr
        then Just $ coin (fromCompact v)
        else Nothing

getAlonzoBalance :: Text -> Shelley.UTxO (AlonzoEra StandardCrypto) -> Either ValidateBalanceError Word64
getAlonzoBalance addrText utxo = do
  case covertToCompactAddress addrText of
    Left err -> Left $ VBErrAlonzo err
    Right cmpAddr -> Right . fromIntegral . sum $ unCoin <$> mapMaybe (compactTxOutValue cmpAddr) (Map.elems $ Shelley.unUTxO utxo)
  where
    compactTxOutValue ::
      CompactAddr StandardCrypto -> Alonzo.AlonzoTxOut (AlonzoEra StandardCrypto) -> Maybe Coin
    compactTxOutValue caddr txOut =
      let (scaddr, val) = case txOut of
            Alonzo.TxOutCompact a v -> (a, v)
            Alonzo.TxOutCompactDH a v _ -> (a, v)
       in if caddr == scaddr
            then Just $ coin (fromCompact val)
            else Nothing

covertToCompactAddress :: Text -> Either String (CompactAddr StandardCrypto)
covertToCompactAddress addrText =
  case Api.deserialiseAddress (Api.AsAddress Api.AsShelleyAddr) addrText of
    Nothing ->
      case decodeAddressBase58 addrText of
        Left err -> Left $ show err
        Right badrr -> Right $ compactAddr (AddrBootstrap $ BootstrapAddress badrr)
    Just (Api.ShelleyAddress n p s) ->
      Right $ compactAddr (Addr n p s)
