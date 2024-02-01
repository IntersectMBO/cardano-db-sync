{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Db.Mock.Unit.Conway.Plutus (
  -- * Plutus send scripts
  simpleScript,
  unlockScriptSameBlock,
  unlockScriptNoPlutus,
  failedScript,
  failedScriptFees,
  failedScriptSameBlock,
  multipleScripts,
  multipleScriptsRollback,
  multipleScriptsSameBlock,
  multipleScriptsFailed,
  multipleScriptsFailedSameBlock,

  -- * Plutus cert scripts
  registrationScriptTx,
  deregistrationScriptTx,
  deregistrationsScriptTxs,
  deregistrationsScriptTx,
  deregistrationsScriptTx',
  deregistrationsScriptTx'',

  -- * Plutus MultiAsset scripts
  mintMultiAsset,
  mintMultiAssets,
  swapMultiAssets,
  swapMultiAssetsDisabled,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import qualified Cardano.Db as DB
import Cardano.DbSync.Era.Shelley.Generic.Util (renderAddress)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Value (MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus.Data
import Cardano.Ledger.SafeHash (extractHash)
import Cardano.Mock.ChainSync.Server (IOManager ())
import Cardano.Mock.Forging.Interpreter (withConwayLedgerState)
import qualified Cardano.Mock.Forging.Tx.Alonzo.ScriptsExamples as Examples
import qualified Cardano.Mock.Forging.Tx.Conway as Conway
import Cardano.Mock.Forging.Types
import Cardano.Mock.Query (queryMultiAssetCount)
import Cardano.Prelude hiding (head)
import qualified Data.Map as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Ouroboros.Consensus.Shelley.Eras (StandardConway ())
import Ouroboros.Network.Block (genesisPoint)
import Test.Cardano.Db.Mock.Config
import qualified Test.Cardano.Db.Mock.UnifiedApi as Api
import Test.Cardano.Db.Mock.Validate
import Test.Tasty.HUnit (Assertion ())
import Prelude (head, tail, (!!))

simpleScript :: IOManager -> [(Text, Text)] -> Assertion
simpleScript =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Fill the rest of the epoch
    epoch <- Api.fillUntilNextEpoch interpreter mockServer

    -- Forge a block with a script
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline True] 20_000 20_000

    -- Verify the outputs match expected
    assertBlockNoBackoff dbSync (length epoch + 2)
    assertEqQuery
      dbSync
      (map getOutFields <$> DB.queryScriptOutputs)
      [expectedFields]
      "Unexpected script outputs"
  where
    testLabel = "conwaySimpleScript"
    getOutFields txOut =
      ( DB.txOutAddress txOut
      , DB.txOutAddressHasScript txOut
      , DB.txOutValue txOut
      , DB.txOutDataHash txOut
      )
    expectedFields =
      ( renderAddress Examples.alwaysSucceedsScriptAddr
      , True
      , DB.DbLovelace 20_000
      , Just $
          hashToBytes (extractHash $ hashData @StandardConway Examples.plutusDataList)
      )

unlockScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge lock/unlock scripts in the same block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutNoInline True]
          20_000
          20_000
          state'
      let utxo0 = head (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTx
          [UTxOPair utxo0]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500
          state'
      pure [tx0, tx1]

    -- Verify script counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 1, 1, 1, 1, 1, 0, 0)
  where
    testLabel = "conwayUnlockScriptSameBlock"

unlockScriptNoPlutus :: IOManager -> [(Text, Text)] -> Assertion
unlockScriptNoPlutus =
  withCustomConfig args Nothing conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake credentials
    void $ Api.registerAllStakeCreds interpreter mockServer

    -- Lock some funds
    lockTx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline True] 20_000 20_000
    -- Unlock the funds above with a script
    let utxos = map UTxOPair (Conway.mkUTxOConway lockTx)
    unlockTx <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx utxos (UTxOIndex 1) (UTxOIndex 2) True 10_000 500

    -- Submit them
    void $
      Api.forgeNextFindLeaderAndSubmit
        interpreter
        mockServer
        (map TxConway [lockTx, unlockTx])

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    -- Should not have any scripts
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 0, 0)
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-no-plutus.json"
        , claFullMode = False
        }
    testLabel = "conwayConfigPlutusDisbaled"

failedScript :: IOManager -> [(Text, Text)] -> Assertion
failedScript =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Forge another block with a failing unlock script
    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500

    -- Verify the invalid tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "conwayFailedScript"

failedScriptFees :: IOManager -> [(Text, Text)] -> Assertion
failedScriptFees =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a lock script
    tx <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx (UTxOIndex 0) [Conway.TxOutNoInline False] 20_000 20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx] (NodeId 1)

    -- Forge another block with a failing unlock script
    let utxo = head (Conway.mkUTxOConway tx)
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500

    -- Verify fees
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
    assertNonZeroFeesContract dbSync
  where
    testLabel = "conwayFailedScriptFees"

failedScriptSameBlock :: IOManager -> [(Text, Text)] -> Assertion
failedScriptSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with stake registrations
    void $ Api.registerAllStakeCreds interpreter mockServer
    -- Forge a single block with lock/unlock scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      -- Create a lock script
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          [Conway.TxOutNoInline False]
          20_000
          20_000
          state'
      -- Extract the utxo
      let utxo = head (Conway.mkUTxOConway tx0)
      -- Create a failing unlock script
      tx1 <-
        Conway.mkUnlockScriptTx
          [UTxOPair utxo]
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Verify invalid tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 1, 0, 1, 1)
  where
    testLabel = "conwayFailedScriptSameBlock"

multipleScripts :: IOManager -> [(Text, Text)] -> Assertion
multipleScripts =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge multiple script transactions
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    let utxo = Conway.mkUTxOConway tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          [UTxOPair pair1, UTxOPair pair2]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500

    -- Submit the txs in separate blocks
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify tx counts
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScripts"

multipleScriptsRollback :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsRollback =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Create multiple scripts
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    let utxo = Conway.mkUTxOConway tx0
        pair1 = head utxo
        pair2 = utxo !! 2
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          [UTxOPair pair1, UTxOPair pair2]
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500

    -- Submit the txs in separate blocks
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)

    -- Roll back to genesis
    Api.rollbackTo interpreter mockServer genesisPoint
    -- Forge another block
    void $ Api.forgeNextFindLeaderAndSubmit interpreter mockServer []

    -- Submit the txs again
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify tx counts
    assertBlockNoBackoff dbSync 3
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScriptsRollback"

multipleScriptsSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a single block with multiple scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
          state'
      let utxo = Conway.mkUTxOConway tx0
          pairs = [head utxo, utxo !! 2] -- Omit the failing (middle) script
      tx1 <-
        Conway.mkUnlockScriptTx
          (map UTxOPair pairs)
          (UTxOIndex 1)
          (UTxOIndex 2)
          True
          10_000
          500
          state'
      pure [tx0, tx1]

    -- Verify tx counts
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 2, 1, 1, 3, 2, 0, 0)
  where
    testLabel = "conwayMultipleScriptsSameBlock"

multipleScriptsFailed :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailed =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with multiple scripts
    tx0 <-
      withConwayLedgerState interpreter $
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx0] (NodeId 1)

    -- Forge another block with failing scripts
    let utxos = Conway.mkUTxOConway tx0
    tx1 <-
      withConwayLedgerState interpreter $
        Conway.mkUnlockScriptTx
          (map UTxOPair utxos)
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
    void $
      Api.forgeNextAndSubmit interpreter mockServer $
        MockBlock [TxConway tx1] (NodeId 1)

    -- Verify failed txs
    assertBlockNoBackoff dbSync 2
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "conwayMultipleScriptsFailed"

multipleScriptsFailedSameBlock :: IOManager -> [(Text, Text)] -> Assertion
multipleScriptsFailedSameBlock =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Add multiple scripts in the same block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      tx0 <-
        Conway.mkLockByScriptTx
          (UTxOIndex 0)
          (map Conway.TxOutNoInline [True, False, True])
          20_000
          20_000
          state'
      let utxos = tail (Conway.mkUTxOConway tx0)
      tx1 <-
        Conway.mkUnlockScriptTx
          (map UTxOPair utxos)
          (UTxOIndex 1)
          (UTxOIndex 2)
          False -- Force failure
          10_000
          500
          state'

      pure [tx0, tx1]

    -- Verify failed txs
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (0, 0, 0, 0, 3, 0, 1, 1)
  where
    testLabel = "conwayMultipleScriptsFailedSameBlock"

registrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
registrationScriptTx =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a transaction with a registration cert
    void $
      Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $
        Conway.mkSimpleDCertTx [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]

    -- Verify stake address script counts
    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (0, 0, 0, 1)
  where
    testLabel = "conwayRegistrationScriptTx"

deregistrationScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationScriptTx =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge registration/deregistration cert transaction
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      sequence
        [ Conway.mkSimpleDCertTx
            [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]
            state'
        , Conway.mkScriptDCertTx
            [(StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)]
            True
            state'
        ]

    -- Verify dereg and stake address script counts
    assertBlockNoBackoff dbSync 1
    assertScriptCert dbSync (1, 0, 0, 1)
  where
    testLabel = "conwayDeregistrationScriptTx"

deregistrationsScriptTxs :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTxs =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge multiple reg/dereg transactions in a single block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      reg <-
        Conway.mkSimpleDCertTx
          [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]
          state'
      dereg <-
        Conway.mkScriptDCertTx
          [(StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)]
          True
          state'
      pure
        [ reg
        , dereg
        , Conway.addValidityInterval 1000 reg
        , Conway.addValidityInterval 2000 dereg
        ]

    assertBlockNoBackoff dbSync 1
    -- Verify dereg and stake address script counts
    assertScriptCert dbSync (2, 0, 0, 1)
    -- Verify script/redeemer/datum counts
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "conwayDeregistrationsScriptTxs"

deregistrationsScriptTx :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge multiple reg/dereg transactions in a single block
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      sequence
        [ -- A transaction with one registration
          Conway.mkSimpleDCertTx
            [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]
            state'
        , -- A transaction with multiple deregistrations/registrations
          Conway.mkScriptDCertTx
            [ (StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)
            , -- No redeemer
              (StakeIndexScript True, False, Conway.mkRegTxCert SNothing)
            , -- Add redeemer
              (StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)
            ]
            True
            state'
        ]

    assertBlockNoBackoff dbSync 1
    -- Verify dereg and stake address script counts
    assertScriptCert dbSync (2, 0, 0, 1)
    -- Verify script/redeemer/datum counts
    assertAlonzoCounts dbSync (1, 2, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "conwayDeregistrationsScriptTx"

-- Like previous but missing a redeemer. This is a known ledger issue
deregistrationsScriptTx' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx' =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge registrations/deregistrations without a redeemer
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ -- A transaction with one registration
          Conway.mkSimpleDCertTx
            [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]
            state'
        , -- Multiple degistrations/registrations, missing redeemer
          Conway.mkScriptDCertTx
            [ -- No redeemer
              (StakeIndexScript True, False, Conway.mkUnRegTxCert SNothing)
            , (StakeIndexScript True, False, Conway.mkRegTxCert SNothing)
            , -- Add redeemer
              (StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)
            ]
            True
            state'
        ]

    assertBlockNoBackoff dbSync 1
    -- TODO: This is a bug! The first field (delegations) should be 2. However the
    -- deregistrations are missing the redeemers
    assertScriptCert dbSync (0, 0, 0, 1)
    -- Redeemer count should now be 1
    assertAlonzoCounts dbSync (1, 1, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "conwayDeregistrationsScriptTx'"

-- Like previous but missing the other redeemer. This is a known ledger issue
deregistrationsScriptTx'' :: IOManager -> [(Text, Text)] -> Assertion
deregistrationsScriptTx'' =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge registrations/deregistrations without a redeemer
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' ->
      sequence
        [ -- A transaction with one registration
          Conway.mkSimpleDCertTx
            [(StakeIndexScript True, Conway.mkRegTxCert SNothing)]
            state'
        , -- Multiple deregistrations/registrations, missing redeemer
          Conway.mkScriptDCertTx
            [ -- Add redeemer
              (StakeIndexScript True, True, Conway.mkUnRegTxCert SNothing)
            , (StakeIndexScript True, False, Conway.mkRegTxCert SNothing)
            , -- No redeemer
              (StakeIndexScript True, False, Conway.mkUnRegTxCert SNothing)
            ]
            True
            state'
        ]

    assertBlockNoBackoff dbSync 1
    -- TODO: Is this a bug? The first field (delegations) should be 2, but it's only 1
    assertScriptCert dbSync (1, 0, 0, 1)
    -- Redeemer count should now be 1
    assertAlonzoCounts dbSync (1, 1, 1, 0, 0, 0, 0, 0)
  where
    testLabel = "conwayDeregistrationsScriptTx''"

mintMultiAsset :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAsset =
  withFullConfigAndDropDB conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with a multi-asset script
    void $ Api.withConwayFindLeaderAndSubmitTx interpreter mockServer $ \state' -> do
      let val =
            MultiAsset $
              Map.singleton
                (PolicyID Examples.alwaysMintScriptHash)
                (Map.singleton (head Examples.assetNames) 1)
      Conway.mkMultiAssetsScriptTx
        [UTxOIndex 0]
        (UTxOIndex 1)
        [(UTxOAddressNew 0, MaryValue (Coin 10_000) mempty)]
        []
        val
        True
        100
        state'

    -- Verify script counts
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (1, 1, 1, 1, 0, 0, 0, 0)
  where
    testLabel = "conwayMintMultiAsset"

mintMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
mintMultiAssets =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with multiple multi-asset scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      let assets = Map.fromList [(head Examples.assetNames, 10), (Examples.assetNames !! 1, 4)]
          policy0 = PolicyID Examples.alwaysMintScriptHash
          policy1 = PolicyID Examples.alwaysSucceedsScriptHash
          val = MultiAsset $ Map.fromList [(policy0, assets), (policy1, assets)]

      sequence
        [ Conway.mkMultiAssetsScriptTx
            [UTxOIndex 0]
            (UTxOIndex 1)
            [(UTxOAddressNew 0, MaryValue (Coin 10_000) mempty)]
            []
            val
            True
            100
            state'
        , Conway.mkMultiAssetsScriptTx
            [UTxOIndex 2]
            (UTxOIndex 3)
            [(UTxOAddressNew 0, MaryValue (Coin 10_000) mempty)]
            []
            val
            True
            200
            state'
        ]

    -- Verify script counts
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 4, 1, 2, 0, 0, 0, 0)
  where
    testLabel = "conwayMintMultiAssets"

swapMultiAssets :: IOManager -> [(Text, Text)] -> Assertion
swapMultiAssets =
  withFullConfig conwayConfigDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with multiple multi-asset scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      let assetsMinted =
            Map.fromList [(head Examples.assetNames, 10), (Examples.assetNames !! 1, 4)]
          policy0 = PolicyID Examples.alwaysMintScriptHash
          policy1 = PolicyID Examples.alwaysSucceedsScriptHash
          mintValue =
            MultiAsset $
              Map.fromList [(policy0, assetsMinted), (policy1, assetsMinted)]
          assets =
            Map.fromList [(head Examples.assetNames, 5), (Examples.assetNames !! 1, 2)]
          outValue =
            MaryValue (Coin 20) $
              MultiAsset $
                Map.fromList [(policy0, assets), (policy1, assets)]

      -- Forge a multi-asset script
      tx0 <-
        Conway.mkMultiAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [ (UTxOAddress Examples.alwaysSucceedsScriptAddr, outValue)
          , (UTxOAddress Examples.alwaysMintScriptAddr, outValue)
          ]
          []
          mintValue
          True
          100
          state'

      -- Consume the outputs from tx0
      let utxos = Conway.mkUTxOConway tx0
      tx1 <-
        Conway.mkMultiAssetsScriptTx
          [UTxOPair (head utxos), UTxOPair (utxos !! 1), UTxOIndex 2]
          (UTxOIndex 3)
          [ (UTxOAddress Examples.alwaysSucceedsScriptAddr, outValue)
          , (UTxOAddress Examples.alwaysMintScriptAddr, outValue)
          , (UTxOAddressNew 0, outValue)
          , (UTxOAddressNew 0, outValue)
          ]
          []
          mintValue
          True
          200
          state'

      pure [tx0, tx1]

    -- Verify script counts
    assertBlockNoBackoff dbSync 1
    assertAlonzoCounts dbSync (2, 6, 1, 2, 4, 2, 0, 0)
    assertEqBackoff dbSync queryMultiAssetCount 4 [] "Expected multi-assets"
  where
    testLabel = "conwaySwapMultiAssets"

swapMultiAssetsDisabled :: IOManager -> [(Text, Text)] -> Assertion
swapMultiAssetsDisabled =
  withCustomConfig args Nothing cfgDir testLabel $ \interpreter mockServer dbSync -> do
    startDBSync dbSync

    -- Forge a block with multiple multi-asset scripts
    void $ Api.withConwayFindLeaderAndSubmit interpreter mockServer $ \state' -> do
      let policy = PolicyID Examples.alwaysMintScriptHash
          assets = Map.singleton (Prelude.head Examples.assetNames) 1
          mintedValue = MultiAsset $ Map.singleton policy assets
          outValue = MaryValue (Coin 20) (MultiAsset $ Map.singleton policy assets)

      -- Forge a multi-asset script
      tx0 <-
        Conway.mkMultiAssetsScriptTx
          [UTxOIndex 0]
          (UTxOIndex 1)
          [(UTxOAddress Examples.alwaysSucceedsScriptAddr, outValue)]
          []
          mintedValue
          True
          100
          state'

      pure [tx0]

    -- Wait for it to sync
    assertBlockNoBackoff dbSync 1
    -- Verify multi-assets
    assertEqBackoff dbSync queryMultiAssetCount 0 [] "Unexpected multi-assets"
  where
    args =
      initCommandLineArgs
        { claConfigFilename = "test-db-sync-config-no-multi-assets.json"
        , claFullMode = False
        }

    testLabel = "conwayConfigMultiAssetsDisabled"
    cfgDir = conwayConfigDir
