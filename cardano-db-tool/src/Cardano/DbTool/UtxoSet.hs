{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.UtxoSet (
  utxoSetAtSlot,
  utxoSetSum,
) where

import Cardano.Chain.Common (decodeAddressBase58, isRedeemAddress)
import qualified Cardano.Db as DB
import qualified Cardano.Db.Schema.Variants.TxOutAddress as VA
import qualified Cardano.Db.Schema.Variants.TxOutCore as VC
import Cardano.Prelude (textShow)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import System.Exit (exitSuccess)
import System.IO (IOMode (..), withFile)

utxoSetAtSlot :: DB.TxOutVariantType -> Word64 -> IO ()
utxoSetAtSlot txOutVariantType slotNo = do
  (genesisSupply, utxoSet, fees, eUtcTime) <- queryAtSlot txOutVariantType slotNo

  let supply = utxoSetSum utxoSet
  let aggregated = aggregateUtxos utxoSet
  let (accept, reject) = partitionUtxos aggregated

  if null aggregated
    then reportSlotDate slotNo eUtcTime
    else do
      putStr $
        unlines
          [ "\nGenesis supply: " ++ show genesisSupply ++ " Ada"
          , "\nAt slot number " ++ show slotNo ++ ":"
          , "   Date: " ++ either (const "unknown") show eUtcTime
          , "   Supply: " ++ show supply ++ " Ada"
          , "   Fees: " ++ show fees ++ " Ada"
          , "   Supply + fees == genesis supply: " ++ show (fees + supply == genesisSupply)
          , "\nFrom database:"
          , "  Utxo entries: " ++ show (length utxoSet)
          , "  Utxo supply : " ++ show (utxoSetSum utxoSet) ++ " Ada"
          , "\nAfter aggregation:"
          , "  Utxo entries: " ++ show (length aggregated)
          , "  Utxo supply : " ++ show (sum $ map snd aggregated) ++ " Lovelace"
          , "\nAfter paritioning:"
          , "  Accepted Utxo entries: " ++ show (length accept)
          , "  Rejected Utxo entries: " ++ show (length reject)
          , "  Accepted Utxo supply: " ++ show (sum $ map snd accept) ++ " Lovelace"
          , "  Rejected Utxo supply: " ++ show (sum $ map snd reject) ++ " Lovelace"
          , "  Accepted + rejected == totalSupply: "
              ++ show (sum (map snd accept) + sum (map snd reject) == sum (map snd aggregated))
          , ""
          ]

      writeUtxos ("utxo-accept-" ++ show slotNo ++ ".json") accept
      writeUtxos ("utxo-reject-" ++ show slotNo ++ ".json") reject
      putStrLn ""

aggregateUtxos :: [DB.UtxoQueryResult] -> [(Text, Word64)]
aggregateUtxos xs =
  List.sortOn (Text.length . fst)
    . Map.toList
    . Map.fromListWith (+)
    $ map (\result -> (DB.utxoAddress result, getTxOutValue $ DB.utxoTxOutW result)) xs

isRedeemTextAddress :: Text -> Bool
isRedeemTextAddress addr =
  -- Only Byron has redeem addresses and only Byron uses Base58 encoding.
  case decodeAddressBase58 addr of
    Right a -> isRedeemAddress a
    Left _ -> False

partitionUtxos :: [(Text, Word64)] -> ([(Text, Word64)], [(Text, Word64)])
partitionUtxos =
  List.partition accept
  where
    -- Accept addresses that for which this predicate is True.
    accept :: (Text, a) -> Bool
    accept (addr, _) =
      Text.length addr <= 180 && not (isRedeemTextAddress addr)

queryAtSlot :: DB.TxOutVariantType -> Word64 -> IO (DB.Ada, [DB.UtxoQueryResult], DB.Ada, Either DB.DbError UTCTime)
queryAtSlot txOutVariantType slotNo =
  -- Run the following queries in a single transaction.
  DB.runDbStandaloneSilent $ do
    (,,,)
      <$> DB.queryGenesisSupply txOutVariantType
      <*> DB.queryUtxoAtSlotNo txOutVariantType slotNo
      <*> DB.queryFeesUpToSlotNo slotNo
      <*> DB.querySlotUtcTime slotNo

reportSlotDate :: Word64 -> Either DB.DbError UTCTime -> IO ()
reportSlotDate slotNo eUtcTime = do
  case eUtcTime of
    Left err -> putStrLn $ "\nDatabase not initialized or not accessible: " <> show err
    Right time -> putStrLn $ "\nSlot number " ++ show slotNo ++ " will occur at " ++ show time ++ ".\n"
  exitSuccess

showUtxo :: (Text, Word64) -> Text
showUtxo (addr, value) =
  mconcat
    [ "    {\n"
    , "      \"address\": \""
    , addr
    , "\",\n"
    , "      \"value\": "
    , textShow value
    , "\n"
    , "    }"
    ]

utxoSetSum :: [DB.UtxoQueryResult] -> DB.Ada
utxoSetSum xs =
  DB.word64ToAda . sum $ map (getTxOutValue . DB.utxoTxOutW) xs

getTxOutValue :: DB.TxOutW -> Word64
getTxOutValue wrapper = case wrapper of
  DB.VCTxOutW txOut -> DB.unDbLovelace $ VC.txOutCoreValue txOut
  DB.VATxOutW txOut _ -> DB.unDbLovelace $ VA.txOutAddressValue txOut

writeUtxos :: FilePath -> [(Text, Word64)] -> IO ()
writeUtxos fname xs = do
  putStrLn $ "Writing file: " ++ fname
  withFile fname WriteMode $ \handle -> do
    Text.hPutStrLn handle "{\n  \"fund\": ["
    mapM_ (Text.hPutStr handle) (List.intersperse ",\n" $ map showUtxo xs)
    Text.hPutStrLn handle "\n  ]\n}"
