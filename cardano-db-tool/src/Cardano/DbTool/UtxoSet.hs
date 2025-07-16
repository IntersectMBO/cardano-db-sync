{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbTool.UtxoSet (
  utxoSetAtSlot,
  utxoSetSum,
) where

import Cardano.Chain.Common (decodeAddressBase58, isRedeemAddress)
import Cardano.Db
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

utxoSetAtSlot :: TxOutVariantType -> Word64 -> IO ()
utxoSetAtSlot txOutTableType slotNo = do
  (genesisSupply, utxoSet, fees, eUtcTime) <- queryAtSlot txOutTableType slotNo

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

aggregateUtxos :: [UtxoQueryResult] -> [(Text, Word64)]
aggregateUtxos xs =
  List.sortOn (Text.length . fst)
    . Map.toList
    . Map.fromListWith (+)
    $ map (\result -> (utxoAddress result, getTxOutValue $ utxoTxOutW result)) xs

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

queryAtSlot :: TxOutVariantType -> Word64 -> IO (Ada, [UtxoQueryResult], Ada, Either LookupFail UTCTime)
queryAtSlot txOutTableType slotNo =
  -- Run the following queries in a single transaction.
  runDbNoLoggingEnv $ do
    (,,,)
      <$> queryGenesisSupply txOutTableType
      <*> queryUtxoAtSlotNo txOutTableType slotNo
      <*> queryFeesUpToSlotNo slotNo
      <*> querySlotUtcTime slotNo

reportSlotDate :: Word64 -> Either a UTCTime -> IO ()
reportSlotDate slotNo eUtcTime = do
  case eUtcTime of
    Left _ -> putStrLn "\nDatabase not initialized or not accessible"
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

utxoSetSum :: [UtxoQueryResult] -> Ada
utxoSetSum xs =
  word64ToAda . sum $ map (getTxOutValue . utxoTxOutW) xs

getTxOutValue :: TxOutW -> Word64
getTxOutValue wrapper = case wrapper of
  CTxOutW txOut -> unDbLovelace $ VC.txOutValue txOut
  VTxOutW txOut _ -> unDbLovelace $ VA.txOutValue txOut

writeUtxos :: FilePath -> [(Text, Word64)] -> IO ()
writeUtxos fname xs = do
  putStrLn $ "Writing file: " ++ fname
  withFile fname WriteMode $ \handle -> do
    Text.hPutStrLn handle "{\n  \"fund\": ["
    mapM_ (Text.hPutStr handle) (List.intersperse ",\n" $ map showUtxo xs)
    Text.hPutStrLn handle "\n  ]\n}"
