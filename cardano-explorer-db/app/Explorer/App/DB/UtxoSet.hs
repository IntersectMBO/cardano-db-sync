{-# LANGUAGE OverloadedStrings #-}
module Explorer.App.DB.UtxoSet
  ( utxoSetAtBlock
  ) where

import           Cardano.Chain.Common (fromCBORTextAddress, isRedeemAddress)
import           Control.Monad (when)

import           Data.Word (Word64)

import           Explorer.DB

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           System.IO (IOMode (..), withFile)

utxoSetAtBlock :: Word64 -> IO ()
utxoSetAtBlock slotNo = do
  (genesisSupply, utxoSet, fees) <-
        -- Run the following queries in a single transaction.
        runDbNoLogging $ do
            (,,) <$> queryGenesisSupply
                    <*> queryUtxoAtSlotNo slotNo
                    <*> queryFeesUpToSlotNo slotNo

  let supply = utxoSetSum utxoSet
  let aggregated = aggregateUtxos utxoSet
  let (accept, reject) = partitionUtxos aggregated

  putStrLn $ unlines
      [ "\nGenesis supply: " ++ show genesisSupply ++ " Ada"
      , "\nAt slot number " ++ show slotNo ++ ":"
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
      ]

  when (length aggregated > 0) $ do
    writeUtxos ("utxo-accept-" ++ show slotNo ++ ".json") accept
    writeUtxos ("utxo-reject-" ++ show slotNo ++ ".json") reject

  putStrLn ""

-- -----------------------------------------------------------------------------

aggregateUtxos :: [(TxOut, a)] -> [(Text, Word64)]
aggregateUtxos xs =
  List.sortOn (Text.length . fst)
    . Map.toList
    . Map.fromListWith (+)
    $ map (\(x, _) -> (txOutAddress x, txOutValue x)) xs

isNotRedeemTextAddress :: Text -> Bool
isNotRedeemTextAddress addr =
  case fromCBORTextAddress addr of
    Left e -> error $ "isNotRedeemTextAddress: cannot decode " ++ show addr ++ ": " ++ show e
    Right a -> not $ isRedeemAddress a

partitionUtxos :: [(Text, Word64)] -> ([(Text, Word64)], [(Text, Word64)])
partitionUtxos =
    List.partition accept
  where
    -- Accept addresses that for which this predicate is True.
    accept :: (Text, a) -> Bool
    accept (addr, _) =
      Text.length addr <= 180 && isNotRedeemTextAddress addr

showUtxo :: (Text, Word64) -> Text
showUtxo (addr, value) =
  mconcat
    [ "    {\n"
    , "      \"address\": \"",  addr, "\",\n"
    , "      \"value\": ", textShow value, "\n"
    , "    }"
    ]

textShow :: Show a => a -> Text
textShow = Text.pack . show

utxoSetSum :: [(TxOut, a)] -> Ada
utxoSetSum xs =
  word64ToAda . sum $ map (txOutValue . fst) xs

writeUtxos :: FilePath -> [(Text, Word64)] -> IO ()
writeUtxos fname xs = do
  putStrLn $ "Writing file: " ++ fname
  withFile fname WriteMode $ \ handle -> do
    Text.hPutStrLn handle "{\n  \"fund\": ["
    mapM_ (Text.hPutStr handle) (List.intersperse ",\n" $ map showUtxo xs)
    Text.hPutStrLn handle "\n  ]\n}"
