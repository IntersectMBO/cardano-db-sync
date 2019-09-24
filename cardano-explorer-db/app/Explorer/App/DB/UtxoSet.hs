module Explorer.App.DB.UtxoSet
  ( utxoSetAtBlock
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Word (Word64)

import           Explorer.DB

utxoSetSum :: [(TxOut, a)] -> Ada
utxoSetSum xs =
  word64ToAda . sum $ map (txOutValue . fst) xs

utxoSetAtBlock :: Word64 -> IO ()
utxoSetAtBlock blkNo =
  runDbNoLogging $ do
    gs <- queryGenesisSupply
    liftIO $ putStrLn ("Genesis supply: " ++ show gs ++ " Ada")
    utxoSet <- queryUtxoAtBlockNo blkNo
    liftIO $ putStrLn ("Supply at block number " ++ show blkNo ++ ": " ++ show (utxoSetSum utxoSet) ++ " Ada")
    ts <- queryTotalSupply
    liftIO $ putStrLn ("Supply at chain head: " ++ show ts ++ " Ada")
