module Cardano.Db.App.Validation
  ( runValidation
  ) where

import           Control.Monad (replicateM_)

import           Data.Word (Word64)

import           Cardano.Db

import           System.Console.ANSI (setSGRCode)
import           System.Console.ANSI.Types (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..))
import           System.IO (hFlush, stdout)
import           System.Random (randomRIO)

runValidation :: Word -> IO ()
runValidation count =
  validateTotalSupplyDecreasing count


data TestParams = TestParams
  { testFirstBlockNo :: Word64
  , testSecondBlockNo :: Word64
  , genesisSupply :: Ada
  }

genTestParameters :: IO TestParams
genTestParameters = do
  mlatest <- runDbNoLogging queryLatestBlockNo
  case mlatest of
    Nothing -> error "Cardano.Db.App.Validation: Empty database"
    Just latest -> do
      block1 <- randomRIO (1, latest - 1)
      TestParams block1
          <$> randomRIO (block1, latest)
          <*> runDbNoLogging queryGenesisSupply


-- | Validate that the total supply is decreasing.
-- This is only true for the Byron error where transaction fees are burnt.
validateTotalSupplyDecreasing :: Word -> IO ()
validateTotalSupplyDecreasing count =
  replicateM_ (fromIntegral count) $ do
    test <- genTestParameters

    putStrF $ "Total supply plus fees at block " ++ show (testFirstBlockNo test)
            ++ " is same as genesis supply: "
    (fee1, supply1) <- runDbNoLogging $ do
                        (,) <$> queryFeesUpToBlockNo (testFirstBlockNo test)
                            <*> fmap2 utxoSetSum queryUtxoAtBlockNo (testFirstBlockNo test)
    if genesisSupply test == supply1 + fee1
      then putStrLn $ greenText "ok"
      else error $ redText (show (genesisSupply test) ++ " /= " ++ show (supply1 + fee1))

    putStrF $ "Validate total supply decreasing from block " ++ show (testFirstBlockNo test)
            ++ " to block " ++ show (testSecondBlockNo test) ++ ": "

    supply2 <- runDbNoLogging $ fmap2 utxoSetSum queryUtxoAtBlockNo (testSecondBlockNo test)
    if supply1 >= supply2
      then putStrLn $ greenText "ok"
      else error $ redText (show supply1 ++ " < " ++ show supply2)

-- -----------------------------------------------------------------------------

codeGreen :: String
codeGreen = setSGRCode [SetColor Foreground Vivid Green]

codeRed :: String
codeRed = setSGRCode [SetColor Foreground Vivid Red]

codeReset :: String
codeReset = setSGRCode [Reset]

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

greenText :: String -> String
greenText s = codeGreen ++ s ++ codeReset

redText :: String -> String
redText s = codeRed ++ s ++ codeReset

putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout

utxoSetSum :: [(TxOut, a)] -> Ada
utxoSetSum xs =
  word64ToAda . sum $ map (txOutValue . fst) xs
