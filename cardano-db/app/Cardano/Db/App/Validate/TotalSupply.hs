module Cardano.Db.App.Validate.TotalSupply
  ( validateTotalSupplyDecreasing
  ) where

import           Cardano.Db.App.Validate.Util

import           Control.Monad (replicateM_)

import           Data.Word (Word64)

import           Cardano.Db

import           System.Random (randomRIO)


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
