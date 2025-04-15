{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Report.Synced (
  assertFullySynced,
) where

import qualified Cardano.Db as Db
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Time.Clock as Time
import Database.Esqueleto.Experimental (
  SqlBackend,
  desc,
  from,
  limit,
  orderBy,
  select,
  table,
  unValue,
  where_,
  (^.),
 )
import System.Exit (exitFailure)

assertFullySynced :: IO ()
assertFullySynced = do
  blockTime <- maybe (assertFail Nothing) pure =<< Db.runDbNoLoggingEnv queryLatestBlockTime
  currentTime <- Time.getCurrentTime
  let diff = Time.diffUTCTime currentTime blockTime
  when (diff > 300.0) $
    assertFail (Just $ renderDifftime diff)

assertFail :: Maybe String -> IO a
assertFail mdiff = do
  case mdiff of
    Nothing -> putStrLn "Error: Database is not fully synced."
    Just diff -> putStrLn $ "Error: Database is not fully synced. Currently " ++ diff ++ " behind the tip."
  exitFailure

-- -----------------------------------------------------------------------------

queryLatestBlockTime :: MonadIO m => DB.DbAction m (Maybe UTCTime)
queryLatestBlockTime = do
  res <- select $ do
    blk <- from $ table @Db.Block
    where_ (Db.isJust (blk ^. Db.BlockSlotNo))
    orderBy [desc (blk ^. Db.BlockSlotNo)]
    limit 1
    pure (blk ^. Db.BlockTime)
  pure $ fmap unValue (Db.listToMaybe res)

renderDifftime :: NominalDiffTime -> String
renderDifftime ndt
  | ndt > 3.0 * 24.0 * 3600.0 = show (ceiling (ndt / (24.0 * 3600.0)) :: Word) ++ " days"
  | ndt > 3.0 * 3600.0 = show (ceiling (ndt / 3600.0) :: Word) ++ " hours"
  | ndt > 3.0 * 60.0 = show (ceiling (ndt / 60.0) :: Word) ++ " minutes"
  | otherwise = show (ceiling ndt :: Word) ++ " seconds"
