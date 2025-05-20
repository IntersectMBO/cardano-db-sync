{-# LANGUAGE TypeApplications #-}

module Cardano.DbTool.Validate.PoolOwner (
  validateAllPoolsHaveOwners,
) where

import Cardano.Db
import Cardano.DbTool.Validate.Util
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Esqueleto.Experimental (
  SqlBackend,
  Value (..),
  countRows,
  from,
  notExists,
  select,
  table,
  unValue,
  where_,
  (==.),
  (^.),
 )

validateAllPoolsHaveOwners :: IO ()
validateAllPoolsHaveOwners = do
  putStrF "All pools have owners : "
  count <- runDbNoLoggingEnv queryPoolsWithoutOwners
  if count == 0
    then putStrLn $ greenText "ok"
    else putStrLn $ redText ("Failed, " ++ show count ++ " pools are without owners.")

-- -----------------------------------------------------------------------------

-- select * from pool_hash
--  where not exists (select * from pool_owner where pool_owner.pool_hash_id = pool_hash.id) ;

queryPoolsWithoutOwners :: MonadIO m => DB.DbAction m Int
queryPoolsWithoutOwners = do
  res <- select $ do
    pupd <- from $ table @PoolUpdate
    where_ . notExists $ do
      powner <- from (table @PoolOwner)
      where_ (pupd ^. PoolUpdateId ==. powner ^. PoolOwnerPoolUpdateId)
    pure countRows

  pure $ maybe 0 unValue (listToMaybe res)
