{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Db.Update
  ( upateDatumBytes
  , upateRedeemerDataBytes
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Cardano.Db.Schema
import           Data.ByteString (ByteString)
import           Database.Persist.Postgresql (SqlBackend)
import           Database.Persist.Class
import           Database.Persist ((=.))

upateDatumBytes :: MonadIO m => DatumId -> ByteString -> ReaderT SqlBackend m ()
upateDatumBytes datumId bytes = update datumId [DatumBytes =. bytes]

upateRedeemerDataBytes :: MonadIO m => RedeemerDataId -> ByteString -> ReaderT SqlBackend m ()
upateRedeemerDataBytes rdmDataId bytes = update rdmDataId [RedeemerDataBytes =. bytes]
