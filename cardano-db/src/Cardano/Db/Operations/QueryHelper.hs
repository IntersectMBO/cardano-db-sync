{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Db.Operations.QueryHelper where

import Cardano.Db.Schema.BaseSchema
import Cardano.Db.Types
import Data.Fixed (Micro)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Database.Esqueleto.Experimental (
  Entity (..),
  PersistField,
  SqlExpr,
  Value (unValue),
  ValueList,
  from,
  in_,
  isNothing,
  not_,
  subList_select,
  table,
  unSqlBackendKey,
  val,
  where_,
  (<=.),
  (^.),
 )

-- Filter out 'Nothing' from a 'Maybe a'.
isJust :: PersistField a => SqlExpr (Value (Maybe a)) -> SqlExpr (Value Bool)
isJust = not_ . isNothing

-- every tx made before or at the snapshot time
txLessEqual :: BlockId -> SqlExpr (ValueList TxId)
txLessEqual blkid =
  subList_select $
    from (table @Tx) >>= \tx -> do
      where_ $ tx ^. TxBlockId `in_` blockLessEqual
      pure $ tx ^. TxId
  where
    -- every block made before or at the snapshot time
    blockLessEqual :: SqlExpr (ValueList BlockId)
    blockLessEqual =
      subList_select $
        from (table @Block) >>= \blk -> do
          where_ $ blk ^. BlockId <=. val blkid
          pure $ blk ^. BlockId

maybeToEither :: e -> (a -> b) -> Maybe a -> Either e b
maybeToEither e f = maybe (Left e) (Right . f)

-- | Get the UTxO set after the specified 'BlockNo' has been applied to the chain.
-- Unfortunately the 'sum_' operation above returns a 'PersistRational' so we need
-- to un-wibble it.
unValueSumAda :: Maybe (Value (Maybe Micro)) -> Ada
unValueSumAda mvm =
  case fmap unValue mvm of
    Just (Just x) -> lovelaceToAda x
    _otherwise -> Ada 0

entityPair :: Entity a -> (Key a, a)
entityPair e =
  (entityKey e, entityVal e)

unBlockId :: BlockId -> Word64
unBlockId = fromIntegral . unSqlBackendKey . unBlockKey

unTxId :: TxId -> Word64
unTxId = fromIntegral . unSqlBackendKey . unTxKey

unTxInId :: TxInId -> Word64
unTxInId = fromIntegral . unSqlBackendKey . unTxInKey

defaultUTCTime :: UTCTime
defaultUTCTime = read "2000-01-01 00:00:00.000000 UTC"

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)

unValue3 :: (Value a, Value b, Value c) -> (a, b, c)
unValue3 (a, b, c) = (unValue a, unValue b, unValue c)

unValue4 :: (Value a, Value b, Value c, Value d) -> (a, b, c, d)
unValue4 (a, b, c, d) = (unValue a, unValue b, unValue c, unValue d)

unValue5 :: (Value a, Value b, Value c, Value d, Value e) -> (a, b, c, d, e)
unValue5 (a, b, c, d, e) = (unValue a, unValue b, unValue c, unValue d, unValue e)
