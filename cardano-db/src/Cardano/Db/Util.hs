module Cardano.Db.Util where

import Cardano.Db.Schema.BaseSchema
import Data.Word (Word64)

toTxId :: BlockId -> Word64 -> TxId
toTxId blockId blockIndex = TxKey $ 100000 * unBlockKey blockId + fromIntegral blockIndex
