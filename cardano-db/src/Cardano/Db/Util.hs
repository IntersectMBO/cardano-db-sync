module Cardano.Db.Util where

import Data.Word (Word64)
import Cardano.Db.Schema.BaseSchema

toTxId :: BlockId -> Word64 -> TxId
toTxId blockId blockIndex = TxKey $ 1000 * unBlockKey blockId + fromIntegral blockIndex
