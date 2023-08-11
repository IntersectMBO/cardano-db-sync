module Cardano.DbSync.DepositsMap where

import Data.ByteString (ByteString)
import Data.Strict.Map (Map)
import qualified Data.Strict.Map as Map
import Cardano.Ledger.Coin

newtype DepositsMap = DepositsMap {unDepositsMap :: Map ByteString Coin}

lookup :: ByteString -> DepositsMap  -> Maybe Coin
lookup bs = Map.lookup bs . unDepositsMap

empty :: DepositsMap
empty = DepositsMap Map.empty

