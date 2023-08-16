module Cardano.DbSync.DepositsMap where

import Cardano.Ledger.Coin
import Data.ByteString (ByteString)
import Data.Strict.Map (Map)
import qualified Data.Strict.Map as Map

newtype DepositsMap = DepositsMap {unDepositsMap :: Map ByteString Coin}

lookup :: ByteString -> DepositsMap -> Maybe Coin
lookup bs = Map.lookup bs . unDepositsMap

empty :: DepositsMap
empty = DepositsMap Map.empty
