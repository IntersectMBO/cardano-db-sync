module Test.Cardano.Db.Mock.Examples
  ( mockBlock0
  , mockBlock1
  , mockBlock2
  ) where

import           Cardano.Mock.Forging.Types

mockBlock0 :: MockBlock
mockBlock0 =
  MockBlock
    { txs = []
    , node = NodeId 0
    }

mockBlock1 :: MockBlock
mockBlock1 =
  MockBlock
    { txs = []
    , node = NodeId 1
    }

mockBlock2 :: MockBlock
mockBlock2 =
  MockBlock
    { txs = []
    , node = NodeId 2
    }
