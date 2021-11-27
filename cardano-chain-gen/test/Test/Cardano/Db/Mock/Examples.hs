module Test.Cardano.Db.Mock.Examples where

import           Cardano.Mock.Forging.Interpreter

mockBlock0 :: MockBlock
mockBlock0 = MockBlock
  { txs = []
  , node = NodeId 0
  }

mockBlock1 :: MockBlock
mockBlock1 = MockBlock
  { txs = []
  , node = NodeId 1
  }

mockBlock2 :: MockBlock
mockBlock2 = MockBlock
  { txs = []
  , node = NodeId 2
  }
