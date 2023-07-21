{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Cardano.DbSync.Era.Shelley.Generic.Tx.Conway (
  fromConwayTx,
) where

import Prelude
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import Ouroboros.Consensus.Cardano.Block (StandardConway)
import Data.Word (Word64)
import Cardano.DbSync.Era.Shelley.Generic.Tx.Types

fromConwayTx :: Bool -> Maybe Alonzo.Prices -> (Word64, Core.Tx StandardConway) -> Tx
fromConwayTx = undefined
