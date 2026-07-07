--
-- copied from https://github.com/input-output-hk/ouroboros-network
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

-- | A shim layer for `Win32-network`'s `IOManager`
--
module Cardano.BM.IOManager
  ( module X
  ) where

import           System.IOManager as X
