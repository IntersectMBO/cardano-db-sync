{-# LANGUAGE TypeApplications #-}

module Cardano.Mock.Forging.Crypto
  ( RawSeed (..)
  , mkVRFKeyPair
  , mkSeedFromWords
  ) where

import           Data.Typeable (Proxy (Proxy))
import           Data.Word (Word64)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.Hash (Blake2b_256, hashToBytes, hashWithSerialiser)
import           Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import           Cardano.Crypto.VRF (SignKeyVRF, VRFAlgorithm, VerKeyVRF, deriveVerKeyVRF,
                   genKeyVRF)


instance ToCBOR RawSeed where
  toCBOR (RawSeed w1 w2 w3 w4 w5) = toCBOR (w1, w2, w3, w4, w5)
  encodedSizeExpr size _ = 1 + size (Proxy :: Proxy Word64) * 5

data RawSeed = RawSeed !Word64 !Word64 !Word64 !Word64 !Word64
  deriving (Eq, Show)

-- | For testing purposes, generate a deterministic VRF key pair given a seed.
mkVRFKeyPair :: VRFAlgorithm v => RawSeed -> (SignKeyVRF v, VerKeyVRF v)
mkVRFKeyPair seed =
  let sk = genKeyVRF $ mkSeedFromWords seed
   in (sk, deriveVerKeyVRF sk)

-- | Construct a seed from a bunch of Word64s
--
--   We multiply these words by some extra stuff to make sure they contain
--   enough bits for our seed.
mkSeedFromWords :: RawSeed -> Seed
mkSeedFromWords stuff =
  mkSeedFromBytes . hashToBytes $ hashWithSerialiser @Blake2b_256 toCBOR stuff
