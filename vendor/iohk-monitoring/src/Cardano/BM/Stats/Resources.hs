{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.BM.Stats.Resources
  ( Resources(..)
  , ResourceStats
  , Word64
  )
where

import           Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), ToJSON (..),
                   defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

-- | Concrete data provided by 'readResourceStats'.
--   (See platform-specific files in Cardano.BM.Counters)
type ResourceStats = Resources Word64

-- * HKD for resources used by the process.
--
data Resources a
  = Resources
      { rCentiCpu   :: !a
      , rCentiGC    :: !a
      , rCentiMut   :: !a
      , rGcsMajor   :: !a
      , rGcsMinor   :: !a
      , rAlloc      :: !a
      , rLive       :: !a
      , rHeap       :: !a
      , rRSS        :: !a
      , rCentiBlkIO :: !a
      , rThreads    :: !a
      }
  deriving (Functor, Generic, Show)

instance Applicative Resources where
  pure a = Resources a a a a a a a a a a a
  f <*> x =
    Resources
    { rCentiCpu   = rCentiCpu   f (rCentiCpu   x)
    , rCentiGC    = rCentiGC    f (rCentiGC    x)
    , rCentiMut   = rCentiMut   f (rCentiMut   x)
    , rGcsMajor   = rGcsMajor   f (rGcsMajor   x)
    , rGcsMinor   = rGcsMinor   f (rGcsMinor   x)
    , rAlloc      = rAlloc      f (rAlloc      x)
    , rLive       = rLive       f (rLive       x)
    , rHeap       = rHeap       f (rHeap       x)
    , rRSS        = rRSS        f (rRSS        x)
    , rCentiBlkIO = rCentiBlkIO f (rCentiBlkIO x)
    , rThreads    = rThreads    f (rThreads    x)
    }

-- * JSON
--
encodingOptions :: Options
encodingOptions = defaultOptions
  { fieldLabelModifier     = drop 1
  , tagSingleConstructors  = True
  , sumEncoding =
      -- This is a copy of defaultTaggedObject in the `aeson` library, but using that generates a
      -- pattern match error with `ghc-9.2` but is accepted by `ghc-8.10`.
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/15656
      TaggedObject
       { tagFieldName = "kind"
       , contentsFieldName = "contents"
       }
  }

instance FromJSON a => FromJSON (Resources a) where
  parseJSON = genericParseJSON encodingOptions

instance ToJSON a => ToJSON (Resources a) where
  toJSON = genericToJSON encodingOptions
  toEncoding = genericToEncoding encodingOptions
