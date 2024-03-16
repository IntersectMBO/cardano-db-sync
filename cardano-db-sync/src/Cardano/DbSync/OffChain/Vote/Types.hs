{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.OffChain.Vote.Types where

import Data.Text (Text)

data OffChainVoteData tp = OffChainVote
  { hashAlgorithm :: Text
  , authors :: [Author]
  , body :: Body tp
  , language :: Text -- key is @language
  }

data Author = Author
  { name :: Maybe Text
  , witness :: Witness
  }

data Witness = Witness
  { witnessAlgorithm :: Text
  , publicKey :: Text
  , signature :: Text
  }

data MinimalBody = Body
  { references :: Maybe [Reference]
  , comment :: Maybe Text
  , externalUpdates :: Maybe [ExternalUpdate]
  }

data GABody = GABody
  { minimalBody :: MinimalBody
  , title :: Text -- 80 chars max
  , abstract :: Text -- 2500 chars
  , motivation :: Text
  , rationale :: Text
  }

data Reference = Reference
  { rtype :: Text -- key is @type. It can be "GovernanceMetadata" or "Other"
  , label :: Text
  , uri :: Text
  , referenceHash :: Maybe ReferenceHash
  }

data ReferenceHash = ReferenceHash
  { hashDigest :: Text
  , rhHashAlgorithm :: Text -- key 'hashAlgorithm'
  }

data ExternalUpdate = ExternalUpdate
  { euTitle :: Text -- key 'title'
  , euUri :: Text -- key 'uri'
  }

data OtherOffChainData = OtherOffChainData
data GovernanceOffChainData = OffChainData

class HasBody tp where
  type Body tp
  toMinimal :: Body tp -> MinimalBody

instance HasBody OtherOffChainData where
  type Body OtherOffChainData = MinimalBody
  toMinimal = id

instance HasBody GovernanceOffChainData where
  type Body GovernanceOffChainData = GABody
  toMinimal = minimalBody

