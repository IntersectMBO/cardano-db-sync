{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Explorer.Web.Api.Legacy.Types
  ( PageNo (..)
  , PageSize (..)
  ) where

import           Servant.API (FromHttpApiData)


newtype PageNo = PageNo
  { unPageNo :: Word
  } deriving Show

deriving instance FromHttpApiData PageNo

newtype PageSize = PageSize
  { unPageSize :: Word
  } deriving Show

deriving instance FromHttpApiData PageSize
