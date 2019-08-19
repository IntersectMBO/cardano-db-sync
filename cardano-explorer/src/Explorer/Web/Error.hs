{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Types describing runtime errors related to Explorer

module Explorer.Web.Error
       ( ExplorerError (..)
       ) where

import           Control.Exception
import           Data.Aeson.TH        (defaultOptions, deriveToJSON)
import           Data.Text            (Text)
import           Formatting           (bprint, stext, (%))
import qualified Formatting.Buildable
import           GHC.Generics         (Generic)

newtype ExplorerError =
    -- | Some internal error.
    Internal Text
    deriving (Show, Generic)

instance Exception ExplorerError

instance Formatting.Buildable.Buildable ExplorerError where
    build (Internal msg) = bprint ("Internal explorer error ("%stext%")") msg

deriveToJSON defaultOptions ''ExplorerError
