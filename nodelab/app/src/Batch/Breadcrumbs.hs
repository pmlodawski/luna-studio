module Batch.Breadcrumbs where

import           Utils.PreludePlus
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs as GenBreadcrumbs
import Data.Aeson (ToJSON)

data Crumb = Module { _name :: String }
           | Function { _name :: String }
           deriving (Show, Eq, Generic)

makeLenses ''Crumb
instance ToJSON Crumb

newtype Breadcrumbs = Breadcrumbs { _crumbs :: [Crumb] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumbs
instance ToJSON Breadcrumbs
