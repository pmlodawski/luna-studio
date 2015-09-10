module Batch.Breadcrumbs where

import           Utils.PreludePlus
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs as GenBreadcrumbs

data Crumb = Module { _name :: String }
           | Function { _name :: String }
           deriving (Show, Eq)

makeLenses ''Crumb

newtype Breadcrumbs = Breadcrumbs { _crumbs :: [Crumb] } deriving (Show, Eq)

makeLenses ''Breadcrumbs
