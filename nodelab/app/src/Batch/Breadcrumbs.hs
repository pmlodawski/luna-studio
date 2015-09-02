module Batch.Breadcrumbs where

import           Utils.PreludePlus
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs as GenBreadcrumbs

newtype Breadcrumbs = Breadcrumbs { unBreadcrumbs :: GenBreadcrumbs.Breadcrumbs } deriving (Show, Eq)
