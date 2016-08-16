module Empire.API.Data.Breadcrumb where

import Prologue
import Data.Binary (Binary)
import Empire.API.Data.Node (NodeId)

data BreadcrumbItem = Lambda NodeId deriving (Show, Eq, Generic)

newtype Breadcrumb = Breadcrumb { _items :: [BreadcrumbItem] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb

instance Binary Breadcrumb
instance Binary BreadcrumbItem
