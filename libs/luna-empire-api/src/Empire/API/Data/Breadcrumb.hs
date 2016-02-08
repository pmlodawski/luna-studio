module Empire.API.Data.Breadcrumb where

import Prologue
import Data.Binary (Binary)

data BreadcrumbItem = Module String | Function String deriving (Show, Eq, Generic)

newtype Breadcrumb = Breadcrumb { _items :: [BreadcrumbItem] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb

instance Binary Breadcrumb
instance Binary BreadcrumbItem
