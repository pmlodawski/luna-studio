module Empire.API.Data.Breadcrumb where

import Prologue
import Data.Binary (Binary)
import Empire.API.Data.Node (NodeId)

data BreadcrumbItem = Lambda NodeId   deriving (Show, Eq, Generic)
data Named a        = Named  String a deriving (Show, Eq, Generic)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb

instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance Binary BreadcrumbItem
