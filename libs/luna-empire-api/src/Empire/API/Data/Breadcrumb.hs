module Empire.API.Data.Breadcrumb where

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Binary          (Binary)
import           Empire.API.Data.Node (NodeId)
import           Prologue

data BreadcrumbItem = Lambda NodeId   deriving (Show, Eq, Generic)
data Named a        = Named  { _name       :: Text
                             , _breadcrumb :: a
                             } deriving (Show, Eq, Generic)

newtype Breadcrumb a = Breadcrumb { _items :: [a] } deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb
makeLenses ''Named

instance Binary a => Binary (Breadcrumb a)
instance Binary a => Binary (Named a)
instance Binary BreadcrumbItem

instance ToJSON a   => ToJSON (Breadcrumb a)
instance ToJSON a   => ToJSON (Named a)
instance               ToJSON BreadcrumbItem

instance FromJSON a => FromJSON (Breadcrumb a)
instance FromJSON a => FromJSON (Named a)
instance               FromJSON BreadcrumbItem
