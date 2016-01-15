module Empire.API.Data.Breadcrumb where

import Prologue
import Data.Binary (Binary)

data Breadcrumb = Breadcrumb deriving (Show, Eq, Generic)

makeLenses ''Breadcrumb
instance Binary Breadcrumb
