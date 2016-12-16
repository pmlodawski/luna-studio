module Empire.API.Request where

import           Prologue
import           Data.Aeson (ToJSON)
import           Data.UUID.Aeson ()
import           Data.Binary (Binary)
import           Data.UUID.Types (UUID)
import qualified Empire.API.Topic as T

data Request a = Request { _requestId :: UUID
                         , _request   :: a
                         } deriving (Show, Eq, Generic)

makeLenses ''Request

instance (Binary a) => Binary (Request a)
instance (ToJSON a) => ToJSON (Request a)
