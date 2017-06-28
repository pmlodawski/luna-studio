module LunaStudio.API.Graph.SetNodesMeta where

import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import           LunaStudio.API.Graph.Result   (Result)
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (NodeId)
import           LunaStudio.Data.NodeMeta      (NodeMeta)
import           Prologue


type SingleUpdate = (NodeId, NodeMeta)

data Request = Request { _location :: GraphLocation
                       , _updates  :: [SingleUpdate]
                       } deriving (Eq, Generic, NFData, Show)

data Inverse = Inverse { _prevMeta :: [SingleUpdate]
                       } deriving (Eq, Generic, NFData, Show)

data Update   = Update { _location' :: GraphLocation
                       , _updates'  :: [SingleUpdate]
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Inverse
makeLenses ''Update
instance Binary Request
instance Binary Inverse
instance Binary Update
instance G.GraphRequest Request where location = location


type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.updateMeta"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update