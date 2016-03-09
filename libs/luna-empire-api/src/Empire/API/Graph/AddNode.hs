module Empire.API.Graph.AddNode where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Update             as Update

data NodeType = InputNode { _name :: String
                          , _tpe  :: String
                          }
              | ExpressionNode { _expression :: String }
              deriving (Generic, Show, Eq)

data Request = Request { _location  :: GraphLocation
                       , _nodeType  :: NodeType
                       , _nodeMeta  :: NodeMeta
                       , _connectTo :: Maybe Int
                       , _tag       :: Int
                       } deriving (Generic, Show, Eq)

type Update = Update.SimpleUpdate Request

makeLenses ''Request

instance Binary NodeType
instance Binary Request
