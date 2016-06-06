{-# LANGUAGE DeriveGeneric #-}

module Empire.ResultSaver.ProjectDump where

import           Data.Binary                       (Binary)
import           Data.Map.Lazy                     (Map)
import           Empire.API.Data.Node              (Node, NodeId)
import           Empire.API.Data.Graph             (Graph)
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import           Prologue


data ProjectDump = ProjectDump { _program :: Graph
                               , _nodes   :: Map NodeId Node
                               , _results :: Map NodeId NodeResultUpdate.NodeValue
                               } deriving (Show, Generic)

makeLenses ''ProjectDump

instance Binary ProjectDump
