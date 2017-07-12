{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module LunaStudio.Data.PortRef
    ( module LunaStudio.Data.PortRef
    , nodeLoc
    ) where

import           Control.DeepSeq         (NFData)
import           Data.Binary             (Binary)
import           LunaStudio.Data.Node    (NodeId)
import           LunaStudio.Data.NodeLoc (NodeLoc)
import qualified LunaStudio.Data.NodeLoc as NodeLoc
import           LunaStudio.Data.Port    (InPortId, OutPortId)
import           Prologue

data PortRef a = PortRef { _nodeLoc :: NodeLoc
                         , _portId  :: a
                         } deriving (Eq, Generic, NFData, Ord, Show)

type OutPortRef = PortRef OutPortId
type InPortRef  = PortRef InPortId
type AnyPortRef = forall a. PortRef a

makeLenses ''PortRef
makePrisms ''PortRef

instance Binary InPortRef
instance Binary OutPortRef

{-# DEPRECATED nodeId "Use nodeLoc" #-}
nodeId :: Lens' (PortRef a) NodeId
nodeId = nodeLoc . NodeLoc.nodeId
