{-# LANGUAGE FlexibleInstances #-}
module Empire.API.JSONInstances where

import Prologue
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Empire.API.Data.Connection    as Connection
import Empire.API.Data.DefaultValue  as DefaultValue
import Empire.API.Data.Library       as Library
import Empire.API.Data.Node          as Node
import Empire.API.Data.NodeMeta      as NodeMeta
import Empire.API.Data.Port          as Port
import Empire.API.Data.Project       as Project
import Empire.API.Graph.AddNode      as AddNode
import Empire.API.Graph.Connect      as Connect
import Empire.API.Graph.Disconnect   as Disconnect

instance ToJSON Connection.Connection
instance ToJSON Connection.AnyPortRef
instance ToJSON Connection.OutPortRef
instance ToJSON Connection.InPortRef

instance ToJSON DefaultValue.Value
instance ToJSON DefaultValue.PortDefault

instance ToJSON Node.Node
instance ToJSON NodeMeta.NodeMeta
instance (ToJSON a, ToJSON b) => ToJSON (Map a b) where
    toJSON = toJSON . Map.toList
    {-# INLINE toJSON #-}

instance ToJSON Port.Port
instance ToJSON Port.InPort
instance ToJSON Port.OutPort
instance ToJSON Port.PortId
instance ToJSON Port.ValueType

instance ToJSON AddNode.AddNode
instance ToJSON Connect.Connect
instance ToJSON Disconnect.Disconnect
