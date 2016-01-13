{-# LANGUAGE FlexibleInstances #-}
module Empire.API.JSONInstances where

import Prologue
import Data.Aeson.Types (ToJSON, toJSON)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Empire.API.Response             as Response

import Empire.API.Data.Project         as Project
import Empire.API.Data.Library         as Library
import Empire.API.Data.Breadcrumb      as Breadcrumb
import Empire.API.Data.GraphLocation   as GraphLocation
import Empire.API.Data.Node            as Node
import Empire.API.Data.NodeMeta        as NodeMeta
import Empire.API.Data.Graph           as Graph
import Empire.API.Data.Port            as Port
import Empire.API.Data.PortRef         as PortRef
import Empire.API.Data.Connection      as Connection
import Empire.API.Data.DefaultValue    as DefaultValue

import Empire.API.Graph.AddNode         as AddNode
import Empire.API.Graph.Connect         as Connect
import Empire.API.Graph.Disconnect      as Disconnect
import Empire.API.Graph.RemoveNode      as RemoveNode
import Empire.API.Graph.UpdateNodeMeta  as UpdateNodeMeta
import Empire.API.Graph.NodeUpdate      as NodeUpdate
import Empire.API.Graph.CodeUpdate      as CodeUpdate
import Empire.API.Graph.GetGraph        as GetGraph
import Empire.API.Graph.GetCode        as GetCode
import Empire.API.Graph.SetDefaultValue as SetDefaultValue

import Empire.API.Library.CreateLibrary as CreateLibrary
import Empire.API.Library.ListLibraries as ListLibraries

instance ToJSON Project.Project
instance ToJSON Library.Library
instance ToJSON Breadcrumb.Breadcrumb
instance ToJSON GraphLocation.GraphLocation

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

instance ToJSON PortRef.AnyPortRef
instance ToJSON PortRef.OutPortRef
instance ToJSON PortRef.InPortRef

instance ToJSON Connection.Connection

instance ToJSON DefaultValue.Value
instance ToJSON DefaultValue.PortDefault

instance ToJSON Graph.Graph

instance ToJSON AddNode.Request
instance ToJSON AddNode.Update

instance ToJSON Connect.Request

instance ToJSON Disconnect.Request

instance ToJSON RemoveNode.Request
instance ToJSON RemoveNode.Update

instance ToJSON UpdateNodeMeta.Request
instance ToJSON UpdateNodeMeta.Update

instance ToJSON NodeUpdate.Update
instance ToJSON CodeUpdate.Update

instance ToJSON GetGraph.Request
instance ToJSON GetGraph.Status

instance ToJSON GetCode.Request
instance ToJSON GetCode.Status

instance ToJSON SetDefaultValue.Request

instance ToJSON CreateLibrary.Request
instance ToJSON CreateLibrary.Update

instance ToJSON ListLibraries.Request
instance ToJSON ListLibraries.Status

instance (ToJSON req, ToJSON upd) => ToJSON (Response.Response req upd)
instance ToJSON Response.ResultOk

