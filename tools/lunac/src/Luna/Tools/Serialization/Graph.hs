---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Graph where

--import Control.Monad
--import qualified Data.MultiMap as MMap

import qualified Data.Graph.Inductive       as DG
import qualified Data.HashMap.Strict        as Map
import           Data.Int
import qualified Data.Text.Lazy             as Text
import qualified Data.Vector                as Vector
import           Data.Vector                  (Vector(..))

import qualified Attrs_Types
import qualified Graph_Types
import qualified Luna.Network.Attributes as Attributes
import           Luna.Network.Attributes   (Attributes(..))
import qualified Luna.Network.Def.NodeDef as NodeDef  
import           Luna.Network.Def.NodeDef   (NodeDef(..))
import qualified Luna.Network.Flags as Flags
import           Luna.Network.Flags   (Flags(..))
import qualified Luna.Network.Graph.DefaultValue as DefaultValue
import           Luna.Network.Graph.DefaultValue   (DefaultValue(..))
import qualified Luna.Network.Graph.Edge as Edge
import           Luna.Network.Graph.Edge   (Edge(..))
import qualified Luna.Network.Graph.Graph as Graph
import           Luna.Network.Graph.Graph   (Graph(..))
import qualified Luna.Network.Graph.Node as Node
import           Luna.Network.Graph.Node   (Node)
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Attrs


instance Serialize Edge Graph_Types.Edge where
  encode a = 
    let 
      src = itoi32 $ Edge.src a
      dst = itoi32 $ Edge.dst a
    in Graph_Types.Edge (Just src) (Just dst)
  decode b =
    case Graph_Types.f_Edge_src b of
      Just src ->
        case Graph_Types.f_Edge_dst b of
          Just dst ->
            let
              srci = i32toi src
              dsti = i32toi dst
            in Right $ Edge srci dsti
          Nothing  -> Left "No destination specified"
      Nothing  -> Left "No source specified!"

instance Serialize Graph Graph_Types.Graph where
  encode a = 
    let
      nodes :: Map.HashMap Int32 Graph_Types.Node
      nodes = Map.fromList $
        map (\(a, b) -> (itoi32 a, encode (b, a))) $ Graph.labNodes a
      edges :: Vector Graph_Types.Edge
      edges = Vector.fromList $ map encode $
        map (\(_, _, label) -> label) $ Graph.labEdges a
    in
      Graph_Types.Graph (Just nodes) $ Just edges
  decode b =
    case Graph_Types.f_Graph_nodes b of
      Just nodes ->
        case Graph_Types.f_Graph_edges b of
          Just edges ->
            let
              goodNodes :: [DG.LNode Node]
              goodNodes = undefined
              goodEdges :: [DG.LEdge Edge]
              goodEdges = undefined
            in Right $ Graph.mkGraph goodNodes goodEdges
          Nothing    -> Left "Edges are not defined"
      Nothing    -> Left "Nodes are not defined"

instance Serialize DefaultValue Graph_Types.DefaultValue where
  encode a =
    case a of
      DefaultInt ii ->
        Graph_Types.DefaultValue (Just Graph_Types.IntV) (Just $ itoi32 ii) Nothing
      DefaultString ss ->
        Graph_Types.DefaultValue (Just Graph_Types.StringV) Nothing (Just $ Text.pack ss)
  decode b =
    case Graph_Types.f_DefaultValue_cls b of
      Just Graph_Types.IntV ->
        case Graph_Types.f_DefaultValue_i b of
          Just ii -> Right $ DefaultInt $ i32toi ii
          Nothing -> Left "No integral default value specified"
      Just Graph_Types.StringV ->
        case Graph_Types.f_DefaultValue_s b of
          Just ss -> Right $ DefaultString $ Text.unpack ss
          Nothing -> Left "No string default value specified"
      Nothing -> Left "No default value type specified"    

instance Serialize (Node, Int) Graph_Types.Node where
  encode (a, nid) =
    let
      nodeType :: Graph_Types.NodeType
      nodeType = case a of
                   Node.Type {}    -> Graph_Types.Type
                   Node.Call {}    -> Graph_Types.Call
                   Node.Default {} -> Graph_Types.Default
                   Node.Inputs {}  -> Graph_Types.Inputs
                   Node.Outputs {} -> Graph_Types.Outputs
                   Node.Tuple {}   -> Graph_Types.Tuple
                   Node.New {}     -> Graph_Types.New
      nodeName :: Maybe Text.Text
      nodeName = fmap Text.pack $ case a of
                   Node.Type tname _ _ -> Just tname
                   Node.Call cname _ _ -> Just cname
                   _                   -> Nothing
      nodeID :: Int32
      nodeID = itoi32 nid
      nodeFlags :: Maybe Attrs_Types.Flags
      nodeFlags = fmap encode $ case a of
                   Node.Type _ flags _ -> Just flags
                   Node.Call _ flags _ -> Just flags
                   _                   -> Nothing
      nodeAttrs :: Maybe Attrs_Types.Attributes
      nodeAttrs = fmap encode $ case a of
                   Node.Inputs  _ attrs -> Just attrs
                   Node.Outputs _ attrs -> Just attrs
                   Node.Tuple   _ attrs -> Just attrs
                   Node.New     _ attrs -> Just attrs
                   _                    -> Nothing

      defValue :: Maybe Graph_Types.DefaultValue
      defValue = fmap encode $ case a of
                   Node.Default val -> Just val
                   _                -> Nothing
    in
      Graph_Types.Node (Just nodeType) nodeName (Just nodeID) nodeFlags nodeAttrs defValue
  decode b =
    let
      gname = case Graph_Types.f_Node_name b of
                  Just nname -> Right $ Text.unpack nname
                  Nothing    -> Left "Node name not defined"

      gID = case Graph_Types.f_Node_nodeID b of
              Just nid -> Right $ i32toi nid
              Nothing  -> Left "Node ID not defined"

      gflags :: Either String Flags
      gflags = case Graph_Types.f_Node_flags b of
                 Just nflags -> decode nflags
                 Nothing     -> Left "Node flags not defined"
      
      gattrs :: Either String Attributes
      gattrs = case Graph_Types.f_Node_attrs b of
                Just nattrs -> decode nattrs
                Nothing     -> Left "Node attributes not defined"

      gdefval :: Either String DefaultValue
      gdefval = case Graph_Types.f_Node_defVal b of
                 Just ndefval -> decode ndefval
                 Nothing      -> Left "Default value not defined"

      gnode :: Either String Node
      gnode =  case Graph_Types.f_Node_cls b of
            Just ntype ->
              case ntype of
                Graph_Types.Type -> do
                  ggname <- gname
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Type ggname ggflags ggattrs
                Graph_Types.Call -> do
                  ggname <- gname
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Call ggname ggflags ggattrs
                Graph_Types.Default -> do
                  ggdefval <- gdefval
                  Right $ Node.Default ggdefval
                Graph_Types.New -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.New ggflags ggattrs
                Graph_Types.Inputs -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Inputs ggflags ggattrs
                Graph_Types.Outputs -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Outputs ggflags ggattrs
                Graph_Types.Tuple -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Tuple ggflags ggattrs
            Nothing     -> Left "Node type not defined"

    in
      do
        ggnode <- gnode
        ggID <- gID
        Right $ (ggnode, ggID)
       
  
      
