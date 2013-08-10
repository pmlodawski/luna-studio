---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph where

--import Control.Monad
--import qualified Data.MultiMap as MMap

import qualified Data.Graph.Inductive                                      as DG
import qualified Data.HashMap.Strict                                       as Map
import           Data.Int                                                    
import qualified Data.Text.Lazy                                            as Text
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)

import qualified Attrs_Types                                               as TAttrs
import qualified Graph_Types                                               as TGraph
import           Flowbox.Luna.Network.Attributes                             (Attributes)
import           Flowbox.Luna.Network.Flags                                  (Flags(..))
import           Flowbox.Luna.Network.Graph.DefaultValue                     (DefaultValue(..))
import qualified Flowbox.Luna.Network.Graph.Edge                           as Edge
import           Flowbox.Luna.Network.Graph.Edge                             (Edge(..))
import qualified Flowbox.Luna.Network.Graph.Graph                          as Graph
import           Flowbox.Luna.Network.Graph.Graph                            (Graph)
import qualified Flowbox.Luna.Network.Graph.Node                           as Node
import           Flowbox.Luna.Network.Graph.Node                             (Node)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs        ()


instance Convert (Int, Int, Edge) TGraph.Edge where
  encode (nsrc, ndst, a) =  let 
      dst = itoi32 $ Edge.dst a
    in TGraph.Edge (Just dst) (Just $ itoi32 nsrc) (Just $ itoi32 ndst)
  decode b =
    case TGraph.f_Edge_portDst b of
      Just dst ->
        case TGraph.f_Edge_nodeSrc b of
          Just nodeSrc ->
            case TGraph.f_Edge_nodeDst b of
              Just nodeDst ->
                Right $ (i32toi nodeSrc, i32toi nodeDst, Edge (i32toi dst))
              Nothing      -> Left "No destination node specified"
          Nothing      ->  Left "No source node specified"
      Nothing  -> Left "No destination port specified"

instance Convert Graph TGraph.Graph where
  encode a = 
    let
      nodes :: Map.HashMap Int32 TGraph.Node
      nodes = Map.fromList $
        map (\(a, b) -> (itoi32 a, encode (a, b))) $ Graph.labNodes a
      edges :: Vector TGraph.Edge
      edges =  Vector.fromList $ map encode $ Graph.labEdges a
    in
      TGraph.Graph (Just nodes) (Just edges)
  decode b =
    case TGraph.f_Graph_nodes b of
      Just nodes ->
        case TGraph.f_Graph_edges b of
          Just edges ->
            let
              transformNode :: (Int32, TGraph.Node) -> Either String (Int, Node)
              transformNode (i, lab) =
                case decode lab of
                  Right (_, node) -> Right (i32toi i, node)
                  Left msg         -> Left msg
              goodNodes :: Either String [DG.LNode Node]
              goodNodes = sequence $ map transformNode $
                Map.toList nodes 

              goodEdges :: Either String [DG.LEdge Edge]
              goodEdges =  sequence $ map decode $ Vector.toList edges
            in
              case goodNodes of
                Right gnodes ->
                  case goodEdges of
                    Right gedges -> Right $ Graph.mkGraph gnodes gedges
                    Left msg     -> Left msg
                Left msg     -> Left msg
          Nothing    -> Left "Edges are not defined"
      Nothing    -> Left "Nodes are not defined"

instance Convert DefaultValue TGraph.DefaultValue where
  encode a =
    case a of
      DefaultInt ii ->
        TGraph.DefaultValue (Just TGraph.IntV) (Just $ itoi32 ii) Nothing
      DefaultString ss ->
        TGraph.DefaultValue (Just TGraph.StringV) Nothing (Just $ Text.pack ss)
  decode b =
    case TGraph.f_DefaultValue_cls b of
      Just TGraph.IntV ->
        case TGraph.f_DefaultValue_i b of
          Just ii -> Right $ DefaultInt $ i32toi ii
          Nothing -> Left "No integral default value specified"
      Just TGraph.StringV ->
        case TGraph.f_DefaultValue_s b of
          Just ss -> Right $ DefaultString $ Text.unpack ss
          Nothing -> Left "No string default value specified"
      Nothing -> Left "No default value type specified"    

instance Convert (Int, Node) TGraph.Node where
  encode (nid, a) =
    let
      nodeType :: TGraph.NodeType
      nodeType = case a of
                   Node.Type {}    -> TGraph.Type
                   Node.Call {}    -> TGraph.Call
                   Node.Default {} -> TGraph.Default
                   Node.Inputs {}  -> TGraph.Inputs
                   Node.Outputs {} -> TGraph.Outputs
                   Node.Tuple {}   -> TGraph.Tuple
                   Node.NTuple {}  -> TGraph.NTuple
                   Node.New {}     -> TGraph.New
      nodeName :: Maybe Text.Text
      nodeName = fmap Text.pack $ case a of
                   Node.Type tname _ _ -> Just tname
                   Node.Call cname _ _ -> Just cname
                   _                   -> Nothing
      nodeID :: Int32
      nodeID = itoi32 nid
      nodeFlags :: Maybe TAttrs.Flags
      nodeFlags = fmap encode $ case a of
                   Node.Type  _ flags _ -> Just flags
                   Node.Call  _ flags _ -> Just flags
                   Node.Inputs  flags _ -> Just flags
                   Node.Outputs flags _ -> Just flags
                   Node.Tuple   flags _ -> Just flags
                   Node.NTuple   flags _ -> Just flags
                   Node.New     flags _ -> Just flags
                   _                    -> Nothing
      nodeAttrs :: Maybe TAttrs.Attributes
      nodeAttrs = fmap encode $ case a of
                   Node.Type  _ _ attrs -> Just attrs
                   Node.Call  _ _ attrs -> Just attrs
                   Node.Inputs  _ attrs -> Just attrs
                   Node.Outputs _ attrs -> Just attrs
                   Node.Tuple   _ attrs -> Just attrs
                   Node.NTuple   _ attrs -> Just attrs
                   Node.New     _ attrs -> Just attrs
                   _                    -> Nothing

      defValue :: Maybe TGraph.DefaultValue
      defValue = fmap encode $ case a of
                   Node.Default val -> Just val
                   _                -> Nothing
    in
      TGraph.Node (Just nodeType) nodeName (Just nodeID) nodeFlags nodeAttrs defValue
  decode b =
    let
      gname = case TGraph.f_Node_name b of
                  Just nname -> Right $ Text.unpack nname
                  Nothing    -> Left "Node name not defined"

      gID = case TGraph.f_Node_nodeID b of
              Just nid -> Right $ i32toi nid
              Nothing  -> Left "Node ID not defined"

      gflags :: Either String Flags
      gflags = case TGraph.f_Node_flags b of
                 Just nflags -> decode nflags
                 Nothing     -> Left "Node flags not defined"
      
      gattrs :: Either String Attributes
      gattrs = case TGraph.f_Node_attrs b of
                Just nattrs -> decode nattrs
                Nothing     -> Left "Node attributes not defined"

      gdefval :: Either String DefaultValue
      gdefval = case TGraph.f_Node_defVal b of
                 Just ndefval -> decode ndefval
                 Nothing      -> Left "Default value not defined"

      gnode :: Either String Node
      gnode =  case TGraph.f_Node_cls b of
            Just ntype ->
              case ntype of
                TGraph.Type -> do
                  ggname <- gname
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Type ggname ggflags ggattrs
                TGraph.Call -> do
                  ggname <- gname
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Call ggname ggflags ggattrs
                TGraph.Default -> do
                  ggdefval <- gdefval
                  Right $ Node.Default ggdefval
                TGraph.New -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.New ggflags ggattrs
                TGraph.Inputs -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Inputs ggflags ggattrs
                TGraph.Outputs -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Outputs ggflags ggattrs
                TGraph.Tuple -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Tuple ggflags ggattrs
                TGraph.NTuple -> do
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.NTuple ggflags ggattrs
            Nothing     -> Left "Node type not defined"

    in
      do
        ggnode <- gnode
        ggID <- gID
        Right $ (ggID, ggnode)
       
  
      
