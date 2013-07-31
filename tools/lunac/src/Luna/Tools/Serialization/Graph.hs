---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Luna.Tools.Serialization.Graph where

--import Control.Monad
--import qualified  Data.Graph.Inductive as DG
--import qualified Data.MultiMap as MMap

import qualified Data.HashMap.Strict        as Map
import           Data.Int
import qualified Data.Text.Lazy             as Text
import qualified Data.Vector                as Vector
import           Data.Vector                  (Vector(..))

import qualified Attrs_Types
import qualified Graph_Types
import qualified Luna.Network.Def.NodeDef as NodeDef
import           Luna.Network.Def.NodeDef   (NodeDef(..))
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
      src = fromIntegral $ Edge.src a :: Int32
      dst = fromIntegral $ Edge.dst a :: Int32
    in Graph_Types.Edge (Just src) (Just dst)
  decode b =
    case Graph_Types.f_Edge_src b of
      Just src ->
        case Graph_Types.f_Edge_dst b of
          Just dst ->
            let
              srci = fromIntegral $ src :: Int
              dsti = fromIntegral $ dst :: Int
            in Right $ Edge srci dsti
          Nothing  -> Left "No destination specified"
      Nothing  -> Left "No source specified!"

instance Serialize Graph Graph_Types.Graph where
  encode a = 
    let
      nodes :: Map.HashMap Int32 Graph_Types.Node
      nodes = Map.fromList $
        map (\(a, b) -> (fromIntegral a :: Int32, encode (b, a))) $ Graph.labNodes a
      edges :: Vector Graph_Types.Edge
      edges = Vector.fromList $ map encode $
        map (\(_, _, label) -> label) $ Graph.labEdges a
    in
      Graph_Types.Graph (Just nodes) $ Just edges
  decode b = undefined

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
      nodeID = fromIntegral nid :: Int32
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
    in
      Graph_Types.Node (Just nodeType) nodeName (Just nodeID) nodeFlags nodeAttrs
  decode b = undefined
