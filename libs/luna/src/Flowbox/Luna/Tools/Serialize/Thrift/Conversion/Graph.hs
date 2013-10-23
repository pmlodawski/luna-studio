---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph where

import           Flowbox.Prelude                                        
import qualified Data.Graph.Inductive.Graph                             
import qualified Data.HashMap.Strict                                  as HashMap
import           Data.HashMap.Strict                                    (HashMap)
import           Data.Int                                               
import qualified Data.Text.Lazy                                       as Text
import qualified Data.Vector                                          as Vector

import qualified Attrs_Types                                          as TAttrs
import qualified Graph_Types                                          as TGraph
import           Flowbox.Control.Error                                  
import           Flowbox.Luna.Network.Attributes                        (Attributes)
import           Flowbox.Luna.Network.Flags                             (Flags)
import           Flowbox.Luna.Network.Graph.Value                       (Value(Value))
import           Flowbox.Luna.Network.Graph.Edge                        (Edge(Edge))
import qualified Flowbox.Luna.Network.Graph.Graph                     as Graph
import           Flowbox.Luna.Network.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Network.Graph.Node                      as Node
import           Flowbox.Luna.Network.Graph.Node                        (Node)
import qualified Flowbox.Luna.Network.Graph.Port                      as Port
import           Flowbox.Luna.Network.Graph.Port                        (Port)
import           Flowbox.Tools.Conversion                               
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs   ()



encodeGraph :: (Convert (Int, t) v, Convert (Graph.LEdge b) a,
                Data.Graph.Inductive.Graph.Graph gr) 
            => gr t b -> (Maybe (HashMap Int32 v), Maybe (Vector.Vector a))
encodeGraph agraph = (Just nodes, Just edges) where 
    nodes = HashMap.fromList $
        map (\(a, b) -> (itoi32 a, encode (a, b))) $ Graph.labNodes agraph
    edges =  Vector.fromList $ map encode $ Graph.labEdges agraph
     

decodeGraph :: (Data.Graph.Inductive.Graph.Graph gr, Convert (Graph.LEdge b) a)
            => (Maybe (HashMap Int32 TGraph.Node), Maybe (Vector.Vector a))
            -> Either [Char] (gr Node b)
decodeGraph (mtnodes, mtedges) = case mtnodes of
    Nothing    -> Left "'nodes' field is missing"
    Just tnodes -> case mtedges of
        Nothing    -> Left "Edges are not defined" 
        Just tedges ->
            let transformNode :: (Int32, TGraph.Node) -> Either String (Int, Node)
                transformNode (i, lab) = case decode lab of
                    Left msg         -> Left msg
                    Right (_, node)  -> Right (i32toi i, node)
                goodNodes = sequence $ map transformNode $
                    HashMap.toList tnodes 

                goodEdges =  sequence $ map decode $ Vector.toList tedges
            in case goodNodes of
                Left msg         -> Left msg
                Right gnodes     -> case goodEdges of
                    Left msg     -> Left msg
                    Right gedges -> Right $ Graph.mkGraph gnodes gedges


instance Convert Port TGraph.Port where
    encode port = case port of 
        Port.All      -> TGraph.Port (Just TGraph.All   ) Nothing
        Port.Number p -> TGraph.Port (Just TGraph.Number) (Just $ itoi32 p)
    decode (TGraph.Port mtcls mtnumber) = case mtcls of 
        Just TGraph.All    -> return $ Port.All
        Just TGraph.Number -> do tnumber <- mtnumber <?> "Failed to decode Port: 'number' field is missing"
                                 return $ Port.Number $ i32toi tnumber
        Nothing            -> Left "Failed to decode Port: 'cls' field is missing"



instance Convert (Int, Int, Edge) TGraph.Edge where
    encode (nodeSrc, nodeDst, Edge portSrc portDst) =  
        let  mtportSrc = Just $ encode portSrc
             mtportDst = Just $ encode portDst
        in TGraph.Edge (Just $ itoi32 nodeSrc) (Just $ itoi32 nodeDst) mtportSrc mtportDst
    decode (TGraph.Edge mtnodeSrc mtnodeDst mtportSrc mtportDst) = do
        tnodeSrc   <- mtnodeSrc <?> "Failed to decode Edge: 'srcNode' field is missing"
        tnodeDst   <- mtnodeDst <?> "Failed to decode Edge: 'dstNode' field is missing"
        tportSrc   <- mtportSrc <?> "Failed to decode Edge: 'srcPort' field is missing"
        tportDst   <- mtportDst <?> "Failed to decode Edge: 'dstPort' field is missing"
        portSrc    <- decode tportSrc
        portDst    <- decode tportDst
        return $ (i32toi tnodeSrc, i32toi tnodeDst, Edge portSrc portDst)


instance Convert Graph TGraph.Graph where
    encode agraph = TGraph.Graph n e where 
        (n, e) = encodeGraph agraph
    decode (TGraph.Graph mtnodes mtedges) = decodeGraph (mtnodes, mtedges)


instance Convert Value TGraph.Value where
  encode (Value v) = TGraph.Value (Just $ Text.pack v)

  decode (TGraph.Value mtvalue) = do 
      tvalue <- mtvalue <?> "Failed to decode Value: 'value' field is missing"
      return $ Value $ Text.unpack tvalue


instance Convert (Int, Node) TGraph.Node where
  encode (nid, a) =
    let
      nodeType :: TGraph.NodeType
      nodeType = case a of
                   Node.Expr    {} -> TGraph.Expr
                   Node.Default {} -> TGraph.Default
                   Node.Inputs  {} -> TGraph.Inputs
                   Node.Outputs {} -> TGraph.Outputs
                   Node.Tuple   {} -> TGraph.Tuple

      nodeExpression :: Maybe Text.Text
      nodeExpression = fmap Text.pack $ case a of
                   Node.Expr texpression _ _ -> Just texpression
                   _                         -> Nothing

      nodeID :: Int32
      nodeID = itoi32 nid

      nodeFlags :: Maybe TAttrs.Flags
      nodeFlags = fmap encode $ case a of
                   Node.Expr  _ aflags _ -> Just aflags
                   Node.Inputs  aflags _ -> Just aflags
                   Node.Outputs aflags _ -> Just aflags
                   Node.Tuple   aflags _ -> Just aflags
                   _                     -> Nothing

      nodeAttrs :: TAttrs.Attributes
      nodeAttrs = encode $ Node.attributes a

      defValue :: Maybe TGraph.Value
      defValue = fmap encode $ case a of
                   Node.Default val _ -> Just val
                   _                  -> Nothing
    in
      TGraph.Node (Just nodeType) nodeExpression (Just nodeID) nodeFlags (Just nodeAttrs) defValue
  decode b =
    let
      gnexpression = case TGraph.f_Node_expression b of
                  Just nexpression -> Right $ Text.unpack nexpression
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

      gdefval :: Either String Value
      gdefval = case TGraph.f_Node_value b of
                 Just ndefval -> decode ndefval
                 Nothing      -> Left "Default value not defined"

      gnode :: Either String Node
      gnode =  case TGraph.f_Node_cls b of
            Just ntype ->
              case ntype of
                TGraph.Expr -> do
                  ggexpression  <- gnexpression
                  ggflags <- gflags
                  ggattrs <- gattrs
                  Right $ Node.Expr ggexpression ggflags ggattrs
                TGraph.Default -> do
                  ggdefval <- gdefval
                  ggattrs  <- gattrs
                  Right $ Node.Default ggdefval ggattrs
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
            Nothing     -> Left "Node type not defined"

    in
      do
        ggnode <- gnode
        ggID <- gID
        Right $ (ggID, ggnode)
       
  
      
