---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.DEP.Data.Serialize.Proto.Conversion.Graph where

import Control.Applicative
import Data.Int            (Int32)

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Graph.Edge                as Gen
import qualified Generated.Proto.Dep.Graph.Graph               as Gen
import qualified Generated.Proto.Dep.Graph.Node                as Gen
import qualified Generated.Proto.Dep.Graph.Node.Cls            as GenNode
import qualified Generated.Proto.Dep.Graph.NodeExpr            as Gen
import qualified Generated.Proto.Dep.Graph.NodeExpr.Cls        as GenNodeExpr
import           Luna.DEP.Data.Serialize.Proto.Conversion.Expr ()
import           Luna.DEP.Graph.Edge                           (Edge)
import qualified Luna.DEP.Graph.Edge                           as Edge
import           Luna.DEP.Graph.Graph                          (Graph)
import qualified Luna.DEP.Graph.Graph                          as Graph
import           Luna.DEP.Graph.Node                           (Node)
import qualified Luna.DEP.Graph.Node                           as Node
import           Luna.DEP.Graph.Node.Expr                      (NodeExpr)
import qualified Luna.DEP.Graph.Node.Expr                      as NodeExpr
import qualified Luna.DEP.Graph.Node.StringExpr                as StringExpr
import           Luna.DEP.Graph.Port                           (Port)
import qualified Luna.DEP.Graph.Port                           as Port



instance Convert (Int, Int, Edge) Gen.Edge where
    encode (nodeSrc, nodeDst, Edge.Data portSrc portDst) =
        Gen.Edge (encodePJ nodeSrc) (encodePJ nodeDst) (encodeP portSrc) (encodeP portDst)
    decode (Gen.Edge mtnodeSrc mtnodeDst mtportSrc mtportDst) = do
        tnodeSrc <- mtnodeSrc <?> "Failed to decode Edge: 'srcNode' field is missing"
        tnodeDst <- mtnodeDst <?> "Failed to decode Edge: 'dstNode' field is missing"
        return (decodeP tnodeSrc, decodeP tnodeDst, Edge.Data (decodeP mtportSrc) (decodeP mtportDst))


instance Convert (Int, Node) Gen.Node where
    encode (nodeID, node) = tnode where
        tnodeID = encodePJ nodeID
        tnodeWithoutPos = case node of
            Node.Expr expr outputName _ -> Gen.Node GenNode.Expr tnodeID (encodeJ expr)
                                                                (encodePJ outputName)
            Node.Inputs  {}             -> Gen.Node GenNode.Inputs  tnodeID Nothing Nothing
            Node.Outputs {}             -> Gen.Node GenNode.Outputs tnodeID Nothing Nothing
        tnode = tnodeWithoutPos (Just $ node ^. Node.pos . _1) (Just $ node ^. Node.pos . _2)
    decode (Gen.Node tcls mtnodeID mtexpr mtoutputName mx my) = do
        nodeID <- decodeP <$> mtnodeID <?> "Failed to decode Node: 'id' field is missing"
        x <- mx <?> "Failed to decode Node: 'x' field is missing"
        y <- my <?> "Failed to decode Node: 'y' field is missing"
        node <- case tcls of
            GenNode.Expr -> do expr       <- decode  =<< mtexpr       <?> "Failed to decode Node: 'expr' field is missing"
                               outputName <- decodeP <$> mtoutputName <?> "Failed to decode Node: 'outputName' field is missing"
                               return $ Node.Expr expr outputName
            GenNode.Inputs  -> return Node.Inputs
            GenNode.Outputs -> return Node.Outputs
        return (nodeID, node (x, y ))


instance Convert Graph Gen.Graph where
    encode graph =
        Gen.Graph (encode $ Graph.labNodes graph) (encode $ Graph.labEdges graph)
    decode (Gen.Graph tnodes tedges) =
        Graph.mkGraph <$> decode tnodes <*> decode tedges


instance ConvertPure Port (Maybe Int32) where
    encodeP port = case port of
        Port.All   -> Nothing
        Port.Num n -> encodePJ n
    decodeP mtport = case mtport of
        Nothing -> Port.All
        Just tn -> Port.Num $ decodeP tn


instance Convert NodeExpr Gen.NodeExpr where
    encode (NodeExpr.ASTExpr    expr   ) = Gen.NodeExpr GenNodeExpr.ASTExpr Nothing $ encodeJ expr
    encode (NodeExpr.StringExpr strExpr) = Gen.NodeExpr GenNodeExpr.String  (encodePJ $ StringExpr.toString strExpr) Nothing
    decode (Gen.NodeExpr cls mstr mexpr) = case cls of
        GenNodeExpr.ASTExpr -> do
            expr <- mexpr <?> "Failed to decode NodeExpr: 'expr' field is missing"
            NodeExpr.ASTExpr <$> decode expr
        GenNodeExpr.String -> do
            str <- mstr <?> "Failed to decode NodeExpr: 'str' field is missing"
            return $ NodeExpr.StringExpr $ StringExpr.fromString $ decodeP str

