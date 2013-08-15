---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView where

import qualified Data.Vector                                               as Vector

import qualified Graphview_Types                                           as TGraphView
import           Flowbox.Batch.GraphView.EdgeView                            (EdgeView(..))
import           Flowbox.Batch.GraphView.GraphView                           (GraphView)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        
import           Flowbox.Tools.Conversion


instance Convert GraphView TGraphView.GraphView where
    encode agraph = TGraphView.GraphView n e where 
        (n, e) = encodeGraph agraph
    decode (TGraphView.GraphView mtnodes mtedges) = decodeGraph (mtnodes, mtedges)


instance Convert (Int, Int, EdgeView) TGraphView.EdgeView where
  encode (asrcNode, adstNode, EdgeView asrcPort adstPort) = let 
      tsrcNode = itoi32 asrcNode
      tdstNode = itoi32 adstNode
      tsrcPort = Vector.fromList $ map (itoi32) asrcPort
      tdstPort = itoi32 adstPort
    in TGraphView.EdgeView (Just tsrcNode) (Just tdstNode) (Just tsrcPort) (Just tdstPort)
  decode (TGraphView.EdgeView mtsrcNode mtdstNode mtsrcPort mtdstPort) = case mtsrcNode of
    Nothing                   -> Left "`srcNode` field is missing"
    Just tsrcNode             -> case mtdstNode of
        Nothing               -> Left "`dstNode` field is missing"
        Just tdstNode         -> case mtsrcPort of
            Nothing           -> Left "`srcPort` field is missing"
            Just tsrcPort     -> case mtdstPort of
                Nothing       -> Left "`srcPort` field is missing"
                Just tdstPort -> Right (asrcNode, adstNode, EdgeView asrcPort adstPort) where
                    asrcNode = i32toi tsrcNode
                    adstNode = i32toi tdstNode
                    asrcPort = map (i32toi) $ Vector.toList tsrcPort
                    adstPort = i32toi tdstPort
