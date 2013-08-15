---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs where

import           Data.Int                                               
import           Data.Text.Lazy                                         (pack, unpack)
import qualified Data.HashMap.Strict                                  as HashMap
import qualified Data.Vector                                          as Vector

import qualified Defs_Types                                           as TDefs
import           Flowbox.Control.Error                                  
import           Flowbox.Luna.Network.Graph.Graph                       (Graph)
import           Flowbox.Luna.Network.Def.Edge                          (Edge(..))
import qualified Flowbox.Luna.Network.Def.Definition                  as Definition
import           Flowbox.Luna.Network.Def.Definition                    (Definition(..))
import qualified Flowbox.Luna.Network.Def.DefManager                  as DefManager
import           Flowbox.Luna.Network.Def.DefManager                    (DefManager)
import qualified Flowbox.Luna.Network.Graph.Graph                     as Graph
import           Flowbox.Luna.Network.Path.Import                       (Import(..))
import qualified Flowbox.Luna.Network.Path.Path                       as Path
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Attrs   ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph   ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Types   ()
import           Flowbox.Tools.Conversion                               


encodeLabNode :: (Definition.ID, Definition) -> (Int32, TDefs.Definition)
encodeLabNode node@(defID, _) = tnode where
    tnode = (itoi32 defID, encodeDef node)

    encodeDef d = td where
        (td, _) = encode d


decodeLabNode :: (Int32, TDefs.Definition) -> Either String (Definition.ID, Definition)
decodeLabNode (_, tdef) = decode (tdef, Graph.empty)


instance Convert (Int, Int, Edge) TDefs.DEdge where
    encode (asrc, adst, _) = tedge where
        tedge = TDefs.DEdge (Just $ itoi32 asrc) (Just $ itoi32 adst)
    decode (TDefs.DEdge mtsrc mtdst) = do 
        tsrc <- mtsrc <?> "Failed to decode Edge: `src` field is missing"
        tdst <- mtdst <?> "Failed to decode Edge: `dst` field is missing"
        return (i32toi tsrc, i32toi tdst, Edge) 


toDefsGraph :: DefManager -> TDefs.DefsGraph
toDefsGraph defManager = tdefGraph where
    labNodesList  = DefManager.labNodes defManager
    tdefs         = HashMap.fromList $ map (encodeLabNode) labNodesList

    labEdgesList  = DefManager.labEdges defManager
    tedges        = Vector.fromList $ map (encode) labEdgesList
    tdefGraph     = TDefs.DefsGraph (Just tdefs) (Just tedges)
     

instance Convert DefManager TDefs.DefManager where
    encode defManager   = tdefManager where
        labNodesList    = DefManager.labNodes defManager
        tdefGr          = map (encode) labNodesList
        (tdefs, graphs) = unzip tdefGr
        tdefsv          = Vector.fromList tdefs
        tgraphsv        = Vector.fromList $ map (encode) graphs
        aedges          = DefManager.labEdges defManager
        tedges          = Vector.fromList $ map (encode) aedges
        tdefManager     = TDefs.DefManager (Just tdefsv) (Just tgraphsv) (Just tedges)
    decode (TDefs.DefManager mtdefs mtgraphs mtedges) = do
        tdefsv   <- mtdefs   <?> "Failed to decode DefsGraph: `defs` field is missing"
        tgraphsv <- mtgraphs <?> "Failed to decode DefsGraph: `graphs` field is missing"
        tedges   <- mtedges  <?> "Failed to decode DefsGraph: `tedges` field is missing"
        let agraphs = convert $ map (decode) $ Vector.toList tgraphsv
        graphs <- agraphs
        let tdefs   = Vector.toList tdefsv
            tdefGr  = zip tdefs graphs
            anodes = convert $ map (decode) tdefGr
            aedges = convert $ map (decode) $ Vector.toList tedges
        nodes <- anodes
        edges <- aedges
        return $ DefManager.mkGraph nodes edges


instance Convert Import TDefs.Import where
    encode (Import apath aitems) = timport where
        mtpath   = Just $ Vector.fromList $ map (pack) $ Path.toList apath
        mtitems  = Just $ Vector.fromList $ map (pack) aitems
        timport  = TDefs.Import mtpath mtitems
    decode (TDefs.Import mtpath mtitems) = do
        tpath  <- mtpath  <?> "Failed to decode Import: `path` field is missing"
        titems <- mtitems <?> "Failed to decode Import: `items` field is missing"
        let
            apath  = Path.fromList $ map (unpack) $ Vector.toList tpath
            aitems = map (unpack) $ Vector.toList titems
        return $ Import apath aitems



instance Convert [Import] TDefs.Imports where
    encode aimports = Vector.fromList $ map (encode) aimports
    decode timports = aimports where
        timportsList = Vector.toList timports
        imports1 = map (decode :: TDefs.Import -> Either String Import) timportsList
        aimports = convert imports1


instance Convert (Int, Definition) (TDefs.Definition, Graph) where
    encode (defID, Definition acls agraph aimports aflags aattributes) = (tdef, agraph) where
        tcls        = Just $ encode acls
        timports    = Just $ encode aimports
        tflags      = Just $ encode aflags
        tattributes = Just $ encode aattributes
        tdefID      = Just $ itoi32 defID
        tdef = TDefs.Definition tcls timports tflags tattributes tdefID 
    decode (TDefs.Definition mtcls mtimports mtflags mtattributes mtdefID, agraph) = do 
        tcls        <- mtcls        <?> "Failed to decode Definition: `type` field is missing"
        timports    <- mtimports    <?> "Failed to decode Definition: `imports` field is missing"
        tflags      <- mtflags      <?> "Failed to decode Definition: `flags` field is missing"
        tattributes <- mtattributes <?> "Failed to decode Definition: `attributes` field is missing"
        tdefID      <- mtdefID      <?> "Failed to decode Definition: `defID` field is missing"
        acls        <- decode tcls
        aimports    <- decode timports
        aflags      <- decode tflags
        aattributes <- decode tattributes 
        let nodeDef = Definition acls agraph aimports aflags aattributes
            adefID = i32toi tdefID
        return (adefID, nodeDef)
