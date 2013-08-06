---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Luna.Tools.Serializer(
storeLib,
restoreLib
) where

import System.Directory
import System.IO

import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary

import qualified Defs_Types                  as TDefs
import qualified Graph_Types                 as TGraph
import qualified Luna.Core                   as Core
import           Luna.Core                     (Core)
import qualified Luna.Network.Def.DefManager as DefManager
import           Luna.Network.Def.DefManager   (DefManager)
import qualified Luna.Network.Def.NodeDef    as NodeDef
import           Luna.Network.Def.NodeDef      (NodeDef)
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library)
import qualified Luna.System.UniPath         as UniPath
import           Luna.System.UniPath           (UniPath)
import qualified Luna.Type.Type              as Type
import           Luna.Type.Type                (Type(..))
import           Luna.Tools.Conversion
import qualified Luna.Tools.Conversion.Defs    ()
import qualified Luna.Tools.Conversion.Graph   ()


nodeFIleExtension :: String
nodeFIleExtension = ".node"


storeNode :: DefManager -> UniPath -> String -> NodeDef.ID -> NodeDef -> IO()
storeNode defManager udirpath nodeDefName nodeDefID nodeDef = do
    let dirpath  = UniPath.toUnixString udirpath
        ubasePath = UniPath.append nodeDefName udirpath
        filePath = (UniPath.toUnixString ubasePath) ++ nodeFIleExtension
    
    createDirectoryIfMissing True dirpath

    withFile filePath WriteMode (\h -> do
        let protocol = BinaryProtocol h
            (tnodeDef, graph) = encode (nodeDefID, nodeDef)
            tgraph = encode graph

        TDefs.write_NodeDef protocol tnodeDef
        TGraph.write_Graph  protocol tgraph)

    _ <- sequence $ map (getNameNStoreNode defManager ubasePath) $ DefManager.suc defManager nodeDefID

    --- DEBUG [
    --withFile filePath ReadMode (\h -> do
    --    let protocol = BinaryProtocol h
    --    d <- TDefs.read_NodeDef protocol 
    --    print d
    --    print "==========="
    --    w <- TGraph.read_Graph protocol
    --    print w)
    --- ] DEBUG

    return ()


getNameNStoreNode :: DefManager -> UniPath -> NodeDef.ID -> IO()
getNameNStoreNode defManager udirpath nodeDefID = do
    case DefManager.lab defManager nodeDefID of
        Nothing -> error "Inconssistence in defManager: ID not found"
        Just nodeDef -> do
            case NodeDef.cls nodeDef of 
                Module   name     -> storeNode defManager udirpath name nodeDefID nodeDef
                Class    name _   -> storeNode defManager udirpath name nodeDefID nodeDef
                Function name _ _ -> storeNode defManager udirpath name nodeDefID nodeDef
                _                 -> error "Inconssistent in defManager: Wrong type of a definition"



storeLib :: Core -> Library -> IO ()
storeLib core lib = do 
    let defManager = Core.defManager core
        rootPath = Library.path lib
        libRootNodeID = Library.rootNodeDefID lib

    getNameNStoreNode defManager rootPath libRootNodeID
    return ()

-- mocked
restoreNode :: DefManager -> UniPath -> IO DefManager 
restoreNode defManager udirpath = do
    let dirpath = UniPath.toUnixString udirpath
    contents <- getDirectoryContents dirpath
    print contents
    return defManager


restoreLib :: Core -> Library -> IO Core
restoreLib core lib = do 
    let defManager = Core.defManager core
        rootPath = Library.path lib

    newDefManager <- restoreNode defManager rootPath

    return $ core {Core.defManager = newDefManager}


