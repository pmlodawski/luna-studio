---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Luna.Tools.Serializer.Lib(
    storeLib
    --restoreLib
) where

import System.Directory
import System.IO
import Text.Regex.Posix

import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary

import qualified Defs_Types                  as TDefs
import qualified Graph_Types                 as TGraph
import qualified Luna.Project                as Project
import           Luna.Project                  (Project)
import qualified Luna.Network.Def.DefManager as DefManager
import           Luna.Network.Def.DefManager   (DefManager)
import qualified Luna.Network.Def.Definition    as Definition
import           Luna.Network.Def.Definition      (Definition)
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library)
import qualified Luna.System.UniPath         as UniPath
import           Luna.System.UniPath           (UniPath)
import qualified Luna.Type.Type              as Type
import           Luna.Type.Type                (Type(..))
import           Luna.Tools.Conversion
import qualified Luna.Tools.Conversion.Defs    ()
import qualified Luna.Tools.Conversion.Graph   ()
import qualified Luna.Tools.Serializer       as Serializer
import           Luna.Tools.Serializer          (Serializable(..))



nodeFileExtension :: String
nodeFileExtension = ".node"


graphFileExtension :: String
graphFileExtension = ".graph"


generate :: DefManager -> UniPath -> Definition.ID -> Definition -> [Serializable]
generate defManager upath defID def = sdef:sgraph:schildren where 
    children  = DefManager.suc defManager defID
    schildren = foldr (\child rest -> checkedGenerate defManager upath child ++ rest) [] children

    (tdef, graph) = encode (defID, def)
    tgraph = encode graph

    defFilename = UniPath.setExtension nodeFileExtension upath
    saveDef = (\h -> do 
        let protocol = BinaryProtocol h
        TDefs.write_Definition protocol tdef)

    sdef = File defFilename saveDef

    graphFilename = UniPath.setExtension graphFileExtension upath
    saveGraph = (\h -> do 
        let protocol = BinaryProtocol h
        TGraph.write_Graph protocol tgraph)

    sgraph = File graphFilename saveGraph



checkedGenerate :: DefManager -> UniPath -> Definition.ID -> [Serializable]
checkedGenerate defManager udirpath defID = s where
    s = case DefManager.lab defManager defID of
        Nothing -> error "Inconssistence in defManager: ID not found"
        Just def -> case Definition.cls def of 
                Module   aname     -> gen aname
                Class    aname _ _ -> gen aname
                Function aname _ _ -> gen aname
                _                 -> error "Inconssistent in defManager: Wrong type of a definition"
                where gen aname = generate defManager (UniPath.append aname udirpath) defID def



storeLib :: Project -> Library -> IO ()
storeLib project lib = do 
    let defManager = Project.defManager project
        rootPath = Library.path lib
        libRootDefID = Library.rootDefID lib

        defs = checkedGenerate defManager rootPath libRootDefID 

    Serializer.serializeMany defs

    return ()



---- mocked
--restoreNode :: DefManager -> UniPath -> IO DefManager 
--restoreNode defManager udirpath  -- =
--    | dirpath =~ nodeDefFilePattern = do putStrLn $ "file! " ++ dirpath
--                                         return defManager
--    | dirpath =~ folderFilePattern  = do putStrLn $ "folder " ++ dirpath 
--                                         contents <- getDirectoryContents dirpath
--                                         _ <- sequence $ map (\c -> restoreNode defManager $ UniPath.append c udirpath) contents
--                                         return defManager
--    | otherwise                     = do putStrLn $ "other " ++ dirpath 
--                                         return defManager
--    where dirpath = UniPath.toUnixString udirpath
--          nodeDefFilePattern = "[.]node$"
--          folderFilePattern  = "[A-Za-z0-9]+$"


--restoreLib :: Project -> Library -> IO Project
--restoreLib project lib = do 
--    let defManager = Project.defManager project
--        rootPath = Library.path lib

--    newDefManager <- restoreNode defManager rootPath

--    return $ project {Project.defManager = newDefManager}




