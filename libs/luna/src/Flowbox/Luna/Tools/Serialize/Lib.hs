---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Luna.Tools.Serialize.Lib(
    storeLibrary,
    restoreLibrary,
) where

import           System.Directory                                            
import           System.IO                                                   
import           Text.Regex.Posix                                            

import           Thrift                                                      
import           Thrift.Transport.Handle                                     
import           Thrift.Protocol                                             
import           Thrift.Protocol.Binary                                      

import qualified Defs_Types                                                as TDefs
import qualified Graph_Types                                               as TGraph
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.System.UniPath                                      (UniPath)
import qualified Flowbox.Luna.Network.Def.DefManager                       as DefManager
import           Flowbox.Luna.Network.Def.DefManager                         (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition                       as Definition
import           Flowbox.Luna.Network.Def.Definition                         (Definition)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import           Flowbox.Luna.Lib.Library                                    (Library)
import qualified Flowbox.Luna.Type.Type                                    as Type
import           Flowbox.Luna.Type.Type                                      (Type(..))
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion   
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        ()
import qualified Flowbox.System.IO.Serializer                              as Serializer
import           Flowbox.System.IO.Serializer                                (Serializable(..))



nodeFileExtension :: String
nodeFileExtension = ".node"


graphFileExtension :: String
graphFileExtension = ".graph"


save :: Handle -> (BinaryProtocol Handle -> object -> IO()) -> object -> IO()
save h method object = do 
    let protocol = BinaryProtocol h
    method protocol object


generate :: DefManager -> UniPath -> Definition.ID -> Definition -> [Serializable]
generate defManager upath defID def = sdef:sgraph:schildren where 
    children  = DefManager.suc defManager defID
    schildren = foldr (\child rest -> checkedGenerate defManager upath child ++ rest) [] children

    (tdef, graph) = encode (defID, def)
    tgraph        = encode graph

    defFilename   = UniPath.setExtension nodeFileExtension upath
    saveDef h     = save h TDefs.write_Definition tdef

    sdef          = Serializable defFilename saveDef

    graphFilename = UniPath.setExtension graphFileExtension upath
    saveGraph h   = save h TGraph.write_Graph tgraph

    sgraph        = Serializable graphFilename saveGraph


checkedGenerate :: DefManager -> UniPath -> Definition.ID -> [Serializable]
checkedGenerate defManager udirpath defID = s where
    s = case DefManager.lab defManager defID of
        Nothing -> error "Inconssistence in defManager: ID not found"
        Just def -> case Definition.cls def of 
                Module   aname     -> gen aname
                Class    aname _ _ -> gen aname
                Function aname _ _ -> gen aname
                _                  -> error "Inconssistent in defManager: Wrong type of a definition"
                where gen aname = generate defManager (UniPath.append aname udirpath) defID def


storeLibrary :: Library -> IO ()
storeLibrary lib = do 
    let defManager   = Library.defs lib
        rootPath     = Library.path lib
        libRootDefID = Library.rootDefID

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

-- mocked
restoreLibrary :: UniPath -> IO Library
restoreLibrary path = do 
    putStrLn "restoreLibrary - NOT IMPLEMENTED"
    return $ Library.make "restored" path




