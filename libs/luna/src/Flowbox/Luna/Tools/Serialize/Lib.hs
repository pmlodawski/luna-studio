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

import           System.IO                                              

import           Thrift.Protocol.Binary                                 
import           Thrift.Transport.Handle                                ()

import qualified Defs_Types                                           as TDefs
import qualified Libs_Types                                           as TLibs
import qualified Flowbox.Luna.Lib.Library                             as Library
import           Flowbox.Luna.Lib.Library                               (Library)
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs    ()
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph   ()
import qualified Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Libs    ()
import qualified Flowbox.System.IO.Serializer                         as Serializer
import           Flowbox.System.IO.Serializer                           (Serializable(..), Deserializable(..))
import           Flowbox.System.UniPath                                 (UniPath)
import           Flowbox.Tools.Conversion                               



saveLib :: Library -> Handle -> IO ()
saveLib library h = do 
    let (tlibrary, defManager) = encode (-1, library)
        tdefManager            = encode defManager
        protocol = BinaryProtocol h

    TLibs.write_Library    protocol tlibrary
    TDefs.write_DefManager protocol tdefManager


getLib :: Handle -> IO Library
getLib h = do 
    let protocol = BinaryProtocol h
    tlibrary    <- TLibs.read_Library    protocol
    tdefManager <- TDefs.read_DefManager protocol
    case decode tdefManager of 
        Left msg               -> error msg
        Right adefManager      -> case decode (tlibrary, adefManager) of
            Left m             -> error m
            Right (_, library) -> return library


storeLibrary :: Library -> IO ()
storeLibrary lib = do 
    let 
        libpath      = Library.path lib
        slib  = Serializable libpath (saveLib lib)

    Serializer.serialize slib


restoreLibrary :: UniPath -> IO Library
restoreLibrary lpath = do
    let dlib = Deserializable lpath getLib

    Serializer.deserialize dlib


--- [PM] OLD VERSION BELOW ----

--nodeFileExtension :: String
--nodeFileExtension = ".node"


--graphFileExtension :: String
--graphFileExtension = ".graph"


--libConfigFile :: String
--libConfigFile = "lib.conf"


--thriftSave :: (BinaryProtocol Handle -> object -> IO()) -> object -> Handle -> IO()
--thriftSave write object h = do 
--    let protocol = BinaryProtocol h
--    write protocol object


--thriftLoad :: (BinaryProtocol Handle -> IO tobject) -> (tobject -> Either String object) -> Handle -> IO object
--thriftLoad read convert h = do
--    let protocol = BinaryProtocol h
--    tobject <- read protocol
--    case convert tobject of
--        Left msg -> error msg
--        Right ob -> return ob


--generate :: DefManager -> UniPath -> Definition.ID -> Definition -> [Serializable]
--generate defManager upath defID def = sdef:sgraph:schildren where 
--    children  = DefManager.suc defManager defID
--    schildren = foldr (\child rest -> checkedGenerate defManager upath child ++ rest) [] children

--    (tdef, graph) = encode (defID, def)
--    tgraph        = encode graph

--    defFilename   = UniPath.setExtension nodeFileExtension upath
--    saveDef       = thriftSave TDefs.write_Definition tdef

--    sdef          = Serializable defFilename saveDef

--    graphFilename = UniPath.setExtension graphFileExtension upath
--    saveGraph     = thriftSave TGraph.write_Graph tgraph

--    sgraph        = Serializable graphFilename saveGraph


--checkedGenerate :: DefManager -> UniPath -> Definition.ID -> [Serializable]
--checkedGenerate defManager udirpath defID = s where
--    s = case DefManager.lab defManager defID of
--        Nothing -> error "Inconssistence in defManager: ID not found"
--        Just def -> case Definition.cls def of 
--                Module   aname     -> gen aname
--                Class    aname _ _ -> gen aname
--                Function aname _ _ -> gen aname
--                _                  -> error "Inconssistent in defManager: Wrong type of a definition"
--                where gen aname = generate defManager (UniPath.append aname udirpath) defID def


--prepareLibrary :: Library -> Serializable
--prepareLibrary lib = slib where
--    lpath  = UniPath.append libConfigFile $ Library.path lib
--    tlib   = fst $ encode (-1::Library.ID, lib)

--    savelib = thriftSave TLibs.write_Library tlib

--    slib = Serializable lpath savelib


--storeLibrary_old :: Library -> IO ()
--storeLibrary_old lib = do 
--    let defManager   = Library.defs lib
--        rootPath     = Library.path lib
--        libRootDefID = Library.rootDefID

--        defs = checkedGenerate defManager rootPath libRootDefID 
--        slib  = prepareLibrary lib

--    Serializer.serialize slib
--    Serializer.serializeMany defs


--getDefinition :: UniPath -> IO Definition
--getDefinition upath = do 
--    let 
--        convert :: TDefs.Definition -> Either String Definition
--        convert tdef = do 
--            (_, def) <- decode (tdef, Graph.empty)
--            return def

--        ddef :: Deserializable Definition
--        ddef = Deserializable upath (thriftLoad TDefs.read_Definition convert) where

--    Serializer.deserialize ddef


--restoreDefs :: UniPath -> IO DefManager 
--restoreDefs upath =
--    restoreDefsContinue upath (DefManager.empty) Nothing


---- TODO [PM] THIS METHOD DOES NOT WOR AT ALL
--restoreDefsContinue :: UniPath -> DefManager -> Maybe Definition.ID -> IO DefManager
--restoreDefsContinue upath defManager mparentID -- =
--    | apath =~ defFilePattern  = do 
--        putStrLn $ "file! " ++ apath
--        def <- getDefinition upath
--        case mparentID of 
--            Nothing       -> do let (newDefManager, defID) = DefManager.insNewNode def defManager
--                                return newDefManager
--            Just parentID -> do let (newDefManager, defID) = DefManager.addNewToParent (parentID, def) defManager
--                                return newDefManager
--    | apath =~ folderPattern   = do isDir <- doesDirectoryExist apath
--                                    case isDir of 
--                                        False -> handleOther
--                                        True -> do putStrLn $ "folder " ++ apath 
--                                                   contents <- getDirectoryContents apath
--                                                   _ <- sequence $ map (\c -> restoreDefs $ UniPath.append c upath) contents
--                                                   return defManager
--    | otherwise                     = handleOther
--    where defManager     = DefManager.empty
--          apath          = UniPath.toUnixString upath
--          defFilePattern = "[.]node$"
--          folderPattern  = "[A-Za-z0-9]+$"

--          handleOther = do putStrLn "other"
--                           return defManager


--getLibrary :: UniPath -> IO Library
--getLibrary upath = do 
--    let 
--        convert :: TLibs.Library -> Either String Library
--        convert t = case decode (t, DefManager.empty) :: Either String (Library.ID, Library) of
--            Left m       -> Left m
--            Right (_, a) -> Right a

--        dlib :: Deserializable Library
--        dlib = Deserializable upath $ thriftLoad TLibs.read_Library convert

--    Serializer.deserialize dlib





---- mocked
--restoreLibrary_old :: UniPath -> IO Library
--restoreLibrary_old lpath = do
--    library <- getLibrary $ UniPath.append libConfigFile lpath
--    defs <- restoreDefs lpath
--    return library{Library.defs = defs}
