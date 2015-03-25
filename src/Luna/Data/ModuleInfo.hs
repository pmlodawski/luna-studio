
module Luna.Data.ModuleInfo where

import           Data.Binary             
import           Data.Maybe               (fromMaybe)
import           Data.Map                 (Map)
import qualified Data.Map         as Map
import           Data.Text.Internal.Lazy  (Text)
import           Data.Text.Lazy   as T
import           System.Environment       (lookupEnv)
import qualified System.Directory as Dir
import           System.FilePath          (joinPath, (</>))

import qualified Luna.Data.StructInfo as SI
import           Luna.Data.StructInfo     (StructInfo, Scope, OriginInfo)
import           Luna.Syntax.AST          (ID)
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name         (TName(TName), TNameP)
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import           Luna.Syntax.Name.Pattern (NamePatDesc, SegmentDesc)

import           Flowbox.Data.MapForest   (Node)
import           Flowbox.System.UniPath   (UniPath, PathItem)
import           Flowbox.Prelude



type Name = String

-- stores the information about a module, needed while importing
-- and resolving names. Checking whether a file needs recompilation is done based on the file  edit dates
data ModuleInfo = ModuleInfo {
                     _name     :: Path,       -- [?] for now this is Path, but may be subject to change
                     _path     :: UniPath,    -- [?] is this really necessary
                     _strInfo  :: StructInfo  -- [?] Namespace here?
                  } deriving (Generic, Eq, Show, Read)

makeLenses ''ModuleInfo


--------------------------------------------------------------------
-- simple utility functions for lookups and checks
--------------------------------------------------------------------

nameExists :: NamePath -> ModuleInfo -> Bool
nameExists name mInfo = Map.member name (mInfo^.strInfo^.(SI.symTable))



getSymbolId :: NamePath -> ModuleInfo -> Maybe ID
getSymbolId name mInfo = Map.lookup name (mInfo^.strInfo^.(SI.symTable))



-- checks if the module exists (but not if it's parsed)
moduleExists :: Path -> IO Bool
moduleExists path = do
    let fullPath = modPathToString path ++ ".luna"
    f <- Dir.findFile ["."] fullPath 
    return $ case f of
        Just p  -> True
        Nothing -> False



-- checks if module is already parsed (i.e. the ModuleInfo is present)
moduleIsParsed :: Path -> IO Bool
moduleIsParsed path = do
    let fullPath = modPathToString path ++ liFileSuffix
    liPath <- liDirectory
    f      <- Dir.findFile [liPath] fullPath
    return $ case f of
        Just p  -> True
        Nothing -> False



modPathToString :: Path -> String
modPathToString path = joinPath $ Flowbox.Prelude.map getFromTName path
    where getFromTName (TName np) = toString np



-- the difference between this one and modPathToString is that
-- this returns the directory of the module, not the module name itself
modPathToDirString :: Path -> FilePath
modPathToDirString path = joinPath . Flowbox.Prelude.init $ Flowbox.Prelude.map getFromTName path
    where getFromTName (TName np) = toString np

--------------------------------------------------------------------------
-- ModuleInfo serialization utils
--------------------------------------------------------------------------
liFileSuffix :: FilePath
liFileSuffix = "li"



liDirectory :: IO FilePath
liDirectory = do
    r <- lookupEnv "LUNAROOT"
    let root = fromMaybe "." r
    return $ root </> "modinfo"



-- does the main serialization:
writeModInfoToFile :: ModuleInfo -> IO ()
writeModInfoToFile modInfo = do
    -- if the directory doesn't exist, create one:
    liDir <- liDirectory
    let modDir = liDir </> (modPathToDirString $ modInfo^.name)
    Dir.createDirectoryIfMissing True modDir
    -- serialize with Data.Binry:
    encodeFile modDir modInfo


-- serialization of only StructInfo:
writeStructInfoToFile :: String -> StructInfo -> IO ()
writeStructInfoToFile name sInfo = do
    liDir <- liDirectory
    let p = liDir </> name
    Dir.createDirectoryIfMissing True liDir
    encodeFile p sInfo


-- deserialization:
readModInfoFromFile :: Path -> IO (Maybe ModuleInfo)
readModInfoFromFile path = do
    isParsed <- moduleIsParsed path
    if isParsed
        then return Nothing
        else do
            liDir <- liDirectory
            let modPath = liDir </> ((modPathToString path) ++ liFileSuffix)
            fmap Just $ decodeFile modPath
        
-- deserialization of StructInfo only:
readStructInfoFromFile :: String -> IO (Maybe StructInfo)
readStructInfoFromFile name = do
    liDir <- liDirectory
    let fPath = liDir </> name
    exists <- Dir.doesFileExist fPath
    case exists of
        False -> return Nothing
        True  -> fmap Just (decodeFile $ fPath)

        
-----------------------------------------------------------------------------
-- instance declarations for serialization
-- they can be moved to a separate module, save ModuleInfo (that would cause cycle imports
-----------------------------------------------------------------------------

instance Binary ModuleInfo
instance Binary StructInfo
instance Binary OriginInfo
instance Binary Scope
instance Binary QualPath
instance Binary NamePath
instance Binary PathItem
instance Binary NamePatDesc
instance Binary SegmentDesc
instance Binary (TName NamePath)
instance Binary SI.Error

instance Binary (Node Text OriginInfo)

-- perhaps this could be done without going through string
instance Binary Text
    where put txt = put $ T.unpack txt
          get     = do t <- get :: Get String
                       return $ T.pack t


