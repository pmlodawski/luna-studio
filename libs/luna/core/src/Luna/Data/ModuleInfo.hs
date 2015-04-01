
module Luna.Data.ModuleInfo where

import           Control.Monad            ((>=>), (<=<), liftM, foldM)
import           Data.Binary             
import           Data.Either              (lefts, rights)
import           Data.List                (find, filter)
import           Data.Maybe               (fromMaybe, fromJust)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.IntMap              as IntMap
import           Data.Text.Internal.Lazy  (Text)
import           Data.Text.Lazy           as T
import           System.Environment       (lookupEnv)
import qualified System.Directory         as Dir
import           System.FilePath          (joinPath, (</>))

import qualified Luna.Data.StructInfo     as SI
import           Luna.Data.StructInfo     (StructInfo, Scope, OriginInfo)
import           Luna.Syntax.AST          (ID)
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name         (TName(TName), TNameP)
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import           Luna.Syntax.Name.Pattern (NamePatDesc, SegmentDesc)

import           Flowbox.Data.MapForest   (Node)
import qualified Flowbox.Data.MapForest   as MF
import           Flowbox.System.UniPath   (UniPath, PathItem)
import           Data.Either.Combinators  (mapRight)
import           Flowbox.Prelude




type Name = String


data ImportError = NotFoundError { path   :: Path }
                 | AmbRefError   { symbol :: NamePath, modules :: [Path] }
                 deriving (Generic, Eq, Show, Read)


-- stores the information about a module, needed while importing
-- and resolving names. Checking whether a file needs recompilation is done based on the file  edit dates
data ModuleInfo = ModuleInfo {
                     _name     :: Path,
                     _symTable :: Map NamePath ID,
                     _imports  :: [Path],
                     _strInfo  :: StructInfo,  -- [?] Namespace here?
                     _errors   :: [ImportError]
                  } deriving (Generic, Eq, Show, Read)

makeLenses ''ModuleInfo



-- checks whether a given symbol is anywhere in the imported list
-- returns the list of ALL matches (non-singleton list means some kind of conflict)
getSymbolOriginsAux :: NamePath -> [ModuleInfo] -> [Path]
getSymbolOriginsAux symbol infos = Flowbox.Prelude.map (^. name) results
    where results = Flowbox.Prelude.filter (nameExists symbol) infos

-- [TODO] update to account for the Either version of getModuleInfo(s)
-- this is the main version, as you only have to pass it your currently parsed module
getSymbolOrigins :: NamePath -> ModuleInfo -> IO [Path]
getSymbolOrigins symbol mInfo = do
    res <- getModuleInfos (mInfo ^. imports)
    return $ fmap (_name) $ rights res

-- given a list of paths, lookups all the necessary ModuleInfo structs
getModuleInfos :: [Path] -> IO [Either ImportError ModuleInfo]
getModuleInfos paths = mapM getModuleInfo paths

getStructInfosMap :: [Path] -> IO (Map Path (Either ImportError StructInfo))
getStructInfosMap paths = foldM fun Map.empty paths

fun :: Map Path (Either ImportError StructInfo) -> Path -> IO (Map Path (Either ImportError StructInfo))
fun currentMap path = do
    mi <- getModuleInfo path
    let si = mapRight _strInfo mi
    return $ Map.insert path si currentMap


getModuleInfo :: Path -> IO (Either ImportError ModuleInfo)
--getModuleInfo = (return . fromJust <=< readModInfoFromFile)
getModuleInfo path = do
    result <- readModInfoFromFile path
    return $ case result of
        Just modInfo -> Right modInfo
        Nothing      -> Left (NotFoundError path)


regError :: ImportError -> ModuleInfo -> ModuleInfo
regError err = errors %~ (err:)

-------------------------------------------------------------------------------------
-- wrappers for structInfo functions
-------------------------------------------------------------------------------------
regOrigin :: ID -> ID -> NamePath -> Path -> ModuleInfo -> ModuleInfo
regOrigin id pid name path = strInfo %~ SI.regOrigin id pid name path


regOrphan :: ID -> SI.Error -> ModuleInfo -> ModuleInfo
regOrphan id err = strInfo %~ SI.regOrphan id err


regParent id pid = strInfo %~ SI.regParent id pid

regArgPat id argPat = strInfo %~ SI.regArgPat id argPat

--------------------------------------------------------------------
-- simple utility functions for lookups and checks
--------------------------------------------------------------------

nameExists :: NamePath -> ModuleInfo -> Bool
nameExists name mInfo = Map.member name (mInfo ^. symTable)



getSymbolId :: NamePath -> ModuleInfo -> Maybe ID
getSymbolId name mInfo = Map.lookup name (mInfo ^. symTable)



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
    let fullPath = modPathToString path ++ "." ++ liFileSuffix
    liPath <- liDirectory
    f      <- Dir.findFile [liPath] fullPath
    return $ case f of
        Just p  -> True
        Nothing -> False


modPathToString :: Path -> String
modPathToString path = joinPath $ Flowbox.Prelude.map toString path


-- the difference between this one and modPathToString is that
-- this returns the directory of the module, not the module name itself
modPathToDirString :: Path -> FilePath
modPathToDirString path = joinPath . Flowbox.Prelude.init $ Flowbox.Prelude.map toString path

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
    let modDir = liDir </> (modPathToDirString $ modInfo ^. name)
    Dir.createDirectoryIfMissing True modDir
    let fPath = liDir </> (modPathToString $ modInfo ^. name) ++ (liFileSuffix)
    -- serialize with Data.Binry:
    encodeFile fPath modInfo


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
        then do 
            liDir <- liDirectory
            let modPath = liDir </> ((modPathToString path) ++ "." ++ liFileSuffix)
            putStrLn $ "[[[[[[MODPATH: " ++ modPath ++ " ]]]]]]"
            fmap Just $ decodeFile modPath
        else return Nothing
        
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
instance Binary ImportError

instance Binary (Node Text OriginInfo)

-- perhaps this could be done without going through string
instance Binary Text
    where put txt = put $ T.unpack txt
          get     = do t <- get :: Get String
                       return $ T.pack t


instance Monoid ModuleInfo where
    mempty      = ModuleInfo mempty mempty mempty mempty mempty
    mappend a b = ModuleInfo (mappend (a ^. name)     (b ^. name))
                             (mappend (a ^. symTable) (b ^. symTable))
                             (mappend (a ^. imports)  (b ^. imports))
                             (mappend (a ^. strInfo)  (b ^. strInfo))
                             (mappend (a ^. errors)   (b ^. errors))

