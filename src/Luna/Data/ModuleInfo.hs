
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
import qualified Data.Text.Lazy           as T
import           System.Environment       (lookupEnv)
import qualified System.Directory         as Dir
import           System.FilePath          (joinPath, (</>))

import qualified Luna.Data.StructInfo     as SI
import           Luna.Data.StructInfo     (StructInfo, Scope, OriginInfo)
import           Luna.Syntax.AST          (ID)
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name         (TName(TName), TNameP)
import           Luna.Syntax.Name.Path    (NamePath, QualPath(QualPath))
import qualified Luna.Syntax.Name.Path    as NP
import           Luna.Syntax.Name.Pattern (NamePatDesc, SegmentDesc)

import           Flowbox.Data.MapForest   (Node)
import qualified Flowbox.Data.MapForest   as MF
import           Flowbox.System.UniPath   (UniPath, PathItem)
import           Data.Either.Combinators  (mapRight)
import           Flowbox.Prelude




type Name = String


data ImportError = NotFoundError { path   :: QualPath }
                 | AmbRefError   { symbol :: NamePath, modules :: [QualPath] }
                 deriving (Generic, Eq, Show, Ord, Read)


-- stores the information about a module, needed while importing
-- and resolving names. Checking whether a file needs recompilation is done based on the file  edit dates
data ModuleInfo = ModuleInfo {
                     _name     :: QualPath,
--                     _symTable :: Map NamePath ID,
                     _imports  :: [QualPath],
                     _strInfo  :: StructInfo,  -- [?] Namespace here?
                     _errors   :: [ImportError]
                  } deriving (Generic, Eq, Show, Read)

makeLenses ''ModuleInfo


-- given a list of paths, lookups all the necessary ModuleInfo structs
getModuleInfos :: [QualPath] -> IO [Either ImportError ModuleInfo]
getModuleInfos paths = mapM getModuleInfo paths

getModuleInfo :: QualPath -> IO (Either ImportError ModuleInfo)
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
regOrigin :: ID -> SI.OriginInfo -> ModuleInfo -> ModuleInfo
regOrigin id origin = strInfo %~ SI.regOrigin id origin


regOrphan :: ID -> SI.Error -> ModuleInfo -> ModuleInfo
regOrphan id err = strInfo %~ SI.regOrphan id err


regParent id pid = strInfo %~ SI.regParent id pid

regArgPat id argPat = strInfo %~ SI.regArgPat id argPat

--------------------------------------------------------------------
-- simple utility functions for lookups and checks
--------------------------------------------------------------------

--nameExists :: NamePath -> ModuleInfo -> Bool
--nameExists name mInfo = Map.member name (mInfo ^. symTable)



--getSymbolId :: NamePath -> ModuleInfo -> Maybe ID
--getSymbolId name mInfo = Map.lookup name (mInfo ^. symTable)



-- checks if the module exists (but not if it's parsed)
moduleExists :: QualPath -> IO Bool
moduleExists path = do
    let fullPath = modPathToString path ++ lunaFileSuffix
    f <- Dir.findFile ["."] fullPath 
    return $ case f of
        Just p  -> True
        Nothing -> False



-- checks if module is already parsed (i.e. the ModuleInfo is present)
moduleIsParsed :: QualPath -> IO Bool
moduleIsParsed path = do
    let fullPath = modPathToString path ++ liFileSuffix
        liPath   = liDirectory
    f      <- Dir.findFile [liPath] fullPath
    return $ case f of
        Just p  -> True
        Nothing -> False


modPathToString :: QualPath -> String
modPathToString qp@(QualPath _ n) = (modPathToDirString qp) </> (T.unpack n)


-- the difference between this one and modPathToString is that
-- this returns the directory of the module, not the module name itself
modPathToDirString :: QualPath -> FilePath
modPathToDirString (QualPath ns _) = joinPath $ map T.unpack ns


modName :: QualPath -> String
modName qp = T.unpack $ NP._name qp 


pathToQualPath :: Path -> QualPath
pathToQualPath path = QualPath ns n
    where list = map toText path
          n    = last list
          ns   = init list

qualPathToPath :: QualPath -> Path
qualPathToPath (QualPath ns n) = segs ++ [seg]
    where segs = (map makeTNameP ns)
          seg  = (fromText n) :: TNameP
          makeTNameP = (\x -> fromText x) :: T.Text -> TNameP

--------------------------------------------------------------------------
-- ModuleInfo serialization utils
--------------------------------------------------------------------------
lunaFileSuffix :: FilePath
lunaFileSuffix = ".luna"

liFileSuffix :: FilePath
liFileSuffix = ".li"


liDirectory :: FilePath
liDirectory = "modinfo"



-- does the main serialization:
writeModInfoToFile :: ModuleInfo -> IO ()
writeModInfoToFile modInfo = do
    -- if the directory doesn't exist, create one:
    let modDir = liDirectory </> (modPathToDirString $ modInfo ^. name)
    Dir.createDirectoryIfMissing True modDir
    let mName = modName $ _name modInfo
    let fPath = liDirectory </> mName ++ liFileSuffix
    -- serialize with Data.Binry:
    encodeFile fPath modInfo



-- deserialization:
readModInfoFromFile :: QualPath -> IO (Maybe ModuleInfo)
readModInfoFromFile path = do
    isParsed <- moduleIsParsed path
    if isParsed
        then do 
            let modPath = liDirectory </> ((modName path) ++ liFileSuffix)
            fmap Just $ decodeFile modPath
        else return Nothing


-----------------------------------------------------------------------------
-- instance declarations for serialization
-- they can be moved to a separate module, save ModuleInfo (that would cause cycle imports
-----------------------------------------------------------------------------

instance Binary ModuleInfo
instance Binary StructInfo
instance Binary OriginInfo
instance Binary Scope
instance Binary NamePatDesc
instance Binary SegmentDesc
instance Binary SI.Error
instance Binary ImportError
instance Binary (Node Text OriginInfo)


instance Monoid ModuleInfo where
    mempty      = ModuleInfo mempty mempty mempty mempty --mempty
    mappend a b = ModuleInfo (mappend (a ^. name)     (b ^. name))
                             --(mappend (a ^. symTable) (b ^. symTable))
                             (mappend (a ^. imports)  (b ^. imports))
                             (mappend (a ^. strInfo)  (b ^. strInfo))
                             (mappend (a ^. errors)   (b ^. errors))


instance Monoid QualPath where
    mempty      = QualPath [] mempty
    mappend a b = b

