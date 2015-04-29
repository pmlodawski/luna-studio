---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------
module Luna.Data.ImportInfo where

import           Control.Monad.RWS        (RWST)
import qualified Control.Monad.RWS        as RWST
import qualified Data.Map                 as Map
import           Data.Map                 (Map)
--CR[PM->TD] : new line between std imports and Flowbox imports
import qualified Flowbox.Data.MapForest   as MapForest
import           Luna.Data.StructInfo     (StructInfo, OriginInfo(OriginInfo), Scope)
import qualified Luna.Data.StructInfo     as SI
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name.Path    (NamePath, QualPath(QualPath))
import qualified Luna.Syntax.Name.Path    as NP
import           Luna.Data.ModuleInfo     (ImportError, qualPathToPath)
import           Flowbox.Prelude
--CR[PM->TD] : 3 new lines between imports and code

type ID = Int

data Tag = Vars | Types deriving (Show, Eq)


data Import = Import {
--CR[PM->TD] : commas should be on the left and following braces, see other files
    _impPath  :: QualPath,
    _wildcard :: Bool,
    _hiding   :: [NamePath],
    _targets  :: [NamePath],
    _rename   :: Maybe NamePath
} deriving (Generic, Show, Eq, Read)


data ImportInfo = ImportInfo {
--CR[PM->TD] : commas should be on the left and following braces, see other files
    _path        :: QualPath,       -- move to Namespace (?)
    _imports     :: [Import],
    _structInfos :: Map QualPath StructInfo,
    _symTable    :: Map NamePath [OriginInfo],
    _typeTable   :: Map NamePath [OriginInfo],
    _errors      :: [ImportError]
} deriving (Generic, Show, Eq, Read)

makeLenses ''ImportInfo

----------------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------------
class ImportInfoMonad m where
    get :: m ImportInfo
    put :: ImportInfo -> m ()

--CR[PM->TD] : too many newlines (should be 2)


----------------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------------
setPath :: QualPath -> ImportInfo -> ImportInfo
setPath p = path .~ p

--CR[PM->TD] : too many newlines (should be 2)


getPath :: ImportInfo -> Path
--CR[PM->TD] : use lens
getPath info = qualPathToPath $ _path info


getImportPaths :: ImportInfo -> [QualPath]
getImportPaths info = map _impPath (_imports info)

-- Creates the mapping between symbol names (NamePath) and their origins (OriginInfo)
createSymTable :: ImportInfo -> ImportInfo
createSymTable = addMNameToSymTable . addMNameToTypeTable . createSymTableVars . createSymTableTypes

-- TODO[PMo] add every module name to both type- and symTable
addMNameToSymTable :: ImportInfo -> ImportInfo
--CR[PM->TD] : use lens
addMNameToSymTable info = info & (symTable %~ (Map.unionWith (++) (createNameMap $ _imports info)))

addMNameToTypeTable :: ImportInfo -> ImportInfo
--CR[PM->TD] : use lens
addMNameToTypeTable info = info & (typeTable %~ (Map.unionWith (++) (createNameMap $ _imports info)))

createNameMap :: [Import] -> Map NamePath [OriginInfo]
createNameMap imps = Map.fromList tuples
--CR[PM->TD] : use lens
    where tuples = fmap (\imp -> (moduleObjectName (_impPath imp), [OriginInfo (_impPath imp) 0])) imps -- TODO check if it's really 0


createSymTableVars :: ImportInfo -> ImportInfo
createSymTableVars info = info & (symTable .~ (combineScopes Vars info))

createSymTableTypes :: ImportInfo -> ImportInfo
createSymTableTypes info = info & (typeTable .~ (combineScopes Types info))

--CR[PM->TD] : too many newlines (should be 2)


regError :: ImportError -> ImportInfo -> ImportInfo
regError err = errors %~ (err:)

--CR[PM->TD] : too many newlines (should be 2)


-- combines the top-level scopes of all the imported modules
-- results in a map from NamePath to [OriginInfo] -- if a given
-- symbol appears in more than one module, the list isn't a singleton
combineScopes :: Tag -> ImportInfo -> Map NamePath [OriginInfo]
combineScopes tag info = Map.unionsWith (++) maps
    where strInfos = Map.elems $ _structInfos info
          maps     = map (topLevelScope tag) strInfos
--CR[PM->TD] : too many newlines (should be 2)



topLevelScope :: Tag -> StructInfo -> Map NamePath [OriginInfo]
topLevelScope tag sInfo = Map.fromList nps
    where scope = SI.scopeLookup (0::Int) sInfo
--CR[PM->TD] : extract this hacky case as a function
          vars  = MapForest.toList $ (case tag of Vars -> fst; Types -> snd) scope
          nps   = map (\(p, origin) -> (toNamePath p, [origin])) vars
--CR[PM->TD] : too many newlines (should be 2)



moduleObjectName :: QualPath -> NamePath
--CR[PM->TD] : use lens instead of pattern match
moduleObjectName (QualPath _ name) = NP.single name


toNamePath :: [Text] -> NamePath
--CR[PM->TD] : what does ts mean
--CR[PM->TD] : spaces on the end of line
toNamePath (t:ts) = NP.multi t ts          


qualPathToList :: QualPath -> [Text]
--CR[PM->TD] : use lens instead
--CR[PM->TD] : what does qp mean?
qualPathToList qp = (NP._path qp) ++ [NP._name qp]

-----------------------------------------------------------------------------------------
--Instances
-----------------------------------------------------------------------------------------

instance Monoid ImportInfo where
    mempty      = ImportInfo mempty mempty mempty mempty mempty mempty
    mappend a b = ImportInfo (mappend (a ^. path)        (b ^. path))
                             (mappend (a ^. imports)     (b ^. imports))
                             (mappend (a ^. structInfos) (b ^. structInfos))
                             (mappend (a ^. symTable)    (b ^. symTable))
                             (mappend (a ^. typeTable)   (b ^. typeTable))
                             (mappend (a ^. errors)      (b ^. errors))


instance (Monad m, Monoid w) => ImportInfoMonad (RWST r w ImportInfo m) where
    get = RWST.get
    put = RWST.put
                             
