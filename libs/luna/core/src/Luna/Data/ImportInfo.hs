---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------
module Luna.Data.ImportInfo where

import           Control.Monad.RWS (RWST)
import qualified Control.Monad.RWS as RWST
import           Data.Map          (Map)
import qualified Data.Map          as Map

import qualified Flowbox.Data.MapForest as MapForest
import           Flowbox.Prelude
import           Luna.Data.ModuleInfo   (ImportError, qualPathToPath)
import           Luna.Data.StructInfo   (OriginInfo (OriginInfo), Scope, StructInfo)
import qualified Luna.Data.StructInfo   as SI
import           Luna.Syntax.Decl       (Path)
import           Luna.Syntax.Name.Path  (NamePath, QualPath (QualPath))
import qualified Luna.Syntax.Name.Path  as Q
import qualified Luna.Syntax.Name.Path  as NP



type ID = Int

data Tag = Vars | Types deriving (Show, Eq)


data Import = Import { _impPath  :: QualPath
                     , _wildcard :: Bool
                     , _hiding   :: [NamePath]
                     , _targets  :: [NamePath]
                     , _rename   :: Maybe NamePath
                     } deriving (Generic, Show, Eq, Read)

makeLenses ''Import

data ImportInfo = ImportInfo { _path        :: QualPath -- move to Namespace (?)
                             , _imports     :: [Import]
                             , _structInfos :: Map QualPath StructInfo
                             , _symTable    :: Map NamePath [OriginInfo]
                             , _typeTable   :: Map NamePath [OriginInfo]
                             , _errors      :: [ImportError]
                             } deriving (Generic, Show, Eq, Read)

makeLenses ''ImportInfo

----------------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------------
class ImportInfoMonad m where
    get :: m ImportInfo
    put :: ImportInfo -> m ()


----------------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------------
setPath :: QualPath -> ImportInfo -> ImportInfo
setPath p = path .~ p


getPath :: ImportInfo -> Path
getPath info = qualPathToPath $ info ^. path


getImportPaths :: ImportInfo -> [QualPath]
getImportPaths info = map _impPath (info ^. imports)

-- Creates the mapping between symbol names (NamePath) and their origins (OriginInfo)
createSymTable :: ImportInfo -> ImportInfo
createSymTable = addMNameToSymTable . addMNameToTypeTable . createSymTableVars . createSymTableTypes

-- TODO[PMo] add every module name to both type- and symTable
addMNameToSymTable :: ImportInfo -> ImportInfo
addMNameToSymTable info = info & (symTable %~ (Map.unionWith (++) (createNameMap $ info ^. imports)))

addMNameToTypeTable :: ImportInfo -> ImportInfo
addMNameToTypeTable info = info & (typeTable %~ (Map.unionWith (++) (createNameMap $ info ^. imports)))

createNameMap :: [Import] -> Map NamePath [OriginInfo]
createNameMap imps = Map.fromList tuples
    where tuples = fmap (\imp -> (moduleObjectName (imp ^. impPath), [OriginInfo (imp ^. impPath) 0])) imps -- TODO check if it's really 0

createSymTableVars :: ImportInfo -> ImportInfo
createSymTableVars info = info & (symTable .~ (combineScopes Vars info))

createSymTableTypes :: ImportInfo -> ImportInfo
createSymTableTypes info = info & (typeTable .~ (combineScopes Types info))


regError :: ImportError -> ImportInfo -> ImportInfo
regError err = errors %~ (err:)


-- combines the top-level scopes of all the imported modules
-- results in a map from NamePath to [OriginInfo] -- if a given
-- symbol appears in more than one module, the list isn't a singleton
combineScopes :: Tag -> ImportInfo -> Map NamePath [OriginInfo]
combineScopes tag info = Map.unionsWith (++) maps
    where strInfos = Map.elems $ info ^. structInfos
          maps     = map (topLevelScope tag) strInfos


topLevelScope :: Tag -> StructInfo -> Map NamePath [OriginInfo]
topLevelScope tag sInfo = Map.fromList nps
    where scope = SI.scopeLookup (0::Int) sInfo
          vars  = MapForest.toList $ (unpackTuple tag) scope
          nps   = map (\(p, origin) -> (toNamePath p, [origin])) vars
          unpackTuple Vars  = fst
          unpackTuple Types = snd

moduleObjectName :: QualPath -> NamePath
moduleObjectName path = NP.single (path ^. Q.name)


toNamePath :: [Text] -> NamePath
toNamePath (text:texts) = NP.multi text texts


qualPathToList :: QualPath -> [Text]
qualPathToList qualPath = (qualPath ^. NP.path) ++ [qualPath ^. NP.name]

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

