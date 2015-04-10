module Luna.Data.ImportInfo where

import qualified Data.Map                 as Map
import           Data.Map                 (Map)
import qualified Flowbox.Data.MapForest   as MapForest
import           Luna.Data.StructInfo     (StructInfo, OriginInfo, Scope)
import qualified Luna.Data.StructInfo     as SI
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name.Path    (NamePath, QualPath(QualPath), multi)
import           Luna.Data.ModuleInfo     (ImportError, qualPathToPath)
import           Control.Monad.RWS        (RWST)
import qualified Control.Monad.RWS        as RWST
import           Flowbox.Prelude

type ID = Int

data ImportInfo = ImportInfo {
    _path        :: QualPath,       -- move to Namespace (?)
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


----------------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------------
setPath :: QualPath -> ImportInfo -> ImportInfo
setPath p = path .~ p



getPath :: ImportInfo -> Path
getPath info = qualPathToPath $ _path info



-- Creates the mapping between symbol names (NamePath) and their origins (OriginInfo)
createSymTable :: ImportInfo -> ImportInfo
createSymTable = createSymTableVars . createSymTableTypes

createSymTableVars :: ImportInfo -> ImportInfo
createSymTableVars info = info & (symTable .~ (combineScopesVars info))

createSymTableTypes :: ImportInfo -> ImportInfo
createSymTableTypes info = info & (typeTable .~ (combineScopesTypes info))



regError :: ImportError -> ImportInfo -> ImportInfo
regError err = errors %~ (err:)



-- combines the top-level scopes of all the imported modules
-- results in a map from NamePath to [OriginInfo] -- if a given
-- symbol appears in more than one module, the list isn't a singleton
combineScopesVars :: ImportInfo -> Map NamePath [OriginInfo]
combineScopesVars info = Map.unionsWith (++) maps
    where strInfos = Map.elems $ _structInfos info
          maps     = map topLevelScopeVars strInfos


combineScopesTypes :: ImportInfo -> Map NamePath [OriginInfo]
combineScopesTypes info = Map.unionsWith (++) maps
    where strInfos = Map.elems $ _structInfos info
          maps     = map topLevelScopeTypes strInfos



topLevelScopeVars :: StructInfo -> Map NamePath [OriginInfo]
topLevelScopeVars sInfo = Map.fromList varsNP
    where scope   = SI.scopeLookup (0::Int) sInfo
          vars    = MapForest.toList $ fst scope
          varsNP  = map (\(p, origin) -> (toNamePath p, [origin])) vars

topLevelScopeTypes :: StructInfo -> Map NamePath [OriginInfo]
topLevelScopeTypes sInfo = Map.fromList typesNP
    where scope   = SI.scopeLookup (0::Int) sInfo
          types   = MapForest.toList $ snd scope
          typesNP = map (\(p, origin) -> (toNamePath p, [origin])) types

toNamePath :: [Text] -> NamePath
toNamePath (t:ts) = multi t ts          


-----------------------------------------------------------------------------------------
--Instances
-----------------------------------------------------------------------------------------

instance Monoid ImportInfo where
    mempty      = ImportInfo mempty mempty mempty mempty mempty
    mappend a b = ImportInfo (mappend (a ^. path)        (b ^. path))
                             (mappend (a ^. structInfos) (b ^. structInfos))
                             (mappend (a ^. symTable)    (b ^. symTable))
                             (mappend (a ^. typeTable)   (b ^. typeTable))
                             (mappend (a ^. errors)      (b ^. errors))


instance (Monad m, Monoid w) => ImportInfoMonad (RWST r w ImportInfo m) where
    get = RWST.get
    put = RWST.put
                             
