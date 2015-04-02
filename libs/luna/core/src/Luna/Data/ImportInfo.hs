module Luna.Data.ImportInfo where

import qualified Data.Map                 as Map
import           Data.Map                 (Map)
import           Luna.Data.StructInfo     (StructInfo)
import           Luna.Syntax.Decl         (Path)
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import           Luna.Data.ModuleInfo     (ImportError)
import           Flowbox.Prelude

type ID = Int

data ImportInfo = ImportInfo {
    _strInfos :: Map Path StructInfo,
    _symTable :: Map NamePath (QualPath, ID),
    _errors   :: [ImportError]
}             deriving (Show)

makeLenses ''ImportInfo



 -- appendScope :: Path -> StructInfo -> ImportInfo -> ImportInfo
-- append  


instance Monoid ImportInfo where
    mempty      = ImportInfo  mempty mempty mempty
    mappend a b = ImportInfo (mappend (a ^. strInfos) (b ^. strInfos))
                             (mappend (a ^. symTable) (b ^. symTable))
                             (mappend (a ^. errors) (b ^. errors))






