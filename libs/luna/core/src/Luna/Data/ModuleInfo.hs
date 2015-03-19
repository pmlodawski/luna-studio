
module Luna.Data.ModuleInfo where

import           Data.Map               (Map)
import qualified Data.Map       as Map
import           System.Directory       (findFile)
import           System.FilePath        (joinPath)


import           Luna.Data.StructInfo   (StructInfo)
import           Luna.Syntax.Name.Path
import           Luna.Syntax.AST        (ID)
import           Luna.Syntax.Name       (TName(TName), TNameP)
import           Luna.Syntax.Decl       (Path)

import           Flowbox.System.UniPath (UniPath)
import           Flowbox.Prelude

type Name = String

-- stores the information about a module, needed while importing
-- and resolving names. Checking whether a file needs recompilation is done based on the file  edit dates
data ModuleInfo = ModuleInfo {
                     _name     :: NamePath,
                     _path     :: UniPath,
                     _strInfo  :: StructInfo,
                     _symTable :: Map Name ID
                  }

makeLenses ''ModuleInfo



nameExists :: Name -> ModuleInfo -> Bool
nameExists name mInfo = Map.member name (_symTable mInfo)



getSymbolId :: Name -> ModuleInfo -> Maybe ID
getSymbolId name mInfo = Map.lookup name (_symTable mInfo)



moduleExists :: Path -> IO Bool
moduleExists path = do
    f <- findFile ["."] (modPathToString path)
    return $ case f of
        Just p  -> True
        Nothing -> False



-- Path == [TNameP] == [TName NamePath]
modPathToString :: Path -> String
modPathToString path =  joinPath $ map getFromTName path
    where getFromTName (TName np) = toString np
 
