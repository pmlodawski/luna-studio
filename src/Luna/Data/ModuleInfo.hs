
module Luna.Data.ModuleInfo where

import           Data.Maybe             (fromMaybe)
import           Data.Map               (Map)
import qualified Data.Map       as Map
import           System.Environment     (lookupEnv)
import           System.Directory       (findFile, createDirectory)
import           System.FilePath        (joinPath, (</>))


import           Luna.Data.StructInfo   (StructInfo)
import           Luna.Syntax.Name.Path  (NamePath)
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



-- checks if the module exists (but not if it's parsed)
moduleExists :: Path -> IO Bool
moduleExists path = do
    let fullPath = modPathToString path ++ ".hs"
    f <- findFile ["."] fullPath 
    return $ case f of
        Just p  -> True
        Nothing -> False



-- checks if module is already parsed (i.e. the ModuleInfo is present)
moduleIsParsed :: Path -> IO Bool
moduleIsParsed path = do
    let fullPath = modPathToString path ++ liFileSuffix
    liPath <- liFilePath
    f      <- findFile [liPath] fullPath
    return $ case f of
        Just p  -> True
        Nothing -> False



modPathToString :: Path -> String
modPathToString path =  joinPath $ map getFromTName path
    where getFromTName (TName np) = toString np



-- ModuleInfo serialization utils:
liFileSuffix :: FilePath
liFileSuffix = "li"

liFilePath :: IO FilePath
liFilePath = do
    r <- lookupEnv "LUNAROOT"
    let root = fromMaybe "." r
    return $ root </> "modinfo"

