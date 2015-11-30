module Empire.Commands.Library where

import           Prologue
import           Control.Monad.State
import           System.Path              (Path)
import qualified Data.IntMap              as IntMap

import           Empire.Data.Project      (Project, ProjectId)
import qualified Empire.Data.Project      as Project
import           Empire.Data.Library      (Library, LibraryId)
import qualified Empire.Data.Library      as Library

import           Empire.Empire            (Empire, Empire')
import qualified Empire.Empire            as Empire
import           Empire.Commands.Project  (withProject)


insertAtNewId :: Library -> Empire' Project LibraryId
insertAtNewId library = do
    libs <- use Project.libs
    let key = if IntMap.null libs then 0 else 1 + (fst . IntMap.findMax $ libs)
    Project.libs . at key ?= library
    return key

createLibrary :: ProjectId -> Maybe String -> Path -> Empire (LibraryId, Library)
createLibrary pid name path = withProject pid $ do
    let library = Library.make name path
    id <- insertAtNewId library
    return (id, library)

listLibraries :: ProjectId -> Empire [(LibraryId, Library)]
listLibraries pid = withProject pid $ uses Project.libs IntMap.toList
