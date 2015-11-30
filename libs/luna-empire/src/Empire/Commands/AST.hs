module Empire.Commands.AST where

import           Prologue
import           Control.Monad.State

import           Empire.Commands.Library (withLibrary)
import           Empire.Data.Library     (LibraryId)
import qualified Empire.Data.Library     as Library
import           Empire.Data.Project     (ProjectId)
import           Empire.Empire           (Empire, Command)
import           Empire.Utils.AST        (addVar)

addNode :: ProjectId -> LibraryId -> String -> Empire ()
addNode pid lid expr = withLibrary pid lid $ do
    refNode <- zoom Library.body $ state $ addVar expr
    return ()
