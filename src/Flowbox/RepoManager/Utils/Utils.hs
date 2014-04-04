module Flowbox.RepoManager.Utils.Utils where

import           Flowbox.Prelude
import qualified Data.List as List
import           System.FilePath  (pathSeparator)

concatPath :: [String] -> String
concatPath directories = List.intercalate [pathSeparator] directories