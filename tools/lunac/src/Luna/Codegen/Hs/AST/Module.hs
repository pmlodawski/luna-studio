---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Module (
    Module(..),
    empty,
    genCode
)where

import qualified Luna.Codegen.Hs.Path as  Path
import           Luna.Codegen.Hs.Path    (Path)

data Module = Module { path       :: Path
                     , submodules :: [Module]
                     --, functions :: [Function]
                     --, datatypes :: [DataType]
                     --, classes   

                        
                     } deriving (Show)

empty :: Module
empty = Module Path.empty []

genCode :: Module -> String
genCode mod = "module " ++ Path.toModulePath (path mod) ++ " where\n\n"