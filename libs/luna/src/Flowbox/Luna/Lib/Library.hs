---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.Library (
    Library(..),
    ID,
    make
) where

import           Flowbox.Luna.Data.AST.Module (Module)
import qualified Flowbox.Luna.Data.AST.Module as Module
import qualified Flowbox.Luna.Data.AST.Type   as Type
import           Flowbox.Prelude
import           Flowbox.System.UniPath       (UniPath)



data Library = Library { name :: String
                       , path :: UniPath
                       , ast  :: Module
                       } deriving (Show)

type ID  = Int


make :: String -> UniPath -> [String] -> Library
make name' path' moduleName = Library name' path' $ Module.mk 0 $ Type.Module 1 moduleName
