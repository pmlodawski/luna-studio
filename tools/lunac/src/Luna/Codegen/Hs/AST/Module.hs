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

import qualified Luna.Codegen.Hs.Path            as Path
import           Luna.Codegen.Hs.Path              (Path)
import qualified Luna.Codegen.Hs.Import          as Import
import           Luna.Codegen.Hs.Import            (Import)
import qualified Luna.Codegen.Hs.AST.Function    as Function
import           Luna.Codegen.Hs.AST.Function      (Function)
import qualified Luna.Codegen.Hs.AST.DataType    as DataType
import           Luna.Codegen.Hs.AST.DataType      (DataType)
import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                 (join)

data Module = Module { path       :: Path
                     , submodules :: [Module]
                     , datatypes  :: [DataType]
                     , functions  :: [Function]
                     , imports    :: [Import]
                     , exprs      :: [Expr]
                     --, datatypes :: [DataType]
                     --, classes   

                        
                     } deriving (Show)

empty :: Module
empty = Module Path.empty [] [] [] [] []

genCode :: Module -> String
genCode mod = "module " ++ mypath ++ " where\n" ++ imps ++ "\n" ++ dtypes ++ funcs
    where
        mypath = (Path.toModulePath . path) mod
        imps   = join "\n" $ map Import.genCode   (imports mod)
        dtypes = join "\n" $ map DataType.genCode (datatypes mod)
        funcs  = join "\n" $ map Function.genCode (functions mod)
