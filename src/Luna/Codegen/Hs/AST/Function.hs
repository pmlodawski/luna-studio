---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Function (
    Function(..),
    empty,
    genCode,
    addExpr,
    addVarAlias
)where

import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                 (join)
import qualified Luna.Codegen.Hs.GenContext         as GenContext
import           Luna.Codegen.Hs.GenContext           (GenContext)


data Function = Function { name       :: String,
                           inputs     :: [String],
                           exprs      :: [Expr]
                         } deriving (Show)


empty :: Function
empty = Function "" [] []

--genCode :: GenContext -> Function -> String
genCode func =  name func ++ " " ++ join " " (inputs func) ++ " = \n"
                 ++ "    let\n"
                 ++ join "\n" (map Expr.genCode (exprs func)) 

addExpr :: Expr -> Function -> Function
addExpr expr func = func { exprs = expr : exprs func }


addVarAlias :: (String, String) -> Function -> Function
addVarAlias (n1, n2) = addExpr (Expr.Assignment (Expr.Var n1) (Expr.Var n2) Expr.Pure)