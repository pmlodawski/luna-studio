---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Class (
    Class(..),
    empty,
    genCode,
)where

import Debug.Trace

import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                 (join)
import qualified Luna.Codegen.Hs.Path            as Path
import qualified Luna.Codegen.Hs.AST.Function    as Function
import           Luna.Codegen.Hs.AST.Function      (Function)


data Class = Class { name   :: String,
                     params :: [Expr],
                     fields :: [Expr],
                     deps   :: [Expr]
                   } deriving (Show)


empty :: Class
empty = Class "" [] [] []


genCode :: Class -> String
genCode cls = header ++ signature ++ dep ++ " where\n" ++ body where
    header    = "class " ++ (name cls) ++ " "
    signature = join " " $ map Expr.genCode (params cls)
    deps'     = deps cls
    dep       = if null deps' then "" else " | " ++ join ", " (map Expr.genCode (deps cls))
    body      = Path.indent ++ (join ("\n"++Path.indent) $ map Expr.genCode (fields cls))

