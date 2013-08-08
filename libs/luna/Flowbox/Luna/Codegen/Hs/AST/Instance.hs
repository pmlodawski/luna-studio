---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.AST.Instance (
    Instance(..),
    empty,
    genCode,
)where

import Debug.Trace

import qualified Flowbox.Luna.Codegen.Hs.AST.Expr        as Expr
import           Flowbox.Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                         (join)
import qualified Flowbox.Luna.Codegen.Hs.Path            as Path
import qualified Flowbox.Luna.Codegen.Hs.AST.Function    as Function
import           Flowbox.Luna.Codegen.Hs.AST.Function      (Function)


data Instance = Instance { name   :: String,
                           params :: [Expr],
                           funcs  :: [Function]
                         } deriving (Show)


empty :: Instance
empty = Instance "" [] [] 


genCode :: Instance -> String
genCode inst = header ++ signature ++ " where\n" ++ body where
    header    = "instance " ++ (name inst) ++ " "
    signature = join " " $ map Expr.genCode (params inst)
    body      = join "\n" $ map Function.genCode (funcs inst)

