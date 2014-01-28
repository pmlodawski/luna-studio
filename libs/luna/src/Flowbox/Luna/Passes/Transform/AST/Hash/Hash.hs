---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Passes.Transform.AST.Hash.Hash where

import Control.Applicative
import Control.Monad.State
import Data.Char           (ord)
--import           Data.Hashable       (hash)

import qualified Flowbox.Luna.Data.AST.Expr   as Expr
import           Flowbox.Luna.Data.AST.Module (Module)
import qualified Flowbox.Luna.Data.AST.Module as Module
import           Flowbox.Luna.Data.AST.Pat    (Pat)
import qualified Flowbox.Luna.Data.AST.Pat    as Pat
import           Flowbox.Luna.Passes.Pass     (Pass)
import qualified Flowbox.Luna.Passes.Pass     as Pass
import           Flowbox.Prelude              hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Hash.Hash"


type HashPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: Module -> Pass.Result Module
run = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . ssaModule


ssaModule :: Module -> HashPass Module
ssaModule mod = Module.traverseM ssaModule ssaExpr pure ssaPat pure mod


ssaExpr :: Expr.Expr -> HashPass Expr.Expr
ssaExpr ast = case ast of
    Expr.Function _ _ name _ _ _    -> set Expr.name (hashMe2 name) <$> continue
    Expr.Accessor _ name _          -> set Expr.name (hashMe2 name) <$> continue
    _                                                -> continue
    where --hashMe   = show.abs.hash
          continue = Expr.traverseM ssaExpr pure ssaPat pure ast


ssaPat :: Pat -> HashPass Pat
ssaPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> Pat.traverseM ssaPat pure pure pat

--FIXME [wd]: some reduntant functions here
hashMe2 :: [Char] -> [Char]
hashMe2 = concat.(map hashMeBody)

hashMeBody :: Char -> [Char]
hashMeBody c
    | (c >= 'a' && c <='z') || (c >= 'A' && c <='Z') = [c]
    | c == '_'                                       = "__"
    | otherwise                                      = "_" ++ (show.ord) c
