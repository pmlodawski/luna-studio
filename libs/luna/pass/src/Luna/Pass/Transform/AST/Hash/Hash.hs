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
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Pass.Transform.AST.Hash.Hash where

import Control.Applicative
import Control.Monad.State
import Data.Char           (ord)
--import           Data.Hashable       (hash)

import           Flowbox.Prelude           hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import           Luna.AST.Expr             (Expr)
import qualified Luna.AST.Expr             as Expr
import           Luna.AST.Module           (Module)
import qualified Luna.AST.Module           as Module
import           Luna.AST.Pat              (Pat)
import qualified Luna.AST.Pat              as Pat
import           Luna.Pass.Pass            (Pass)
import qualified Luna.Pass.Pass            as Pass



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type HashPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: Module -> Pass.Result Module
run = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . hashModule


runExpr :: Expr -> Pass.Result Expr
runExpr = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . hashExpr


hashModule :: Module -> HashPass Module
hashModule mod = Module.traverseM hashModule hashExpr pure hashPat pure mod


hashExpr :: Expr.Expr -> HashPass Expr.Expr
hashExpr ast = case ast of
    Expr.Function {} -> hashMe
    Expr.Infix    {} -> hashMe
    Expr.Accessor {} -> hashMe
    Expr.RefType  {} -> hashMe
    _                -> continue
    where hashMe   = set Expr.name (hashMe2 $ view Expr.name ast) <$> continue
          continue = Expr.traverseM hashExpr pure hashPat pure ast


hashPat :: Pat -> HashPass Pat
hashPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> Pat.traverseM hashPat pure pure pat

--FIXME [wd]: some reduntant functions here
hashMe2 :: [Char] -> [Char]
hashMe2 = concatMap hashMeBody

hashMeBody :: Char -> [Char]
hashMeBody c
    | (c >= 'a' && c <='z') || (c >= 'A' && c <='Z') = [c]
    | c == '_'                                       = "__"
    | otherwise                                      = "_" ++ (show.ord) c
