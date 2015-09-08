---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.Pass.Transform.SimpleText.Builder.Builder where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.DEP.AST.Expr                                     (Expr)
import           Luna.DEP.Data.ASTInfo                                 (ASTInfo)
import           Luna.DEP.Pass.Pass                                    (Pass)
import qualified Luna.DEP.Pass.Pass                                    as Pass
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.Undo as Undo.ImplicitSelf
import           Luna.DEP.Util.LunaShow                                (lunaShow)



logger :: Logger
logger = getLogger $moduleName


type STBPass result = Pass Pass.NoState result


run ::  ASTInfo -> Expr -> Pass.Result (String, ASTInfo)
run = Pass.run_ (Pass.Info "SimpleTextBuilder") Pass.NoState .: fun2text


fun2text :: ASTInfo -> Expr -> STBPass (String, ASTInfo)
fun2text astInfo expr = do
    (expr', astInfo') <- EitherT $ Undo.ImplicitSelf.runExpr astInfo expr
    return (lunaShow expr', astInfo')
