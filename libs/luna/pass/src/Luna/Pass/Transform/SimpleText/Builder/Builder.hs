---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Pass.Transform.SimpleText.Builder.Builder where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.AST.Expr                                     (Expr)
import           Luna.Data.ASTInfo                                 (ASTInfo)
import           Luna.Graph.PropertyMap                            (PropertyMap)
import           Luna.Pass.Pass                                    (Pass)
import qualified Luna.Pass.Pass                                    as Pass
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.Undo as Undo.ImplicitSelf
import           Luna.Util.LunaShow                                (lunaShow)



logger :: Logger
logger = getLogger $(moduleName)


type STBPass result = Pass Pass.NoState result


run :: PropertyMap -> ASTInfo -> Expr -> Pass.Result (String, PropertyMap, ASTInfo)
run = Pass.run_ (Pass.Info "SimpleTextBuilder") Pass.NoState .:. fun2text


fun2text :: PropertyMap -> ASTInfo -> Expr -> STBPass (String, PropertyMap, ASTInfo)
fun2text propertyMap astInfo expr = do
    (expr, astInfo) <- EitherT $ Undo.ImplicitSelf.runExpr astInfo expr
    return (lunaShow expr, propertyMap, astInfo)
