---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Test.Luna.DEP.Pass.Transform.Graph.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.DEP.AST.Control.Crumb                    (Breadcrumbs)
import qualified Luna.DEP.AST.Control.Crumb                    as Crumb
import qualified Luna.DEP.AST.Control.Focus                    as Focus
import qualified Luna.DEP.AST.Control.Zipper                   as Zipper
import           Luna.DEP.AST.Expr                             (Expr)
import           Luna.DEP.AST.Module                           (Module)
import qualified Luna.DEP.AST.Name                             as Name
import           Luna.DEP.Data.ASTInfo                         (ASTInfo)
import           Luna.DEP.Graph.Graph                          (Graph)
import           Luna.DEP.Graph.PropertyMap                    (PropertyMap)
import qualified Luna.DEP.Pass.Analysis.Alias.Alias            as Analysis.Alias
import qualified Luna.DEP.Pass.Transform.AST.IDFixer.IDFixer   as IDFixer
import qualified Luna.DEP.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.DEP.Pass.Transform.Graph.Parser.Parser   as GraphParser



named :: a -> b -> (a, b)
named = (,)


named3 :: a -> b -> c -> (a, b, c)
named3 = (,,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function (Name.single "main") []]


getGraph :: Breadcrumbs -> PropertyMap -> Module -> IO (Graph, PropertyMap)
getGraph bc pm ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    expr  <- focus ^? Focus.expr <??> "test.Common.getFunctionGraph : Target is not a function"
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    EitherT $ GraphBuilder.run aliasInfo pm True expr


getExpr :: Breadcrumbs -> Graph -> PropertyMap -> Module -> ASTInfo -> IO (Module, PropertyMap, ASTInfo)
getExpr bc graph pm ast astInfo = eitherStringToM' $ runEitherT $ do
    zipper <- hoistEither $ Zipper.focusBreadcrumbs' bc ast
    let focus = Zipper.getFocus zipper
    expr <- focus ^? Focus.expr <??> "test.Common.getExpr : Target is not a function"
    (expr2', pm2)     <- EitherT $ GraphParser.run graph pm expr
    (expr2, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing False expr2'
    let newFocus = focus & Focus.expr .~ expr2
    return (Zipper.close $ Zipper.modify (const newFocus) zipper, pm2, astInfo')


getMain :: Module -> IO Expr
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"
