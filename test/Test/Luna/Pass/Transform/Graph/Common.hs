---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Test.Luna.Pass.Transform.Graph.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.AST.Control.Crumb                    (Breadcrumbs)
import qualified Luna.AST.Control.Crumb                    as Crumb
import qualified Luna.AST.Control.Focus                    as Focus
import qualified Luna.AST.Control.Zipper                   as Zipper
import           Luna.AST.Expr                             (Expr)
import           Luna.AST.Module                           (Module)
import qualified Luna.AST.Name                             as Name
import           Luna.Graph.Graph                          (Graph)
import           Luna.Graph.PropertyMap                    (PropertyMap)
import qualified Luna.Pass.Analysis.Alias.Alias            as Analysis.Alias
import qualified Luna.Pass.Analysis.ID.MaxID               as MaxID
import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer   as IDFixer
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser   as GraphParser



named :: a -> b -> (a, b)
named = (,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function (Name.single "main") []]


getGraph :: Breadcrumbs -> PropertyMap -> Module -> IO (Graph, PropertyMap)
getGraph bc pm ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    expr  <- focus ^? Focus.expr <??> "test.Common.getFunctionGraph : Target is not a function"
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    EitherT $ GraphBuilder.run aliasInfo pm True expr


getExpr :: Breadcrumbs -> Graph -> PropertyMap -> Module -> IO (Module, PropertyMap)
getExpr bc graph pm ast = eitherStringToM' $ runEitherT $ do
    zipper <- hoistEither $ Zipper.focusBreadcrumbs' bc ast
    let focus = Zipper.getFocus zipper
    expr <- focus ^? Focus.expr <??> "test.Common.getExpr : Target is not a function"
    (expr2', pm2) <- EitherT $ GraphParser.run graph pm expr
    maxID         <- EitherT $ MaxID.runExpr expr2'
    expr2         <- EitherT $ IDFixer.runExpr maxID Nothing False expr2'
    let newFocus = focus & Focus.expr .~ expr2
    return (Zipper.close $ Zipper.modify (const newFocus) zipper, pm2)


getMain :: Module -> IO Expr
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"
