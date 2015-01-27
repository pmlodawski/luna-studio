---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Luna.Pass.Transform.Graph.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.Syntax.Control.BCZipper as BCZipper
import           Luna.Syntax.Control.Crumb    (Breadcrumbs)
import qualified Luna.Syntax.Control.Crumb    as Crumb
import qualified Luna.Syntax.Control.Focus    as Focus
import           Luna.Syntax.Module           (Module)
--import qualified Luna.AST.Name                             as Name
--import qualified Luna.Pass.Analysis.Alias.Alias            as Analysis.Alias
--import qualified Luna.Pass.Analysis.ID.MaxID               as MaxID
--import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer   as IDFixer
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser   as GraphParser
--import           Luna.Syntax.Expr                          (Expr)
import Luna.Syntax.Graph.Graph       (Graph)
import Luna.Syntax.Graph.PropertyMap (PropertyMap)



named :: a -> b -> (a, b)
named = (,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function [] "main"]


getGraph :: Breadcrumbs -> PropertyMap -> Module -> IO (Graph, PropertyMap)
getGraph bc pm ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ BCZipper.getFocus <$> BCZipper.focusBreadcrumbs' bc ast
    expr  <- focus ^? Focus.expr <??> "test.Common.getFunctionGraph : Target is not a function"
    aliasInfo <- EitherT $ Analysis.Alias.run ast
    EitherT $ GraphBuilder.run aliasInfo pm True expr


getExpr :: Breadcrumbs -> Graph -> PropertyMap -> Module -> IO (Module, PropertyMap)
getExpr bc graph pm ast = eitherStringToM' $ runEitherT $ do
    zipper <- hoistEither $ BCZipper.focusBreadcrumbs' bc ast
    let focus = BCZipper.getFocus zipper
    expr <- focus ^? Focus.expr <??> "test.Common.getExpr : Target is not a function"
    (expr2', pm2) <- EitherT $ GraphParser.run graph pm expr
    maxID         <- EitherT $ MaxID.runExpr expr2'
    expr2         <- EitherT $ IDFixer.runExpr maxID Nothing False expr2'
    let newFocus = focus & Focus.expr .~ expr2
    return (BCZipper.close $ BCZipper.modify (const newFocus) zipper, pm2)


getMain :: Module -> IO Expr
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ BCZipper.getFocus <$> BCZipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"
