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
mainBC = [Crumb.Module "Main", Crumb.Function "main" []]


getGraph :: PropertyMap -> Module -> IO (Graph, PropertyMap)
getGraph pm ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' mainBC ast
    expr  <- Focus.getFunction focus <??> "test.Common.getGraph : Target is not a function"

    aliasInfo <- EitherT $ Analysis.Alias.run ast
    EitherT $ GraphBuilder.run aliasInfo pm expr


getExpr :: Graph -> PropertyMap -> Module -> IO (Module, PropertyMap)
getExpr graph pm ast = eitherStringToM' $ runEitherT $ do

    zipper <- hoistEither $ Zipper.focusBreadcrumbs' mainBC ast

    expr  <- Focus.getFunction (Zipper.getFocus zipper) <??> "test.Common.getExpr : Target is not a function"

    (expr2', pm2) <- EitherT $ GraphParser.run graph pm expr
    maxID         <- EitherT $ MaxID.runExpr expr2'
    expr2         <- EitherT $ IDFixer.runExpr maxID Nothing False expr2'

    return (Zipper.close $ Zipper.modify (const $ Focus.Function expr2) zipper, pm2)


getMain :: Module -> IO Expr
getMain ast = eitherStringToM' $ runEitherT $ do
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' mainBC ast
    Focus.getFunction focus <??> "test.Common.getMain : Target is not a function"
