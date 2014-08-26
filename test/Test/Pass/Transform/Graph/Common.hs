---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections #-}

module Test.Pass.Transform.Graph.Common where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Luna.AST.Control.Crumb                                        (Breadcrumbs)
import qualified Luna.AST.Control.Crumb                                        as Crumb
import qualified Luna.AST.Control.Focus                                        as Focus
import qualified Luna.AST.Control.Zipper                                       as Zipper
import           Luna.AST.Expr                                                 (Expr)
import           Luna.AST.Module                                               (Module)
import           Luna.Data.Source                                              (Source (Source))
import           Luna.Graph.Graph                                              (Graph)
import           Luna.Graph.PropertyMap                                        (PropertyMap)
import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Luna.Pass.Analysis.ID.MaxID                                   as MaxID
import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer                       as IDFixer
import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Luna.Pass.Transform.Graph.Builder.Builder                     as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser                       as GraphParser



named :: a -> b -> (a, b)
named = (,)


mainBC :: Breadcrumbs
mainBC = [Crumb.Module "Main", Crumb.Function "main" []]


getAST :: String -> IO Module
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    return ast


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
