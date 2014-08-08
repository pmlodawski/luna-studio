---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Graph.Common where

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                                       as Crumb
import           Flowbox.Luna.Data.AST.Expr                                              (Expr)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                                      as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                                     as Zipper
import           Flowbox.Luna.Data.Graph.Graph                                           (Graph)
import           Flowbox.Luna.Data.Pass.AliasInfo                                        (AliasInfo)
import           Flowbox.Luna.Data.Pass.Source                                           (Source (Source))
import           Flowbox.Luna.Data.PropertyMap                                           (PropertyMap)
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Flowbox.Luna.Passes.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import qualified Flowbox.Luna.Passes.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser                   as TxtParser
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder                     as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser                       as GraphParser
import           Flowbox.Prelude



getAST :: String -> IO (Expr, AliasInfo)
getAST code = eitherStringToM' $ runEitherT $ do
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source ["Main"] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast

    let bc = [Crumb.Module "Main", Crumb.Function "main" []]
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    expr  <- Focus.getFunction focus <??> "Target is not a function"
    return (expr, aliasInfo)


getGraph :: AliasInfo -> PropertyMap -> Expr -> IO (Graph, PropertyMap)
getGraph = eitherStringToM' .:. GraphBuilder.run


getExpr :: Graph -> PropertyMap -> Expr -> IO Expr
getExpr = eitherStringToM' .:. GraphParser.run

