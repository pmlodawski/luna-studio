---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Pass.Transform.SimpleText.Parser.Parser where

import Control.Monad.Trans.Either

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Control.Crumb                                        as Crumb
import qualified Luna.AST.Control.Focus                                        as Focus
import qualified Luna.AST.Control.Zipper                                       as Zipper
import           Luna.AST.Expr                                                 (Expr)
import qualified Luna.AST.Expr                                                 as Expr
import           Luna.Data.Source                                              (Source (Source))
import qualified Luna.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import           Luna.Pass.Pass                                                (Pass)
import qualified Luna.Pass.Pass                                                as Pass
import qualified Luna.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt
import qualified Luna.Pass.Transform.AST.TxtParser.TxtParser                   as TxtParser



logger :: Logger
logger = getLogger $(moduleName)


type STPPass result = Pass Pass.NoState result


run :: String -> Expr -> Pass.Result Expr
run = Pass.run_ (Pass.Info "SimpleTextParser") Pass.NoState .: text2fun


text2fun :: String -> Expr -> STPPass Expr
text2fun code (Expr.Function _ path fname _ _ _) = do
    let main = "Main"
    (ast, _, astInfo) <- EitherT $ TxtParser.run $ Source [main] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    let bc   = [Crumb.Module main, Crumb.Function fname path]
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    Focus.getFunction focus <??> "SimpleText.Parser.text2fun : Target is not a function"

    --FIXME [PM] : assign proper ids! Current ones may conflict with other from the rest of module
