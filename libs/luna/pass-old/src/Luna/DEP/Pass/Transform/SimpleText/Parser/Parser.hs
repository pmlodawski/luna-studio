---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.Pass.Transform.SimpleText.Parser.Parser where

import Control.Monad.Trans.Either

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Control.Crumb                                        as Crumb
import qualified Luna.DEP.AST.Control.Focus                                        as Focus
import qualified Luna.DEP.AST.Control.Zipper                                       as Zipper
import           Luna.DEP.AST.Expr                                                 (Expr)
import qualified Luna.DEP.AST.Expr                                                 as Expr
import           Luna.DEP.Data.ASTInfo                                             (ASTInfo)
import qualified Luna.DEP.Data.ASTInfo                                             as ASTInfo
import qualified Luna.DEP.Data.Config                                              as Config
import qualified Luna.DEP.Parser.Parser                                            as Parser
import qualified Luna.DEP.Pass.Analysis.Alias.Alias                                as Analysis.Alias
import qualified Luna.DEP.Pass.Analysis.CallGraph.CallGraph                        as Analysis.CallGraph
import           Luna.DEP.Pass.Pass                                                (Pass)
import qualified Luna.DEP.Pass.Pass                                                as Pass
import qualified Luna.DEP.Pass.Transform.AST.DepSort.DepSort                       as Transform.DepSort
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitCalls.ImplicitCalls   as Desugar.ImplicitCalls
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes as Desugar.ImplicitScopes
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitSelf.ImplicitSelf     as Desugar.ImplicitSelf
import qualified Luna.DEP.Pass.Transform.AST.Desugar.TLRecUpdt.TLRecUpdt           as Desugar.TLRecUpdt

--FIXME[wd]: following imports should be removed after moving to plugin based structure
--           including all use cases. Nothing should modify Parser.State explicitly!
import qualified Luna.DEP.Parser.Pragma as Pragma
import qualified Luna.DEP.Parser.State  as ParserState
import           Luna.DEP.Pragma.Pragma (Pragma)



logger :: Logger
logger = getLogger $moduleName


type STPPass result = Pass Pass.NoState result


run :: String -> Expr -> Pass.Result Expr
run = Pass.run_ (Pass.Info "SimpleTextParser") Pass.NoState .: text2fun


text2fun :: String -> Expr -> STPPass Expr
text2fun code (Expr.Function i path fname _ _ _) = do
    let main = "Main"
        astInfo = ASTInfo.mk i
    ast <- case Parser.parseString code $ Parser.moduleParser [main] (patchedParserState astInfo) of
                        Left  er     -> left $ show er
                        Right (r, _) -> return r
    --(ast, _, astInfo) <- EitherT $ TxtParser.run $ Source [main] code
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitSelf.run astInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.TLRecUpdt.run astInfo ast
    aliasInfo         <- EitherT $ Analysis.Alias.run ast
    callGraph         <- EitherT $ Analysis.CallGraph.run aliasInfo ast
    ast               <- EitherT $ Transform.DepSort.run callGraph aliasInfo ast
    (ast, astInfo)    <- EitherT $ Desugar.ImplicitScopes.run astInfo aliasInfo ast
    (ast, _astInfo)   <- EitherT $ Desugar.ImplicitCalls.run astInfo ast
    let bc   = [Crumb.Module main, Crumb.Function fname path]
    focus <- hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast
    set Expr.id i <$> (Focus.getFunction focus <??> "SimpleText.Parser.text2fun : Target is not a function")

    --FIXME [PM] : assign proper ids! Current ones may conflict with other from the rest of module


patchedParserState :: ASTInfo
                   -> ParserState.State (Pragma Pragma.ImplicitSelf, (Pragma Pragma.AllowOrphans, (Pragma Pragma.TabLength, ())))
patchedParserState info' = def
    & ParserState.info .~ info'
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans
