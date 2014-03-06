---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Passes.Analysis.CallGraph.CallGraph where

import Control.Applicative
import           Control.Monad.State (get)

import qualified Flowbox.Luna.Data.AST.Expr                 as Expr
import           Flowbox.Luna.Data.AST.Module               (Module)
import qualified Flowbox.Luna.Data.AST.Module               as Module
import           Flowbox.Luna.Data.AST.Pat                  (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                  as Pat
import           Flowbox.Luna.Data.AST.Type                 (Type)
import qualified Flowbox.Luna.Data.AST.Type                 as Type
import           Flowbox.Luna.Passes.Analysis.CallGraph.State (State)
import qualified Flowbox.Luna.Passes.Analysis.CallGraph.State as State
import           Flowbox.Luna.Passes.Pass                   (Pass)
import qualified Flowbox.Luna.Passes.Pass                   as Pass
import           Flowbox.Prelude                            hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import           Flowbox.Luna.Data.Pass.AliasInfo                               (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo                               as AliasInfo
import           Flowbox.Luna.Data.Pass.CallGraph (CallGraph)
import qualified Flowbox.Luna.Data.AST.AST                                      as AST

logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.CallGraph.CallGraph"


type CGPass result = Pass State result




run :: AliasInfo -> Module -> Pass.Result CallGraph
run info = (Pass.run_ (Pass.Info "CallGraph") $ State.mk info) . cgMod


cgMod :: Module -> CGPass CallGraph
cgMod el@(Module.Module id cls imports classes typeAliases typeDefs fields methods modules) = do
    mapM_ cgRegisterFunc methods
    mapM_ cgExpr methods
    view State.cg <$> get


cgRegisterFunc :: Expr.Expr -> CGPass ()
cgRegisterFunc el@(Expr.Function {}) = State.registerFunction (el ^. Expr.id)

cgExpr :: Expr.Expr -> CGPass ()
cgExpr el = case el of
    Expr.Function   {} -> withID continue
    _                  -> do
                          info <- State.getInfo
                          let mTargetID = info ^. AliasInfo.aliasMap ^. at id
                              mTargetAST = (do tid <- mTargetID; info ^. AliasInfo.astMap ^. at tid)
                          case mTargetAST of
                              Nothing  -> return ()
                              Just ast -> case ast of
                                   AST.Expr (func@(Expr.Function {})) -> State.registerCall (func ^. Expr.id)
                                   _                                  -> return ()

                          continue
    where id        = el ^. Expr.id
          withID    = State.withID id
          continue  = Expr.traverseM_ cgExpr pure pure pure el


