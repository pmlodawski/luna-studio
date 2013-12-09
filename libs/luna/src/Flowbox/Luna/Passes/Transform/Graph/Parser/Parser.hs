---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import           Control.Applicative                                 
import           Control.Monad.State                                 
import qualified Data.Map                                          as Map
import           Data.Map                                            (Map)
import qualified Data.List as List

import           Flowbox.Prelude                                   hiding (error, mapM, mapM_)
import           Flowbox.Luna.Data.AliasAnalysis                     (AA)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import           Flowbox.Luna.Data.AST.Lit                           (Lit)
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import           Flowbox.Luna.Data.AST.Pat                           (Pat)
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import qualified Flowbox.Luna.Data.Attributes                      as Attributes
import           Flowbox.Luna.Data.Graph.Edge                        (Edge(Edge))
import qualified Flowbox.Luna.Data.Graph.Flags                     as Flags
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import           Flowbox.Luna.Data.Graph.Node                        (Node)
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Pass                            (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State   (GPState)
import           Flowbox.System.Log.Logger                           



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


type GPMonad m = PassMonad GPState m


run :: PassMonad s m => AA -> Graph -> Pass.Result m Expr
run aa = (Pass.run_ (Pass.Info "GraphParser") State.empty) . graph2expr


graph2expr :: GPMonad m => Graph -> Pass.Result m Expr
graph2expr graph = undefined
