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
{-# LANGUAGE TupleSections             #-}

module Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser where

import           Flowbox.Luna.Data.AST.Module                       (Module)
import           Flowbox.Luna.Data.Pass.ASTInfo                     (ASTInfo)
import           Flowbox.Luna.Data.Pass.Source                      (Source)
import           Flowbox.Luna.Data.Pass.SourceMap                   (SourceMap)
import           Flowbox.Luna.Passes.Pass                           (Pass)
import qualified Flowbox.Luna.Passes.Pass                           as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import           Flowbox.System.Log.Logger

import Control.Monad.State
import Flowbox.Prelude     hiding (error)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser"


type ParserPass m = Pass Pass.NoState m


run :: Source -> Pass.Result (Module, SourceMap, ASTInfo)
run = (Pass.run_ (Pass.Info "Luna Parser") Pass.NoState) . parse


parse :: Source -> ParserPass (Module, SourceMap, ASTInfo)
parse src = case Parser.parse src of
    Left  e -> Pass.fail $ show e
    Right v -> return v
