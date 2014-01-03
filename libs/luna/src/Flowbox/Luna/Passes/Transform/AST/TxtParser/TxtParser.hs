---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser where

import qualified Flowbox.Luna.Passes.Pass                           as Pass
import           Flowbox.Luna.Passes.Pass                             (Pass)
import           Flowbox.Luna.Data.Source                             (Source)
import           Flowbox.Luna.Data.AST.Module                         (Module)
import           Flowbox.System.Log.Logger                            
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser
import           Flowbox.Luna.Data.Pass.SourceMap                   (SourceMap)

import           Control.Monad.State                                  
import           Flowbox.Prelude                                    hiding (error)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser"


type ParserPass m = Pass Pass.NoState m


run :: Source -> Pass.Result (Module, SourceMap)
run = (Pass.run_ (Pass.Info "Luna Parser") Pass.NoState) . parse


parse :: Source -> ParserPass (Module, SourceMap)
parse src = case Parser.parse src of
    Left  e -> Pass.fail $ show e
    Right v -> return v
