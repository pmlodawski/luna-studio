---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Transform.AST.TxtParser.TxtParser where

import qualified Flowbox.Luna.Passes.Pass                           as Pass
import           Flowbox.Luna.Passes.Pass                             (PassMonad)
import           Flowbox.Luna.Data.Source                             (Source)
import           Flowbox.Luna.Data.AST.Module                         (Module)
import           Flowbox.System.Log.Logger                            
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser as Parser

import           Control.Monad.State                                  
import           Flowbox.Prelude                                    hiding (error)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Parser.Parser"

type ParserMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => Source -> Pass.Result m Module
run = (Pass.run_ Pass.NoState) . parse


parse :: ParserMonad m => Source -> Pass.Result m Module
parse src = case Parser.parse src of
    Left  e -> Pass.fail $ show e
    Right v -> return v
