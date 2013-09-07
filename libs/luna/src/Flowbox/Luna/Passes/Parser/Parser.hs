---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Parser.Parser where

import qualified Flowbox.Luna.Parser.AST.AST as LAST
import qualified Flowbox.Luna.Passes.Pass    as Pass
import           Flowbox.Luna.Passes.Pass      (PassMonad)
import qualified Flowbox.Luna.Data.Source    as Source
import           Flowbox.Luna.Data.Source      (Source)

import           Flowbox.System.Log.Logger     
import qualified Flowbox.Luna.Parser.Parser  as Parser
import           Control.Monad.State           

import qualified Prelude                     as Prelude
import           Prelude                     hiding (error)


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Parser.Parser"

type ParserMonad m = PassMonad Pass.NoState m


run :: PassMonad s m => Source -> Pass.Result m LAST.Expr
run = (Pass.runM Pass.NoState) . parse


parse :: ParserMonad m => Source -> Pass.Result m LAST.Expr
parse (Source.Source mod code) = case Parser.parse code of
    Left  _ -> Pass.fail
    Right v -> return v
