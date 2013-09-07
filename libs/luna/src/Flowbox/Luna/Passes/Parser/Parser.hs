---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Parser.Parser where

import qualified Flowbox.Luna.Parser.AST.AST as LAST
import qualified Flowbox.Luna.Passes.Pass    as Pass
import           Flowbox.Luna.Passes.Pass      (PassMonad)

import qualified Flowbox.System.Log.Logger   as Logger
import           Flowbox.System.Log.Logger     
import qualified Flowbox.System.Log.LogEntry as LogEntry
import qualified Flowbox.Luna.Parser.Parser  as Parser
import           Control.Monad.State           

import qualified Prelude                     as Prelude
import           Prelude                     hiding (error)

import           Control.Error                 

import           Text.Parsec.Error             (ParseError)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Parser.Parser"

type ParserMonad m = PassMonad Pass.NoState m


run :: ParserMonad m => String -> EitherT String m LAST.Expr
run = (Pass.runM Pass.NoState) . parse


parse :: ParserMonad m => String -> EitherT String m LAST.Expr
parse input = case Parser.parse input of
    Left e -> Pass.fail
    Right v -> return v
