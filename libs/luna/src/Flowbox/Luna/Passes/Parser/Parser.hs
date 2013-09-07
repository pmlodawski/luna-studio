---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds, TupleSections #-}

module Flowbox.Luna.Passes.Parser.Parser where

import qualified Flowbox.Luna.Parser.AST.AST            as LAST
import qualified Flowbox.Luna.Passes.Pass               as Pass
import           Flowbox.Luna.Passes.Pass                 (Pass)

import qualified Flowbox.System.Log.Logger              as Logger
import           Flowbox.System.Log.Logger                
import qualified Flowbox.System.Log.LogEntry            as LogEntry
import qualified Flowbox.Luna.Parser.Parser             as Parser
import           Control.Monad.State   

import qualified Prelude                                as Prelude
import           Prelude                                hiding (error)

import           Control.Error                            

import           Text.Parsec.Error                        (ParseError)

logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Parser.Parser"

type ParserMonad m = (Functor m, MonadState Pass.NoState m, LogWriter m)

--type ParseM m = Pass Pass.NoState m


run :: ParserMonad m => String -> EitherT ParseError m LAST.Expr
run = (Pass.runM Pass.NoState) . parse

--run :: String -> (Either ParseError LAST.Expr,  Pass.NoState, LogList)
--run = (Pass.run Pass.NoState) . parse

parse :: ParserMonad m => String -> EitherT ParseError m LAST.Expr
parse input = hoistEither $ Parser.parse input
