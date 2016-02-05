---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Luna.Pass.Analysis.Find.Find where

import           Control.Monad.State

import           Flowbox.Prelude               hiding (mapM, mapM_, pred)
import           Luna.Pass                     (Pass (Pass))
import           Luna.Pass.Analysis.Find.State (IDState)
import qualified Luna.Pass.Analysis.Find.State as State
import           Luna.Syntax.Enum              (Enumerated)
import           Luna.Syntax.Label             (Label)
import qualified Luna.Syntax.Traversals        as AST



----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Find = Find

type FPass                 r m = StateT (IDState r) m
type FCtx                lab m a = (Enumerated lab, FTraversal (Label lab a) m a)
type FTraversal            r m a = (Monad m, Functor m, AST.Traversal        Find (FPass r m) a a)
type FDefaultTraversal     r m a = (Monad m, Functor m, AST.DefaultTraversal Find (FPass r m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: FTraversal a m a => a -> FPass a m a
traverseM = AST.traverseM Find

defaultTraverseM :: FDefaultTraversal a m a => a -> FPass a m a
defaultTraverseM = AST.defaultTraverseM Find

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: FDefaultTraversal a m a => Pass (IDState a) ((a -> Bool) -> a -> FPass a m [a])
pass = Pass "Find expressions in ast"
            "Find expressions in ast matching predicate"
            def extract

extract :: FDefaultTraversal a m a => (a -> Bool) -> a -> FPass a m [a]
extract pred ast = State.setPredicate pred >> eidLabel ast >> State.getFound


run :: FDefaultTraversal a Identity a => (a -> Bool) -> a -> [a]
run pred a = evalState (extract pred a) def


--eidLabel :: FCtx r lab m a => Label lab a -> FPass r m (Label lab a)
eidLabel label = do
    State.testPredicate label
    defaultTraverseM label


instance FCtx lab m a => AST.Traversal Find (FPass (Label lab a) m) (Label lab a) (Label lab a) where
    traverseM _ = eidLabel
