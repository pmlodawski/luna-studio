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

module Luna.Pass.Analysis.ID.MinID where

import Control.Monad.State

import           Flowbox.Prelude             hiding (mapM, mapM_)
import           Luna.Pass                   (Pass (Pass))
import           Luna.Pass.Analysis.ID.State (IDState)
import qualified Luna.Pass.Analysis.ID.State as State
import qualified Luna.Syntax.AST             as AST
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Enum            as Enum
import           Luna.Syntax.Label           (Label)
import qualified Luna.Syntax.Label           as Label
import qualified Luna.Syntax.Traversals      as AST



data MinIDs = MinIDs


type MinIDPass                 m   = StateT IDState m
type MinIDCtx              lab m a = (Enumerated lab, MinIDTraversal m a)
type MinIDTraversal            m a = (Monad m, Functor m, AST.Traversal        MinIDs (MinIDPass m) a a)
type MinIDDefaultTraversal     m a = (Monad m, Functor m, AST.DefaultTraversal MinIDs (MinIDPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: MinIDTraversal m a => a -> MinIDPass m a
traverseM = AST.traverseM MinIDs

defaultTraverseM :: MinIDDefaultTraversal m a => a -> MinIDPass m a
defaultTraverseM = AST.defaultTraverseM MinIDs

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: MinIDDefaultTraversal m a => Pass IDState (a -> MinIDPass m AST.ID)
pass = Pass "Extract minimum ID"
            "Extract minimum ID contained in labels"
            def extract

extract :: MinIDDefaultTraversal m a => a -> MinIDPass m AST.ID
extract ast = defaultTraverseM ast >> State.getFoundID


run :: MinIDDefaultTraversal Identity a => a -> AST.ID
run a = evalState (extract a) def


minIDLabel :: MinIDCtx lab m a => Label lab a -> MinIDPass m (Label lab a)
minIDLabel label = do
    State.findMinID $ Enum.id $ label ^. Label.label
    defaultTraverseM label


instance MinIDCtx lab m a => AST.Traversal MinIDs (MinIDPass m) (Label lab a) (Label lab a) where
    traverseM _ = minIDLabel

