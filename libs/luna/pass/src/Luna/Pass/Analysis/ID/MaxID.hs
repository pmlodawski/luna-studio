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

module Luna.Pass.Analysis.ID.MaxID where

import           Control.Monad.State

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



data MaxIDs = MaxIDs


type MaxIDPass                 m   = StateT IDState m
type MaxIDCtx              lab m a = (Enumerated lab, MaxIDTraversal m a)
type MaxIDTraversal            m a = (Monad m, Functor m, AST.Traversal        MaxIDs (MaxIDPass m) a a)
type MaxIDDefaultTraversal     m a = (Monad m, Functor m, AST.DefaultTraversal MaxIDs (MaxIDPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: MaxIDTraversal m a => a -> MaxIDPass m a
traverseM = AST.traverseM MaxIDs

defaultTraverseM :: MaxIDDefaultTraversal m a => a -> MaxIDPass m a
defaultTraverseM = AST.defaultTraverseM MaxIDs

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: MaxIDDefaultTraversal m a => Pass IDState (a -> MaxIDPass m AST.ID)
pass = Pass "Extract maximum ID"
            "Extract maximum ID contained in labels"
            def extract

extract :: MaxIDDefaultTraversal m a => a -> MaxIDPass m AST.ID
extract ast = defaultTraverseM ast >> State.getFoundID


run :: MaxIDDefaultTraversal Identity a => a -> AST.ID
run a = evalState (extract a) def


maxIDLabel :: MaxIDCtx lab m a => Label lab a -> MaxIDPass m (Label lab a)
maxIDLabel label = do
    State.findMaxID $ Enum.id $ label ^. Label.label
    defaultTraverseM label


instance MaxIDCtx lab m a => AST.Traversal MaxIDs (MaxIDPass m) (Label lab a) (Label lab a) where
    traverseM _ = maxIDLabel
