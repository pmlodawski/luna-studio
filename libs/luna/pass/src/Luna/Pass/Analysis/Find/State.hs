---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Luna.Pass.Analysis.Find.State where

import           Control.Monad.State       hiding (when)

import           Flowbox.Prelude           hiding (pred)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger $moduleName


data IDState a = IDState { _predicate :: a -> Bool
                         , _found     :: [a]
                         }

makeLenses ''IDState


type IDStateM a m = (Functor m, MonadState (IDState a) m)


instance Default (IDState a) where
    def = IDState (const False) def


getPredicate :: IDStateM a m => m (a -> Bool)
getPredicate = gets $ view predicate

setPredicate :: IDStateM a m => (a -> Bool) -> m ()
setPredicate = modify . set predicate

testPredicate :: IDStateM a m => a -> m ()
testPredicate a = do
    pred <- getPredicate
    when (pred a) $ addFound a

addFound :: IDStateM a m => a -> m ()
addFound a = modify (found %~ (a:))

getFound :: IDStateM a m => m [a]
getFound = gets $ view found
