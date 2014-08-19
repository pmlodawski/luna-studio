---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Analysis.FuncPool.Pool where

import           Control.Monad.State
import           Data.Set            (Set)
import qualified Data.Set            as Set

import Flowbox.Prelude



type FPStateM m = MonadState Pool m

data Pool    = Pool { names :: Set String
                    } deriving (Show)


empty :: Pool
empty = Pool Set.empty


register :: FPStateM m => String -> m ()
register name = do
    s <- get
    put $ s { names = Set.insert name $ names s }

