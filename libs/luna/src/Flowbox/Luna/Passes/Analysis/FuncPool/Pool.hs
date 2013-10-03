---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Analysis.FuncPool.Pool where

import           Flowbox.Prelude                   
import           Control.Monad.State    
import qualified Data.Set            as Set
import           Data.Set              (Set)


data Pool    = Pool { names :: Set String
                    } deriving (Show)

type FPStateM m = MonadState Pool m


empty :: Pool
empty = Pool Set.empty


register :: FPStateM m => String -> m ()
register name = do
    s <- get
    put $ s { names = Set.insert name $ names s }

