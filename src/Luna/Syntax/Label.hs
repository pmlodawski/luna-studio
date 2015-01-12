---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleInstances#-}

module Luna.Syntax.Label where

import           Flowbox.Prelude hiding (element)
import           GHC.Generics (Generic)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Label l a = Label { _label :: l, _element :: a } deriving (Generic, Functor)

makeLenses ''Label


instance (Show l, Show a) => Show (Label l a) where
    show (Label l a) = "L " ++ show l ++ " " ++ show a


instance Default l => Wrap (Label l) where
	wrap   = Label def

instance Unwrap (Label l) where
	unwrap = view element

