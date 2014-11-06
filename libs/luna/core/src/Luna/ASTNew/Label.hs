---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleInstances#-}

module Luna.ASTNew.Label where

import           Flowbox.Prelude
import           GHC.Generics (Generic)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Label l a = Label { _label :: l, _element :: a } deriving (Eq, Generic, Read, Functor)

makeLenses ''Label

instance (Show l, Show a) => Show (Label l a) where
    show (Label l a) = "L " ++ show l ++ " " ++ show a


