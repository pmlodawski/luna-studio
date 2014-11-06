---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Luna.ASTNew.Arg where

import           Flowbox.Prelude

import GHC.Generics     (Generic)
import Luna.ASTNew.Pat  (RPat)
import Luna.ASTNew.Name (VName)



data Arg f a = Arg { _pat :: RPat f, _value :: Maybe a }
type RArg f a = f (Arg f a)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

deriving instance (Show (RPat f), Show a) => Show (Arg f a)