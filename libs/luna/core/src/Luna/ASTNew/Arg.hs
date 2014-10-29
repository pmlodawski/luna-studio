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
import Luna.ASTNew.Pat  (LPat)
import Luna.ASTNew.Name (VName)
import Luna.ASTNew.Label (Label)



data Arg  a v = Arg { _pat :: LPat a, _value :: Maybe v } deriving Show
type LArg a v = Label a (Arg a v)


