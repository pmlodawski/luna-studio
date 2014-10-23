---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Arg where

import           Flowbox.Prelude

import GHC.Generics     (Generic)
import Luna.ASTNew.Name (Named)
import Luna.ASTNew.Pat  (RPat)
import Luna.ASTNew.Name (VName)



data PatVal f a = PatVal { _pat :: RPat f, _value :: Maybe a }

type Arg f a  = Named (PatVal f a) VName
type RArg f a = f (Arg f a)
