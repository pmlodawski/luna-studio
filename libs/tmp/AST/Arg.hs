---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Luna.AST.Arg where

import GHC.Generics (Generic)

import Flowbox.Generics.Deriving.QShow
import Flowbox.Prelude                 hiding (Traversal, cons, drop, id)
import Luna.AST.Common                 (ID)



data Arg a = Named   { _id :: ID, _name :: String, _arg :: a }
           | Unnamed { _id :: ID, _arg :: a                  }
           deriving (Show, Eq, Generic, Read)


instance QShow a => QShow (Arg a)
makeLenses (''Arg)
