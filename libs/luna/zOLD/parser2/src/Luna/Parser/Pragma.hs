---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable        #-}


module Luna.Parser.Pragma where


import           Data.Typeable

import           Flowbox.Prelude
import           Luna.Pragma.Pragma           (IsPragma)


------------------------------------------------------------------------
-- Data Types
------------------------------------------------------------------------

data TabLength    = TabLength Int deriving (Show, Typeable, Read)
data AllowOrphans = AllowOrphans  deriving (Show, Typeable, Read)
data ImplicitSelf = ImplicitSelf  deriving (Show, Typeable, Read)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance IsPragma TabLength
instance IsPragma AllowOrphans
instance IsPragma ImplicitSelf