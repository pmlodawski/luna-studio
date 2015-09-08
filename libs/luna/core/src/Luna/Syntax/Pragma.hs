---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverlappingInstances #-}

module Luna.Syntax.Pragma where

import Data.Binary (Binary)

import Flowbox.Prelude hiding (Cons, traverse)


data Pragma = Enable  Text
            | Disable Text
            | Push    Text
            | Pop     Text
            deriving (Show, Generic, Eq, Read)

instance Binary Pragma
