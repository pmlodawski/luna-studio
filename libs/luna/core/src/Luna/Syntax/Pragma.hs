---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE OverlappingInstances #-}

module Luna.Syntax.Pragma where

import Flowbox.Prelude hiding (Cons, traverse)


data Pragma = Enable  Text
            | Disable Text
            | Push    Text
            | Pop     Text
            deriving (Show, Generic, Eq, Read)


