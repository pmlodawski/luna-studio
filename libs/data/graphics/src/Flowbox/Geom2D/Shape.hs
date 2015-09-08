---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Geom2D.Shape where

import Flowbox.Geom2D.Path
import Flowbox.Prelude

data Shape a = Shape { pathList :: [Path a] } deriving (Eq, Ord, Show)

instance Functor Shape where
    fmap f (Shape points) = Shape ((fmap.fmap) f points)

--instance Applicative Shape where
--    pure a = Shape (pure a)
--    {-# INLINE pure #-}
--    Shape a <*> Shape b = Shape (a <*> b)
--    {-# INLINE (<*>) #-}
