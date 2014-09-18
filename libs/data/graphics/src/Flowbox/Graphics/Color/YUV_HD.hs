---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.Graphics.Color.YUV_HD where

import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)
import Flowbox.Graphics.Utils.Accelerate



data YUV_HD a = YUV_HD { yuv_hdY :: a, yuv_hdU :: a, yuv_hdV :: a }
              deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (YUV_HD a) (YUV_HD a) a a where
    each f (YUV_HD y u v) = YUV_HD <$> f y <*> f u <*> f v
    {-# INLINE each #-}

deriveAccelerate ''YUV_HD
