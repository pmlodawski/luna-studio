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

module Flowbox.Graphics.Color.CMY where

import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)
import Flowbox.Graphics.Utils.Accelerate



data CMY a = CMY { cmyC :: a, cmyM :: a, cmyY :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

instance Each (CMY a) (CMY a) a a where
    each f (CMY c m y) = CMY <$> f c <*> f m <*> f y
    {-# INLINE each #-}

deriveAccelerate ''CMY
