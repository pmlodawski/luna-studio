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

module Flowbox.Graphics.Color.YUV where

import Data.Foldable                     (Foldable)
import Data.Typeable

import Flowbox.Prelude hiding (lift)
import Flowbox.Graphics.Utils.Accelerate



data YUV a = YUV { yuvY :: a, yuvU :: a, yuvV :: a }
           deriving (Foldable, Functor, Traversable, Typeable, Show)

deriveAccelerate ''YUV
deriveEach ''YUV
