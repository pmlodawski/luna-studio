---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Builder.Poly where

import           Flowbox.Prelude        hiding (simple)

import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as Text
import           Data.Text.Lazy.Builder (fromLazyText, singleton, toLazyText)
import qualified Data.Text.Lazy.Builder as Text
import           Prelude                ()


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class ToTextBuilder a where
    toTextBuilder :: a -> Text.Builder

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

polyCons :: (ToTextBuilder a, ToTextBuilder b) => a -> b -> Text.Builder
polyCons a b = toTextBuilder a <> toTextBuilder b

infixr 6 <:>
(<:>) :: (ToTextBuilder a, ToTextBuilder b) => a -> b -> Text.Builder
(<:>) = polyCons


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ToTextBuilder Text where
    toTextBuilder = fromLazyText

instance ToTextBuilder String where
    toTextBuilder = fromString

instance ToTextBuilder Char where
    toTextBuilder = singleton

instance ToTextBuilder Text.Builder where
    toTextBuilder = id

instance Show a => ToTextBuilder a where
    toTextBuilder = fromString . show
