---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Syntax.Foreign where

import           Flowbox.Prelude

import           Data.Binary     (Binary)
import qualified Prelude



data Foreign a = Foreign Target a
               deriving (Show, Generic, Functor, Traversable, Foldable, Eq, Read)


data Target = Haskell
            | CPP
            deriving (Show, Generic, Eq, Read)

instance Binary a => Binary (Foreign a)
instance Binary Target
