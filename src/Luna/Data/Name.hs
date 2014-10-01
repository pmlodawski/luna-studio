---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Data.Name where


import GHC.Generics        (Generic)

import           Flowbox.Prelude
import qualified Data.Map        as Map
import           Data.Map        (Map)
import           Flowbox.Generics.Deriving.QShow


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Name = Single { _base :: String                        }
          | Multi  { _base :: String, _segments :: [String] }
          deriving (Show, Eq, Generic, Read)

makeLenses ''Name

instance QShow Name