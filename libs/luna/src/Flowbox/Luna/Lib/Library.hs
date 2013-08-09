---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.Library(
    Library(..),
    ID,
    empty
) where

import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)

data Library =  Library{
    name          :: String,
    --version :: Version,
    path          :: UniPath,
    rootDefID     :: Int
} deriving (Show)

empty :: Library
empty = Library "" UniPath.empty (-1)

type ID  = Int
