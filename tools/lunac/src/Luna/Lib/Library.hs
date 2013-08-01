---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Lib.Library(
    Library(..),
    ID,
    empty
) where

import qualified Luna.System.UniPath as UniPath
import           Luna.System.UniPath   (UniPath)

data Library =  Library{
    name          :: String,
    --version :: Version,
    path          :: UniPath,
    rootNodeDefID :: Int
} deriving (Show)

empty :: Library
empty = Library "" UniPath.empty (-1)

type ID  = Int
