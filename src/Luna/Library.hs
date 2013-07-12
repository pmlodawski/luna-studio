---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Library(
Library(..),
LibID
) where

import System.UniPath (UniPath)

type LibID = Int

data Library =  Library{
	path :: UniPath
} deriving (Show)