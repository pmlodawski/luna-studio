---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Lib.Library(
Library(..),
LibID,
LibNode(..)
) where

import Luna.System.UniPath (UniPath)


data Library =  Library{
	path :: UniPath
} deriving (Show)


type LibID   = Int
type LibNode = (LibID, Library)
