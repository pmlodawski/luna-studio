---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Path (
    module Luna.Network.Path.Path,
    toModulePath
)where

import           Luna.Network.Path.Path 
import           Data.Char                      (isLower)


toModulePath :: Path -> String
toModulePath (Path []) = ""
toModulePath (Path [segment@(x:xs)]) = name where
    name = if isLower x
        then "U'" ++ segment
        else segment
toModulePath (Path (x:xs)) = toModulePath (Path [x]) ++ "." ++ toModulePath (Path xs)
