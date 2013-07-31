---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Path (
    module Luna.Network.Path.Path,
    toModulePath,
    toModulePaths
)where

import           Luna.Network.Path.Path 
import           Data.Char                      (isLower)
import           Data.String.Utils                 (join)


toModulePath :: Path -> String
toModulePath path = join "." (toModulePaths path)


toModulePaths :: Path -> [String]
toModulePaths path = case path of
    Path []                -> []
    Path [segment@(x:xs)]  -> [name] where
                                  name = if isLower x
                                      then "U'" ++ segment
                                      else segment
    Path (x:xs)            -> toModulePaths (Path [x]) ++ toModulePaths (Path xs)


