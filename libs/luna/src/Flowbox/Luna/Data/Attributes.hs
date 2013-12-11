---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Attributes (
    Attributes,
    module Data.Map,

    get,
    set,
) where

import           Data.Map
import qualified Data.Map        as Map
import           Flowbox.Prelude hiding (set)



type Attributes = Map String (Map String String)


get :: String -> String -> Attributes -> Maybe String
get userKey key attrs = Map.lookup userKey attrs >>= Map.lookup key


set :: String -> String -> String -> Attributes -> Attributes
set userKey key value attrs = Map.insert userKey userAttrs attrs where
    userAttrs = case Map.lookup userKey attrs of
                    Nothing        -> Map.fromList [(key, value)]
                    Just userAttrs -> Map.insert     key  value userAttrs
