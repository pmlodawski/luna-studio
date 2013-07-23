---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Path.Import(
    Import(..)
) where

import           Luna.Network.Path.Path            (Path)

data Import = Import {path :: Path, items :: [String]} deriving (Show)