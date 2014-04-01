---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Graphics.Image.Composition where

import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Prelude                as P

data Premultiply = Premultiply {name :: Channel.Name, invert :: Bool}
                 | Unpremultiply {name:: Channel.Name, invert :: Bool}

-- TODO: finish this data type
data Mask = Mask
