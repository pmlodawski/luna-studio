---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.Image where

import Data.Map

import qualified Flowbox.Graphics.Image.View as View
import           Flowbox.Prelude



data Image view = Image { _views       :: Map View.Name view
                        , _defaultView :: View.Select
                        }
makeLenses ''Image
