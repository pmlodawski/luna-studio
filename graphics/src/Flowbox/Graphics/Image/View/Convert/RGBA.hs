---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Convert.RGBA where

import qualified Flowbox.Graphics.Image.Error as Image
import           Flowbox.Graphics.Image.View.RGBA
import           Flowbox.Graphics.Image.View.Internal
import           Flowbox.Prelude



class View view => ViewRGBA view where
    toRGBA :: view -> Image.Result RGBA

instance ViewRGBA RGBA where
    toRGBA = return
