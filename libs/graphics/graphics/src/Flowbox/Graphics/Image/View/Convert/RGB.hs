---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.Convert.RGB where

import qualified Flowbox.Graphics.Image.Error as Image
import           Flowbox.Graphics.Image.View.RGB
import           Flowbox.Graphics.Image.View.Internal
import           Flowbox.Prelude



class View view => ViewRGB view where
    toRGB :: view -> Image.Result RGB

instance ViewRGB RGB where
    toRGB = return . id
