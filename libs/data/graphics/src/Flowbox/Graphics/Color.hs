---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Flowbox.Graphics.Color (
    module Flowbox.Graphics.Color,
    module X
) where
	
import           Flowbox.Graphics.Color.Conversion        as X
import           Flowbox.Graphics.Color.Internal          as X
import           Flowbox.Graphics.Color.CMY               as X
import           Flowbox.Graphics.Color.CMY.Conversion    as X
import           Flowbox.Graphics.Color.CMYK              as X
import           Flowbox.Graphics.Color.CMYK.Conversion   as X
import           Flowbox.Graphics.Color.HSL               as X
import           Flowbox.Graphics.Color.HSL.Conversion    as X
import           Flowbox.Graphics.Color.HSV               as X
import           Flowbox.Graphics.Color.HSV.Conversion    as X
import           Flowbox.Graphics.Color.RGB               as X
import           Flowbox.Graphics.Color.RGB.Conversion    as X
import           Flowbox.Graphics.Color.RGBA              as X
import           Flowbox.Graphics.Color.RGBA.Conversion   as X
import           Flowbox.Graphics.Color.YUV               as X
import           Flowbox.Graphics.Color.YUV.Conversion    as X
import           Flowbox.Graphics.Color.YUV_HD            as X
import           Flowbox.Graphics.Color.YUV_HD.Conversion as X

-- TODO: LAB color space
