---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Flowbox.Graphics.Color (
    module Flowbox.Graphics.Color,
    module X
) where

--import           Data.Array.Accelerate             -- (Exp, Elt(..), Lift, Unlift, Vector, Arrays)
--import qualified Data.Array.Accelerate             as A
--import           Data.Array.Accelerate.Smart
--import           Data.Array.Accelerate.Tuple       -- (IsTuple(..))
--import           Data.Array.Accelerate.Array.Sugar (EltRepr, EltRepr', ArrRepr, ArrRepr')

import           Flowbox.Graphics.Color.Conversion        as X
import           Flowbox.Graphics.Color.Internal          as X
import           Flowbox.Graphics.Color.CMY.Accelerate    as X
import           Flowbox.Graphics.Color.CMY.Conversion    as X
import           Flowbox.Graphics.Color.CMYK.Accelerate   as X
import           Flowbox.Graphics.Color.CMYK.Conversion   as X
import           Flowbox.Graphics.Color.HSL.Accelerate    as X
import           Flowbox.Graphics.Color.HSL.Conversion    as X
import           Flowbox.Graphics.Color.HSV.Accelerate    as X
import           Flowbox.Graphics.Color.HSV.Conversion    as X
import           Flowbox.Graphics.Color.RGB.Accelerate    as X
import           Flowbox.Graphics.Color.RGB.Conversion    as X
import           Flowbox.Graphics.Color.RGBA.Accelerate   as X
import           Flowbox.Graphics.Color.RGBA.Conversion   as X
import           Flowbox.Graphics.Color.YUV.Accelerate    as X
import           Flowbox.Graphics.Color.YUV.Conversion    as X
import           Flowbox.Graphics.Color.YUV_HD.Accelerate as X
import           Flowbox.Graphics.Color.YUV_HD.Conversion as X



-- TODO: LAB color space
