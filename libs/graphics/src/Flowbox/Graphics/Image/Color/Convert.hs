---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.Graphics.Image.Color.Convert (
    module Flowbox.Graphics.Image.Color.Convert,
    module X
) where

import           Data.Map                          (Map)
import qualified Data.Array.Accelerate as A

--import           Flowbox.Graphics.Color           (Color)
import qualified Flowbox.Graphics.Color                    as Color
import           Flowbox.Graphics.Image                    (Image)
import qualified Flowbox.Graphics.Image                    as Image
import qualified Flowbox.Graphics.Image.Channel            as Channel
import           Flowbox.Graphics.Image.Color.Convert.RGB  as X
import qualified Flowbox.Graphics.Utils                    as U
import           Flowbox.Prelude
