---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Renderer.Proto.FrameRange where

import           Flowbox.Data.Convert
import qualified Generated.Proto.Renderer.FrameRange as Gen
import           Luna.Renderer.Data.FrameRange       (FrameRange (FrameRange))



instance ConvertPure FrameRange Gen.FrameRange where
    encodeP (FrameRange     begin end) = Gen.FrameRange begin end
    decodeP (Gen.FrameRange begin end) = FrameRange     begin end
