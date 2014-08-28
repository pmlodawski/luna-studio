---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.RGBA (
    RGBA
) where

import           Flowbox.Data.Channel                 as ChanTree
import           Flowbox.Graphics.Image.View.Internal (View)
import qualified Flowbox.Graphics.Image.View.Internal as View
import           Flowbox.Prelude



data RGBA = RGBA { name'     :: View.Name
                 , channels' :: View.ChanTree
                 }
         deriving (Show)
--makeLenses ''RGB

instance View RGBA where
    name     = name'
    channels = channels'
    empty    = flip RGBA ChanTree.empty
    set t (RGBA name _) = RGBA name t
