---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Graphics.Image.View.RGB (
    RGB
) where

import           Flowbox.Data.Channel                 as ChanTree
import           Flowbox.Graphics.Image.View.Internal (View)
import qualified Flowbox.Graphics.Image.View.Internal as View
import           Flowbox.Prelude



data RGB = RGB { name'     :: View.Name
               , channels' :: View.ChanTree
               }
         deriving (Show)
--makeLenses ''RGB

instance View RGB where
    name     = name'
    channels = channels'
    empty    = flip RGB ChanTree.empty
    set t (RGB name _) = RGB name t
