module NodeEditor.View.Color where

import           Common.Prelude
import           Data.Convert          (Convertible)
import qualified Data.Color            as Color
import           NodeEditor.Data.Color (Color)


type RGB = (Float, Float, Float)

instance Convertible Color RGB where
    convert = fromRGB . convert
        where fromRGB (Color.RGB r g b _) = (r/256, g/256, b/256)
