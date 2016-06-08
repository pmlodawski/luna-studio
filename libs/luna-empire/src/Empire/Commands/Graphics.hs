module Empire.Commands.Graphics where

import           Unsafe.Coerce
import           GHC.Prim                     (Any)

import           Graphics.API

fromGraphics :: Any -> Graphics
fromGraphics = unsafeCoerce

fromLayer :: Any -> Graphics
fromLayer v = Graphics [unsafeCoerce v :: Layer]
