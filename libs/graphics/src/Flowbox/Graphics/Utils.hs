module Flowbox.Graphics.Utils where

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Flowbox.Prelude       as P

nonIntRem :: (A.Elt e, A.IsFloating e) => Exp e -> Exp e -> Exp e
nonIntRem x y = x - (y * (A.fromIntegral $ (A.truncate (x / y) :: Exp Int)))
