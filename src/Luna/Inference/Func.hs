{-# LANGUAGE UndecidableInstances #-}
module Luna.Inference.Type where

import Flowbox.Prelude
import Luna.Inference.Type

import qualified Luna.Inference.Class    as Class


arg :: Text -> Arg
arg = flip Arg mempty . Just

