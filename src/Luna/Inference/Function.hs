{-# LANGUAGE UndecidableInstances #-}
module Luna.Inference.Function where

import Flowbox.Prelude
import qualified Luna.Inference.Type     as Type
import           Luna.Inference.Type     hiding (Function)
import qualified Luna.Inference.Class    as Class
import           Luna.Inference.Instance
import qualified Data.Sequence           as Seq



type Function = Instance Type.Function

arg :: Text -> Arg
arg = flip Arg def . Just

arity :: Type.Function -> Int
arity = Seq.length . view args


-- TODO: przeniesc wszysktie konwersje na funkcje tu