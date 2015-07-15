{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Luna.Inference.Instance where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Repr
import Luna.Inference.Data (Data)
import qualified Luna.Inference.Data as Data



data Instance t = Instance { _tp  :: t
                           , _val :: Data
                           }

makeLenses ''Instance


