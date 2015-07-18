{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Luna.Inference.Instance where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Repr
import Luna.Inference.RawData




data Instance t = Instance { _tp  :: t
                           , _raw :: RawData
                           }

makeLenses ''Instance


instance ToRawData (Instance t) where
    toRawData = view raw


