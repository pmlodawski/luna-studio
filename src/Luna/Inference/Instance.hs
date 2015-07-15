{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Luna.Inference.Instance where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.Repr
import Luna.Inference.Value




data Instance t = Instance { _tp  :: t
                           , _val :: Value
                           }

makeLenses ''Instance


instance ToValue (Instance t) where
    toValue = view val



