{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Luna.Inference.Data where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


newtype Data = Data { _fromData :: Any }

makeLenses ''Data

unpack :: Data -> a
unpack = unsafeCoerce . view fromData

--class Pack a where
--    pack :: a -> Data

--instance Pack a where
--    pack = Data . unsafeCoerce

--instance Pack Data where
--    pack = id

class Value v where
    fromValue :: a -> v
    toValue   :: v -> a

instance Value Data where
    fromValue = Data . unsafeCoerce
    toValue   = unsafeCoerce . view fromData
