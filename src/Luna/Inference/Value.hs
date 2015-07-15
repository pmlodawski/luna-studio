{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}


module Luna.Inference.Value where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


newtype Value = Value Any


unpackRawData :: ToValue a => a -> d
unpackRawData (toValue -> Value v) = unsafeCoerce v

packRawData :: FromValue a => d -> a
packRawData = fromValue . Value . unsafeCoerce

--class Pack a where
--    pack :: a -> Data

--instance Pack a where
--    pack = Data . unsafeCoerce

--instance Pack Data where
--    pack = id

class FromValue a where
    fromValue :: Value -> a

class ToValue a where
    toValue :: a -> Value

--class IsValue a where
--    fromValue :: Value -> a
--    toValue   :: a -> Value

--class (FromValue a, ToValue a) => IsValue a

instance FromValue Value where
    fromValue = id

instance ToValue Value where
    toValue   = id


