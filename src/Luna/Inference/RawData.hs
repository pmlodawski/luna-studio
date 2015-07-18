
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}


module Luna.Inference.RawData where

import Flowbox.Prelude
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)


newtype RawData = RawData Any

instance Show RawData where
    show _ = "RawData"


unpackRawData :: ToRawData a => a -> d
unpackRawData (toRawData -> RawData v) = unsafeCoerce v

packRawData :: FromRawData a => d -> a
packRawData = fromRawData . RawData . unsafeCoerce

--class Pack a where
--    pack :: a -> Data

--instance Pack a where
--    pack = Data . unsafeCoerce

--instance Pack Data where
--    pack = id

asLambda :: RawData -> (RawData -> RawData)
asLambda = unpackRawData

class FromRawData a where
    fromRawData :: RawData -> a

class ToRawData a where
    toRawData :: a -> RawData

--class IsRawData a where
--    fromRawData :: RawData -> a
--    toRawData   :: a -> RawData

--class (FromRawData a, ToRawData a) => IsRawData a

instance FromRawData RawData where
    fromRawData = id

instance ToRawData RawData where
    toRawData   = id


