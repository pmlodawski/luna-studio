{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-} -- Used by proxy DataType declaration

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module FlowboxM.Luna.Data (
    module FlowboxM.Luna.Data,
    OneTuple(..),
    Generic
) where


import           FlowboxM.Luna.TH.Inst   
import           Control.Applicative     
import           Data.Tuple.OneTuple     
import           GHC.Generics            (Generic)
import           Data.Typeable           (Typeable)
import           GHC.TypeLits            

-- basic datatypes

data Proxy a = Proxy

class Member (name :: Symbol) cls func | name cls -> func where 
    member :: proxy name -> cls -> func

newtype Pure a = Pure { getPure :: a } deriving(Typeable, Generic)
newtype Func a = Func a deriving(Show, Typeable, Generic)


-- GetX 

class Failure (a :: Symbol)

class Get0 m f |  m -> f where get0 :: m -> f
class Get1 m f |  m -> f where get1 :: m -> f
class Get2 m f |  m -> f where get2 :: m -> f
class Get3 m f |  m -> f where get3 :: m -> f
class Get4 m f |  m -> f where get4 :: m -> f
class Get5 m f |  m -> f where get5 :: m -> f
class Get6 m f |  m -> f where get6 :: m -> f
class Get7 m f |  m -> f where get7 :: m -> f
class Get8 m f |  m -> f where get8 :: m -> f
class Get9 m f |  m -> f where get9 :: m -> f

instance Get0 (Pure Int)    (Pure Int)    where get0 = id
instance Get0 (Pure Double) (Pure Double) where get0 = id
instance Get0 (Pure Float)  (Pure Float)  where get0 = id
instance Get0 (Pure ()) (Pure ()) where get0 = id
instance Get0 (Pure (OneTuple a)) (Pure (OneTuple a)) where get0 = id
instance Get0 (Pure (v1,v2)) (Pure (v1,v2)) where get0 = id
instance Get0 (Pure (v1,v2,v3)) (Pure (v1,v2,v3)) where get0 = id
instance Get0 (Pure (v1,v2,v3,v4)) (Pure (v1,v2,v3,v4)) where get0 = id
instance Get0 (Pure (v1,v2,v3,v4,v5)) (Pure (v1,v2,v3,v4,v5)) where get0 = id


concatPure a                  = concat $ map getPure a
rangeFromTo (Pure a) (Pure b) = Pure $ map Pure $ if a < b then [a..b] else [a,a-1..b]
rangeFrom   (Pure a)          = Pure $ map Pure $ [a..]


-- GetIO helpers

class GetIO f where 
    getIO :: f a -> IO a

instance GetIO Pure where
    getIO (Pure a) = return a

instance GetIO IO where
    getIO = id

