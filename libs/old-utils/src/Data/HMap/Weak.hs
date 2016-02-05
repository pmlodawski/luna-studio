{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}



module Data.HMap.Weak where

--import Prelude hiding (lookup)
--import qualified Data.HashMap.Lazy as M
--import Data.HashMap.Lazy(HashMap)
--import Unsafe.Coerce
--import Data.Typeable
--import System.Mem.Weak (Weak, mkWeak, deRefWeak)
--import System.IO.Unsafe (unsafePerformIO)
--import Data.Maybe (fromJust)
--import Control.Monad (liftM)
--import Data.Type.Hide (HideType(HideType), unsafeFromHideType)
--import Data.Monoid
--import Data.Default


--data Key a = Key deriving Show

--class IsKey a b | a -> b where
--    toKey :: a -> Key b

--    default toKey :: a -> Key b
--    toKey = const Key


--instance IsKey (Key a) a where
--    toKey = id

--newtype HMap = HMap (HashMap TypeRep (Weak HideType))


--lookup set = lookupKey Key set

--lookupKey :: (IsKey k a, Typeable a) => k -> HMap -> Maybe a
--lookupKey (toKey -> Key :: Key a) (HMap m) =  (fmap getVal (M.lookup key m) :: Maybe a) where
--  key = typeOf (undefined :: a)
--  -- we know it is alive, how else did we get the key?
--  getVal v = (keepAlive key unsafeFromHideType) -- keep key alive till unsafeFromHideType
--             (unsafePerformIO (liftM fromJust (deRefWeak v)))

--  -- this function keeps the key k alive until computing whnf of application of f to x
--  keepAlive :: x -> (b -> c) -> (b -> c)
--  keepAlive k f x = k `seq` (f x)


--insertKey :: (IsKey k a, Typeable a) => k -> a -> HMap -> HMap
--insertKey _ = insert

--insert :: Typeable a => a -> HMap -> HMap
--insert a (HMap m) = v `seq` HMap (M.insert key v m)
--    where key = typeOf a
--          v   = unsafeMKWeak key (HideType a)
--{- NOINLINE unsafeMKWeak -}
--unsafeMKWeak k a = unsafePerformIO $ mkWeak k a Nothing
--{-# INLINABLE insert #-}



--instance Monoid HMap where
--  mempty                      = HMap M.empty
--  mappend (HMap a) (HMap b) = HMap $ M.union a b

--instance Default HMap where
--  def = mempty


----data A a = A a deriving (Typeable, Show)
----data B = B Int deriving (Typeable, Show)
----data C = C Int deriving (Typeable, Show)

----kA = Key :: Key (A Int)
----kB = Key :: Key (A Float)
----kC = Key :: Key C

----main = do
----    let m = HMap
----        m2 = insertKey kA (A 10)
----           $ insert (A (11::Float))
----           $ insertKey kC (C 12)
----           $ mempty

----    print $ (lookup m2 :: Maybe (A Float))
----    print "end"
