{-# LANGUAGE   GADTs #-} 
{-# LANGUAGE   DeriveDataTypeable #-} 
{-# LANGUAGE   ScopedTypeVariables #-} 



module Data.HTSet.HTSet where

import Prelude hiding (lookup)
import qualified Data.HashMap.Lazy as M
import Data.HashMap.Lazy(HashMap)
import Unsafe.Coerce
import Data.Typeable
import System.Mem.Weak (Weak, mkWeak, deRefWeak)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)
import Control.Monad (liftM)
import Data.Type.Hide (HideType(HideType), unsafeFromHideType)
import Data.Monoid


data Key a = Key deriving Show

newtype HTSet = HTSet (HashMap TypeRep (Weak HideType)) 


lookup set = lookupKey Key set

lookupKey :: Typeable a => Key a -> HTSet -> Maybe a
lookupKey (Key :: Key a) (HTSet m) =  (fmap getVal (M.lookup key m) :: Maybe a) where
  key = typeOf (undefined :: a)
  -- we know it is alive, how else did we get the key?
  getVal v = (keepAlive key unsafeFromHideType) -- keep key alive till unsafeFromHideType 
             (unsafePerformIO (liftM fromJust (deRefWeak v)))

  -- this function keeps the key k alive until computing whnf of application of f to x
  keepAlive :: x -> (b -> c) -> (b -> c)
  keepAlive k f x = k `seq` (f x)


insertKey :: Typeable a => Key a -> a -> HTSet -> HTSet
insertKey _ = insert

insert :: Typeable a => a -> HTSet -> HTSet
insert a (HTSet m) = v `seq` HTSet (M.insert key v m) 
    where key = typeOf a
          v   = unsafeMKWeak key (HideType a)
{- NOINLINE unsafeMKWeak -}
unsafeMKWeak k a = unsafePerformIO $ mkWeak k a Nothing
{-# INLINABLE insert #-}



instance Monoid HTSet where
  mempty                      = HTSet M.empty
  mappend (HTSet a) (HTSet b) = HTSet $ M.union a b


data A a = A a deriving (Typeable, Show)
data B = B Int deriving (Typeable, Show)
data C = C Int deriving (Typeable, Show)

kA = Key :: Key (A Int)
kB = Key :: Key (A Float)
kC = Key :: Key C

main = do
    let m = HTSet
        m2 = insertKey kA (A 10) 
           $ insert (A (11::Float)) 
           $ insertKey kC (C 12) 
           $ mempty

    print $ (lookup m2 :: Maybe (A Float))
    print "end"