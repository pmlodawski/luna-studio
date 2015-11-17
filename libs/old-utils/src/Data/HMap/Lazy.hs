{-# LANGUAGE   GADTs #-} 
{-# LANGUAGE   DeriveDataTypeable #-} 
{-# LANGUAGE   ScopedTypeVariables #-} 
{-# LANGUAGE   ViewPatterns #-} 
{-# LANGUAGE   FunctionalDependencies #-} 
{-# LANGUAGE   StandaloneDeriving #-} 
{-# LANGUAGE   TypeFamilies #-} 
{-# LANGUAGE   UndecidableInstances #-} 
{-# LANGUAGE   NoMonomorphismRestriction #-} 
{-# LANGUAGE   DeriveFunctor #-} 
{-# LANGUAGE   DeriveTraversable #-} 

module Data.HMap.Lazy (
    module Data.HMap.Lazy,
    module X
) where

import           Data.Maps         as X
import           Prelude           hiding (lookup)
import qualified Data.Maps         as Maps
import           Data.Monoid
import           Data.Typeable
import           Data.Default
import           Data.Traversable
import           Data.Map          (Map)
import           Data.IntMap       (IntMap)
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable     (Hashable)
import           Data.Foldable     (Foldable)
import           Data.Type.Hide    (HideType, hideType, revealType, Simple)
import           GHC.Generics      (Generic)

----------------------------------------------------------------------
-- Key
----------------------------------------------------------------------

data Key key val = Key key deriving Show
type IntKey = Key Int

class IsKey a k v | a -> k v where
    toKey :: a -> Key k v

-- == Instances ==

instance IsKey (Key k v) k v where
    toKey = id

instance Default k => Default (Key k v) where
    def = Key def

----------------------------------------------------------------------
-- Hetero wrapper
----------------------------------------------------------------------

newtype H base a = H { unH :: base a } deriving (Show, Monoid, Default, Functor, Foldable, Traversable, Generic)

-- == Utils ==

withBase :: H base a -> (base a -> base' a') -> H base' a'
withBase (H a) f = H $ f a

baseElems :: ValMap (base a) k v => H base a -> [v]
baseElems (H a) = elems a

baseKeys :: ValMap (base a) k v => H base a -> [k]
baseKeys (H a) = keys a

lookupBase :: GenMap (base a) k v => k -> H base a -> Maybe v
lookupBase k (H a) = Maps.lookup k a

-- == Instances ==

instance (IsKey key k v, GenMap (base a) k t, HideType v t) => GenMap (H base a) key v where
        lookup (toKey -> Key k) (H m) = (fmap revealType (Maps.lookup k m) :: Maybe v)
        {-# INLINABLE lookup #-}
        
        insert (toKey -> Key k) a (H m) = v `seq` H (Maps.insert k v m) 
            where v   = hideType a
        {-# INLINABLE insert #-}

----------------------------------------------------------------------
-- Typeable sets
----------------------------------------------------------------------

type HHashMap k = H (HashMap k) Simple
type HMap     k = H (Map     k) Simple
type HIntMap    = H IntMap      Simple

type HTHashMap = HHashMap TypeRep
type HTMap     = HMap     TypeRep

data TypeKey val = TypeKey deriving Show

instance Typeable v => IsKey (TypeKey v) TypeRep v where
    toKey (_ :: TypeKey val) = Key (typeOf (undefined :: val))


--main = do
--    let m = insert kA (A 10)
--          $ insert kB (A 10)
--          $ insert kC (C 10)
--          $ (mempty :: HTMap)

--    print m
--    print $ (lookup kA m)
--    print $ (lookup kB m)
--    print $ (lookup kC m)


--data A a = A a deriving (Typeable, Show)
--data B = B Int deriving (Typeable, Show)
--data C = C Int deriving (Typeable, Show)

--kA = TypeKey :: TypeKey (A Int)
--kB = TypeKey :: TypeKey (A Float)
--kC = TypeKey :: TypeKey C
