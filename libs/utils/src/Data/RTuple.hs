{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.RTuple where

import Prelude hiding (head, tail, map)
import Data.Typeable
import GHC.TypeLits
import Data.Wrapper 
import Unsafe.Coerce (unsafeCoerce)

----------------------------------------------------------------------------------
-- RTuple
----------------------------------------------------------------------------------

data RTuple a = RTuple { fromRTuple :: a } deriving (Eq, Ord)

infixr 0 //
(//) = (,)

-- === Wrapper ===

instance Wrap RTuple where
    wrap = RTuple

instance Unwrap RTuple where
    unwrap (RTuple a) = a

-- === Show ===

class ShowRtup r where
    showRtup :: String -> r -> String

instance ShowRtup () where
    showRtup _ _ = ""

instance Show a => ShowRtup (a,()) where
    showRtup _ (a,()) = show a

instance (Show a, ShowRtup as) => ShowRtup (a,as) where
    showRtup sep (a,as) = show a ++ sep ++ showRtup sep as

instance ShowRtup a => Show (RTuple a) where
    show (RTuple a) = "RTuple (" ++ showRtup ", " a ++ ")"

-- === GetEl ===

class GetEl (n :: Nat) t el | n t -> el where
    getEl :: Proxy n -> t -> el

instance (t~(x,xs), el~x) => GetEl 0 t el where
    getEl _ (x,_) = x

instance (t~(x,xs), GetEl (n-1) xs el) => GetEl n t el where
    getEl n (_,xs) = getEl (Proxy :: Proxy (n-1)) xs

instance GetEl n a el => GetEl n (RTuple a) el where
    getEl n (RTuple a) = getEl n a

-- === Head ===

class Head t el | t -> el where
    head :: t -> el

instance Head (a,as) a where
    head (a,_) = a

instance Head a el => Head (RTuple a) el where
    head (RTuple a) = head a

-- === Tail ===

class Tail t t' | t -> t' where
    tail :: t -> t'

instance Tail (a,as) as where
    tail (_,as) = as

instance Tail t t' => Tail (RTuple t) t' where
    tail (RTuple a) = tail a


-- === UncurryTuple ===

-- |converts function taking a tuple list as argument into standard haskell one
--  eg. `(a,(b,(c,()))) -> out` into `a -> b -> c -> out`
class UncurryTuple f out | f -> out where
    uncurryTuple :: f -> out

instance UncurryTuple (RTuple () -> a) a where
    uncurryTuple f = f $ RTuple ()

--instance UncurryTuple (() -> a) a where
--    uncurryTuple f = f $  ()

instance UncurryTuple (RTuple xs -> f) fout => UncurryTuple (RTuple (x,xs) -> f) (x -> fout) where
    uncurryTuple f = (\x -> uncurryTuple $ f . RTuple . (x,) . fromRTuple)


-- === ToList ===

class ToTuple a lst | a -> lst where
    toTuple :: a -> lst

instance ToTuple (RTuple ()) () where toTuple _ = ()
instance ToTuple (RTuple (t1,())) t1 where toTuple (RTuple (t1,())) = t1
instance ToTuple (RTuple (t1,(t2,()))) (t1,t2) where toTuple (RTuple (t1,(t2,()))) = (t1,t2)
instance ToTuple (RTuple (t1,(t2,(t3,())))) (t1,t2,t3) where toTuple (RTuple (t1,(t2,(t3,())))) = (t1,t2,t3)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,()))))) (t1,t2,t3,t4) where toTuple (RTuple (t1,(t2,(t3,(t4,()))))) = (t1,t2,t3,t4)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,())))))) (t1,t2,t3,t4,t5) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,())))))) = (t1,t2,t3,t4,t5)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))) (t1,t2,t3,t4,t5,t6) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))) = (t1,t2,t3,t4,t5,t6)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))) (t1,t2,t3,t4,t5,t6,t7) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))) = (t1,t2,t3,t4,t5,t6,t7)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))) (t1,t2,t3,t4,t5,t6,t7,t8) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,())))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,())))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)
instance ToTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,(t12,()))))))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) where toTuple (RTuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,(t11,(t12,()))))))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12)



