{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP, EmptyDataDecls, ScopedTypeVariables, FlexibleInstances, OverlappingInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
{-# LANGUAGE IncoherentInstances #-}


{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Utils.MonoType where


import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

import Data.PolyTypeable


import Unsafe.Coerce (unsafeCoerce)




data TVar deriving (Typeable)
data TCon0 c deriving (Typeable)
data TCon1 c a1 deriving (Typeable)
data TCon2 c a1 a2 deriving (Typeable)
data TCon3 c a1 a2 a3 deriving (Typeable)
data TCon4 c a1 a2 a3 a4 deriving (Typeable)
data TCon5 c a1 a2 a3 a4 a5 deriving (Typeable)

class Analyze a b | a -> b

monoType :: Analyze a b => a -> b
monoType = unsafeCoerce

data W a

#define BASE(t) instance (r ~ t)  => Analyze t r; instance (r ~ t)  => Analyze (W t) r
BASE(())
BASE(Bool)
BASE(Char)
BASE(Ordering)
BASE(Int)
BASE(Integer)
BASE(Float)
BASE(Double)

instance (Analyze (W a1) ra1, r ~ c ra1) => Analyze (W (c a1)) r
instance (Analyze (W a1) ra1, r ~ c ra1) => Analyze    (c a1)  r

instance (Analyze (W a1) ra1, Analyze (W a2) ra2, r ~ c ra1 ra2) => Analyze (W (c a1 a2)) r
instance (Analyze (W a1) ra1, Analyze (W a2) ra2, r ~ c ra1 ra2) => Analyze    (c a1 a2)  r

instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, r ~ c ra1 ra2 ra3) => Analyze (W (c a1 a2 a3)) r
instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, r ~ c ra1 ra2 ra3) => Analyze    (c a1 a2 a3)  r

instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, Analyze (W a4) ra4, r ~ c ra1 ra2 ra3 ra4) => Analyze (W (c a1 a2 a3 a4)) r
instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, Analyze (W a4) ra4, r ~ c ra1 ra2 ra3 ra4) => Analyze    (c a1 a2 a3 a4)  r

instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, Analyze (W a4) ra4, Analyze (W a5) ra5, r ~ c ra1 ra2 ra3 ra4 ra5) => Analyze (W (c a1 a2 a3 a4 a5)) r
instance (Analyze (W a1) ra1, Analyze (W a2) ra2, Analyze (W a3) ra3, Analyze (W a4) ra4, Analyze (W a5) ra5, r ~ c ra1 ra2 ra3 ra4 ra5) => Analyze    (c a1 a2 a3 a4 a5)  r

instance (r ~ TVar) => Analyze (W a) r





--class ExtractType a b | a -> b where
--    extractType :: a -> b

--instance ExtractType TVar a where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, r~c u1) => ExtractType (c t1) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, r~c u1 u2) => ExtractType (c t1 t2) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, r~c u1 u2 u3) => ExtractType (c t1 t2 t3) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, ExtractType t4 u4, r~c u1 u2 u3 u4) => ExtractType (c t1 t2 t3 t4) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, ExtractType t4 u4, ExtractType t5 u5, r~c u1 u2 u3 u4 u5) => ExtractType (c t1 t2 t3 t4 t5) r where
--    extractType = unsafeCoerce


--instance (a~b) => ExtractType a b where
--    extractType = id

--class Foo a b | a -> b where
--    foo :: a -> b

--instance Foo [a] [a] where
--    foo = id

--tst v = foo $ extractType' v

--tst2 v = extractType' v
--tst3 v = monoType v

----tstDo = do
--extractType' :: ExtractType a b => a -> b
--extractType' = extractType

----main = do
----    print $ (tst ["ala"]) ++ ["!"]
----    print $ (tst [5::Int]) ++ [7]
----    print $ (tst (monoType [])) ++ [7]
----    --print $ extractType [1::Int]

----    return ()

