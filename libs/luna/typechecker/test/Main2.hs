
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings #-}

--{-# LANGUAGE PolyKinds #-}

module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, when, set)
import Data.Vector as V
import Data.TypeLevel.Bool
import Data.Typeable
import GHC.Prim (Any)
import Control.Monad.State

--data Result = Result deriving (Show)
--data Ixed = Ixed deriving (Show)
--data Ixed2 = Ixed2 deriving (Show)
--data MX = MX

--data Bundle  a b = Bundle a b deriving (Show)
--data Bundle' a b = Bundle' a b deriving (Show)

--b = flip Bundle undefined

--class Foom mods where
--    foom :: mods -> mods


--t1 = foom (b Result, (b Ixed, (b MX, ())))


--class SetMod mod query where setMod :: mod -> (ModVal mod query) -> query -> query
--instance {-# OVERLAPPABLE #-}                                                                    SetMod mod (Bundle mod  a, qs) where setMod mod v (_, qs) = (Bundle mod v,qs)
--instance {-# OVERLAPPABLE #-} (SetMod mod qs, ModVal mod qs ~ ModVal mod (Bundle mod' a, qs)) => SetMod mod (Bundle mod' a, qs) where setMod mod v (q, qs) = (q, setMod mod v qs)


--class GetMod mod query where getMod :: mod -> query -> ModVal mod query
--instance {-# OVERLAPPABLE #-}                                                                    GetMod mod (Bundle mod  a, qs) where getMod _ (Bundle _ a, _ ) = a
--instance {-# OVERLAPPABLE #-} (GetMod mod qs, ModVal mod (Bundle mod' a, qs) ~ ModVal mod qs) => GetMod mod (Bundle mod' a, qs) where getMod m (_         , qs) = getMod m qs

--type family ContainsMod mod query where ContainsMod mod (Bundle mod  a, qs) = True
--                                        ContainsMod mod (Bundle mod' a, qs) = ContainsMod mod qs
--                                        ContainsMod mod (Bundle' mod a, qs) = False
--                                        ContainsMod mod (Bundle' mod' a, qs) = ContainsMod mod qs
--                                        ContainsMod mod ()                  = False

--type family ModVal mod query where ModVal mod (Bundle mod  a, qs) = a
--                                   ModVal mod (Bundle mod' a, qs) = ModVal mod qs
--                                   ModVal mod (Bundle' mod  a, qs) = a
--                                   ModVal mod (Bundle' mod' a, qs) = ModVal mod qs
--                                   --ModVal mod ()                  = Any



----class IfT (cond :: Bool) a b where ifT :: Proxy cond -> a -> b -> If cond a b
----instance IfT True  a b where ifT _ = const
----instance IfT False a b where ifT _ = flip const

--class IfT (cond :: Bool) where ifT :: Proxy cond -> a -> a -> a
--instance IfT True  where ifT _ = const
--instance IfT False where ifT _ = flip const

----ifT' :: Proxy (cond :: Bool) -> a -> a -> a
----ifT' = ifT
----lookupMod

--with mod f q = setMod mod (f $ getMod mod q) q

--type HasMod     mod q = (GetMod mod q, SetMod mod q)
--type Mod      mod q r = (HasMod mod q, IfT (ContainsMod mod q), ModVal mod q ~ r)

----when (mod :: mod) (q :: q) f = ifT (Proxy :: Proxy (ContainsMod mod q)) f id
--when (mod :: mod) f (q :: q) = ifT (Proxy :: Proxy (ContainsMod mod q)) f id q
--set = setMod
--get = getMod

--result :: GetMod Result query => query -> ModVal Result query
--result = get Result

----test :: (Mod Result q (Vector a), Mod Ixed q Int) => a -> q -> q
--test :: (Num (ModVal Ixed r), IfT (ContainsMod Ixed r), GetMod Result r, SetMod Ixed r, SetMod Result r, ModVal Result r ~ Vector a) => a -> r -> r
--test a q = when Ixed (set Ixed 0) $ with Result (flip V.snoc a) q
----test a q = when Ixed (set Ixed $ V.length $ result q) $ with Result (flip V.snoc a) q
----test a (q :: q) = with R (flip V.snoc a) q --  ifT (Proxy :: Proxy (HasMod Ixed q)) (setMod (Bundle Ixed 1) q) q

----test a (q :: q) = setMod R (V.snoc (getMod R q) a) q --  ifT (Proxy :: Proxy (HasMod Ixed q)) (setMod (Bundle Ixed 1) q) q
----class Insertable el t where insert :: el -> t -> t

--c = mempty :: Vector Int

--main = do
--    let x = test (5 :: Int) (Bundle Result c, (Bundle' Ixed undefined, ())) :: _
--    --print $ test 5 (Bundle Result c, ())
--    return ()





--data Weak = Weak (q -> IO ())


data Ixed   = Ixed   deriving (Show)
data Result = Result deriving (Show)


newtype RTup a = RTup { fromRTup :: a } deriving (Functor, Traversable, Foldable)


class RTupShow t where rtupShow :: RTup t -> String
instance RTupShow () where rtupShow _ = ">"
instance (Show a, RTupShow as) => RTupShow (a,as) where rtupShow (split -> (a,ts)) = ", " <> show a <> rtupShow ts

instance                          Show (RTup ())     where show _ = "<>"
instance (Show a, RTupShow as) => Show (RTup (a,as)) where show (split -> (a,ts)) = "<" <> show a <> rtupShow ts

prepend a (RTup t) = RTup (a,t)





type family Appended a t
type instance Appended a (RTup ())     = RTup (a,())
type instance Appended a (RTup (t,ts)) = RTup (t, Appended a ts)

--type family Prepended a t
--type instance Prepended


--class Append a t where append :: a -> RTup t -> Appended a t
--instance Append a (RTup ()) where append a _ = RTup (a,())
--instance Append a (RTup (t,ts)) where append a (RTup (t,ts)) = RTup (t, fromRTup $ append a $ RTup ts)


split (RTup (t,ts)) = (t, RTup ts)

--main = do
--  print $ RTup (1,(2,(3,())))

type family RT t where RT '[]       = ()
                       RT (t ': ts) = (t, RT ts)

data ModRule mod m a = ModRule mod (m a) deriving (Show)

newtype ModReceipt t = ModReceipt { fromModReceipt :: RTup t }

type Receipt t = ModReceipt (RT t)

deriving instance Show (RTup t) => Show (ModReceipt t)

addRule mod rule (ModReceipt t) = ModReceipt $ prepend (ModRule mod rule) t

instance t ~ () => Default (RTup t)       where def = RTup ()
instance t ~ () => Default (ModReceipt t) where def = ModReceipt def


--appendV :: Monad m => a -> Vector a -> Receipt '[ModRule Result m (Vector a), ModRule Ixed m Int]
appendV el v = addRule Result (return $ V.snoc v el)
             $ addRule Ixed   (return $ V.length v )
             $ def


--tst :: (Monad m, Num a) => Receipt '[ModRule Result m (Vector a), ModRule Ixed m Int]
--tst :: _ => _
tst = appendV 5 mempty

--tst2 :: _ => _
--tst2 = fst . fromRTup . fromModReceipt $ tst

main = do
  print "end"