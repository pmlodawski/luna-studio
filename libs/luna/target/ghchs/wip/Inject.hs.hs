
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}


!{-# LANGUAGE RightSideContexts #-}
!{-# LANGUAGE Python #-}

import Control.Applicative  
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Typeable

import Data.Default

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Unsafe.Coerce
--import Control.Monad.State

--import Bind2 (bind, bind2, MonadRebase(..), StateT(..), put,get)

--import Data2

import Utils


foo x y = x

data E1 = E1 deriving (Show, Typeable, Eq)
data E2 = E2 deriving (Show, Typeable, Eq)
data E3 = E3 deriving (Show, Typeable, Eq)
data E4 = E4 deriving (Show, Typeable, Eq)
data E5 = E5 deriving (Show, Typeable, Eq)


newtype Safe a = Safe a deriving (Show, Typeable, Eq)

fromSafe (Safe a) = a


instance Functor Safe where
    fmap f (Safe a) = Safe (f a)


instance Monad Safe where
    return = Safe
    (Safe a) >>= f = f a


data UnsafeBase base err val = Value val
                             | Error err
                             | Other (base val)
                             deriving (Show, Eq
#if __GLASGOW_HASKELL__ == 708
                                      , Typeable
#endif
                                      )


--data EitherError base err val = JustError   err
--                              | EitherError (Either base val)
--                              deriving (Show)

type Unsafe = UnsafeBase Safe

data NOP a = NOP a deriving Show

instance Functor (UnsafeBase base err) <= Functor base where
  fmap f a = case a of
      Value a -> Value $ f a
      Error e -> Error e
      Other b -> Other $ fmap f b


instance Monad (UnsafeBase base err) <= (TransMonad base (UnsafeBase base err) (UnsafeBase base err)) where
    return = Value
    v >>= f = v >>=~ f





instance TransMonad (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (TransMonad base (UnsafeBase base err) (UnsafeBase base err)) where
    a >>=~ f = case a of
        Value v -> f v
        Error e -> Error e
        Other o -> o >>=~ f

instance TransMonad Safe (UnsafeBase base err) (UnsafeBase base err) where
    (Safe a) >>=~ f = f a


--instance TransMonad (UnsafeBase base err1) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) <= (TransMonad base (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2)) where
--    ma >>=~ f = case ma of
--        Value a -> f a
--        Error a -> Other $ Error a
--        Other o -> o >>=~ f


--instance TransMonad (UnsafeBase base err1) (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3) (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3) where
--    ma >>=~ f = case ma of
--        Value a -> f a
--        Error a -> Other . Other $ Error a
--        Other o -> o >>=~ f

-- following implementation allows returning monadic values, in special cases. 
-- as far it seems we do not nbeed the implementation, but it is straightforward to create,
-- but we would need to create a lot of instances covering lots of cases, like:
--    instance TransMonad (UnsafeBase base err1) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2)
--    instance TransMonad (UnsafeBase base err1) (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3) (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3)
--    instance TransMonad base (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3) (UnsafeBase(UnsafeBase(UnsafeBase base err1) err2) err3)
--    ...
instance TransMonad (UnsafeBase base err1) a a where
    ma >>=~ f = undefined

--instance TransMonad base (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) where



instance Applicative (UnsafeBase base err) <= (Functor base, (TransApplicative (UnsafeBase base err) base (UnsafeBase base err))) where
    pure = Value
    a <*> b = a <*>~ b


instance TransApplicative Safe Safe Safe where
    (Safe f) <*>~ (Safe b) = Safe $ f b

instance TransApplicative Safe (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative Safe base base) where
    func@(Safe f) <*>~ b = case b of
        Value v -> Value $ f v
        Error e -> Error e
        Other o -> Other $ func <*>~ o


instance TransApplicative (UnsafeBase base err) Safe (UnsafeBase base err) <= (TransApplicative base Safe base)where
    sf <*>~ arg@(Safe b) = case sf of
        Value f -> Value $ f b
        Error e -> Error e
        Other o -> Other $ o <*>~ arg


--instance TransApplicative (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative (UnsafeBase base err) base base) where
--    a <*>~ b = case a of
--        func@(Value f) -> case b of
--            Value v -> Value $ f v
--            Error e -> Error e
--            Other o -> Other $ func <*>~ o


instance TransApplicative (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative (UnsafeBase base err) base (UnsafeBase base err)) where
    a <*>~ b = case a of
        func@(Value f) -> case b of
            Value v -> Value $ f v
            Error e -> Error e
            Other o -> a <*>~ o
        Error e -> Error e
        -- TODO: end me


--instance TransApplicative (UnsafeBase base err) Safe where
--    sf <*>~ (Safe b) = case sf of
--        --Value f -> Value $ f b
--        Error e -> Error e
--        --Other o -> Other $ func <*>~ o



----------------------------------------------------------------------------



class MagicMerge m1 m2 | m1 -> m2 where
    magicMerge :: m1 a -> m2 a


instance MagicMerge (UnsafeBase Safe err) (UnsafeBase Safe err) where
    magicMerge = id

instance MagicMerge (UnsafeBase (UnsafeBase base err) err) (UnsafeBase base err) where
    magicMerge ma = case ma of
        Value a -> Value a
        Error e -> Error e
        Other o -> o

instance MagicMerge (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase dstBase err1) <= (MagicMerge (UnsafeBase base err2) dstBase) where
    magicMerge (ma :: UnsafeBase (UnsafeBase base err1) err2 a) = case ma of
        Value a -> Value a
        Error e -> Other $ magicMerge (Error e :: UnsafeBase base err2 a)
        Other o -> case o of
            Value a  -> Other $ magicMerge (Value a  :: UnsafeBase base err2 a)
            Error e  -> Error e
            Other o' -> Other $ magicMerge (Other o' :: UnsafeBase base err2 a)


printTyped :: (Typeable a, Show a) => a -> IO ()
printTyped x = putStrLn $ show x ++ " :: " ++ show (typeOf x)

printType :: (Typeable a) => a -> IO ()
printType x = putStrLn $ "_ :: " ++ show (typeOf x)



class InjectType base inj out | base inj -> out where
    injectType :: base a -> inj b -> out a


--instance InjectType a Safe a where
--    injectType a _ = a

instance InjectType Safe Safe Safe where
    injectType a _ = a

instance InjectType Safe (UnsafeBase base e) (UnsafeBase base e) where
    injectType (Safe a) _ = Value a

instance InjectType (UnsafeBase base e) Safe (UnsafeBase base e) where
    injectType a _ = a


---- vvv potrzebne?
--instance InjectType (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) where
--    injectType a i = a

instance InjectType (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase baseOut e) <= (InjectType base1 base2 baseOut) where
    injectType a (i :: UnsafeBase base2 e a) = case a of
        Value a -> Value a
        Error e -> Error e
        Other o -> Other $ injectType o (undefined :: base2 a)

instance InjectType (UnsafeBase base1 e1) (UnsafeBase base2 e2) out <= (InjectType base1 (UnsafeBase Safe e2) dstBase, InjectType (UnsafeBase dstBase e1) base2 out) where
    injectType a (i :: UnsafeBase base2 e2 a) = injectType (injectType a (undefined :: UnsafeBase Safe e2 a)) (undefined :: base2 a) 

----------------------------------------------------------------------------


a = undefined :: UnsafeBase (UnsafeBase Safe E4) E1 Int
b = undefined :: UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E2) E1 Int
--b = undefined :: UnsafeBase Safe e2 a

main = do
        --printTyped $ summe' e12 e21
        --printTyped $ summe' e12 e21
        --print $ summe' e23 e34
    --printType $ summe'' e12 e21
    --printTyped $ summe'' v v
    --printTyped $ summe'' v e12
    --printTyped $ summe'' e12 v
    --printTyped $ summe'' e12 e12
    --printTyped $ summe'' e12 e13
    printType (injectType a b)
    --printType (injectType b a)
    print "end"
-- #if __GLASGOW_HASKELL__ == 708
--     printTyped $ summe' v2x2 e12
-- #endif
--     print $ summe' v2x2 e12
   
    putStrLn ""



