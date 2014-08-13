
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
{-# LANGUAGE IncoherentInstances #-}

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
import Control.TransApplicative
import Control.TransMonad


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

data NOP = NOP deriving Show

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


instance TransMonad (UnsafeBase base err1) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) <= (TransMonad base (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2)) where
    ma >>=~ f = case ma of
        Value a -> f a
        Error a -> Other $ Error a
        Other o -> o >>=~ f


--instance TransMonad base (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) where



instance Applicative (UnsafeBase base err) <= (Functor base, (TransApplicative (UnsafeBase base err) base (UnsafeBase base err))) where
    pure = Value
    a <*> b = a <<*>> b


instance TransApplicative Safe Safe Safe where
    (Safe f) <<*>> (Safe b) = Safe $ f b

instance TransApplicative Safe (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative Safe base base) where
    func@(Safe f) <<*>> b = case b of
        Value v -> Value $ f v
        Error e -> Error e
        Other o -> Other $ func <<*>> o


instance TransApplicative (UnsafeBase base err) Safe (UnsafeBase base err) <= (TransApplicative base Safe base)where
    sf <<*>> arg@(Safe b) = case sf of
        Value f -> Value $ f b
        Error e -> Error e
        Other o -> Other $ o <<*>> arg


--instance TransApplicative (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative (UnsafeBase base err) base base) where
--    a <<*>> b = case a of
--        func@(Value f) -> case b of
--            Value v -> Value $ f v
--            Error e -> Error e
--            Other o -> Other $ func <<*>> o


instance TransApplicative (UnsafeBase base err) (UnsafeBase base err) (UnsafeBase base err) <= (TransApplicative (UnsafeBase base err) base (UnsafeBase base err)) where
    a <<*>> b = case a of
        func@(Value f) -> case b of
            Value v -> Value $ f v
            Error e -> Error e
            Other o -> a <<*>> o
        Error e -> Error e
        -- TODO: end me


--instance TransApplicative (UnsafeBase base err) Safe where
--    sf <<*>> (Safe b) = case sf of
--        --Value f -> Value $ f b
--        Error e -> Error e
--        --Other o -> Other $ func <<*>> o






----------------------------------------------------------------------------


class Raise e a b | e a -> b where
    raise :: e -> a -> b 


instance Raise e (Safe a)               (UnsafeBase Safe e a)     where raise e (Safe a) = Error e
instance Raise e (UnsafeBase base e a)  (UnsafeBase base e a)     where raise e a = Error e
instance Raise e (UnsafeBase base be a) (UnsafeBase outBase be a) <= (Raise e (base a) (outBase a)) where
    raise e base = case base of
        Value val   -> Value val
        Error err   -> Error err
        Other base' -> Other $ raise e base'





class MatchSafe a b c | a b -> c where
    matchSafe :: a -> b -> c

----------------------------------------------------------------------------


--instance MatchSafe (Safe ref) (Safe a) (Safe a) where
--    matchSafe _ = id


--instance MatchSafe (UnsafeBase base e a) (UnsafeBase base e a) (UnsafeBase base e a) where
--    matchSafe _ = id


--instance MatchSafe (UnsafeBase base e ref) (Safe a) (UnsafeBase base e a) where
--    matchSafe _ (Safe a) = Value a


--instance MatchSafe (Safe ref) (UnsafeBase base e a) (UnsafeBase base e a) where
--    matchSafe _ = id


------------------------------------------------------------------------
-- LiftErr
------------------------------------------------------------------------

class LiftErr m1 m2 m3 | m1 m2 -> m3 where
    liftErr :: m1 (a -> b) -> m2 a -> m3 b

    --instance LiftErr Safe Safe Safe where
    --    liftErr (Safe f) (Safe a) = Safe (f a)

instance LiftErr Safe (UnsafeBase base e) (UnsafeBase base e) <= Functor base where
    liftErr (Safe f) b = f <$> b

    --instance LiftErr (UnsafeBase base e) Safe (UnsafeBase base e) <= (LiftErr base Safe base) where
    --    liftErr sf (Safe b) = case sf of
    --        Value f -> Value $ f b
    --        Error e -> Error e
    --        Other o -> Other $ liftErr o (Safe b)

--instance LiftErr (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (Functor base, TransApplicative (UnsafeBase base e) base (UnsafeBase base e)) where
--    liftErr f a = f <*> a


    --instance LiftErr (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (LiftErr Safe base base, LiftErr base Safe base, LiftErr base base base) where
    --    liftErr sf sa = case sf of
    --        Value f -> case sa of
    --            Value a -> Value $ f a
    --            Error e -> Error e
    --            Other o -> Other $ liftErr (Safe f) o
    --        Error e -> Error e
    --        Other o -> case sa of
    --            Value a  -> Other $ liftErr o (Safe a)
    --            Error e  -> Error e
    --            Other o' -> Other $ liftErr o o'

--instance LiftErr (UnsafeBase base e1) (UnsafeBase base e2) (UnsafeBase (UnsafeBase dstBase e2) e1) <= (LiftErr (UnsafeBase base e1) base (UnsafeBase dstBase e1), LiftErr base (UnsafeBase base e2) (UnsafeBase dstBase e2)) where
--    liftErr sf sa = case sf of
--        Value f -> case sa of
--            Value a -> Value $ f a
--            Error e -> Other $ Error e
--            Other o -> case liftErr sf o of
--                Value a -> Value a
--                Error e -> Error e
--                Other o' -> Other $ Other o'
--        Error e -> Error e
--        Other o -> Other $ liftErr o sa



--base1 :: (UnsafeBase Safe E1)
--base2 :: Safe

--LiftErr Safe Safe dstBase

    --instance LiftErr (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase dstBase e) <= (Monad base1, Monad base2, LiftErr base1 base2 dstBase) where
    --    liftErr (sf :: UnsafeBase base1 e (a->b)) (sa :: UnsafeBase base2 e a) = case sf of
    --        Value f -> case sa of
    --            Value a -> Value $ f a
    --            Error e -> Error e
    --            Other o -> Other $ liftErr (return f :: base1 (a->b)) o
    --        Error e -> Error e
    --        Other o -> case sa of
    --            Value a  -> Other $ liftErr o (return a :: base2 a)
    --            Error e  -> Error e
    --            Other o' -> Other $ liftErr o o'


--instance LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) (UnsafeBase dstBase e1) <= (Monad base1, (LiftErr base1 (UnsafeBase base2 e2) dstBase)) where
--    liftErr (sf :: UnsafeBase base1 e1 (a->b)) sa = case sf of
--        Value f -> case sa of
--            Value a -> Value $ f a
--            Error e -> Other $ liftErr (return f :: base1 (a->b)) sa
--        Error e -> Error e


    --instance LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) (UnsafeBase dstBase e1) <= (Monad base1, (LiftErr base1 (UnsafeBase base2 e2) dstBase)) where
    --    liftErr (sf :: UnsafeBase base1 e1 (a->b)) sa = case sf of
    --        Value f -> case sa of
    --            Value a -> Value $ f a
    --            Error e -> Other $ liftErr (return f :: base1 (a->b)) sa
    --        Error e -> Error e



--instance LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) out where
--    liftErr (sf :: UnsafeBase base1 e1 (a->b)) (sa :: UnsafeBase base2 e2 a) = case sf of
--        Value f -> case sa of
--            Value a -> injectType ((Value $ f a) :: UnsafeBase base1 e1 b) sa
--            --Error e -> Other $ liftErr (return f :: base1 (a->b)) sa
--        --Error e -> Error e

--e12   = raise E2 e1   :: 
--e21   = raise E1 e2   :: 
--                  UnsafeBase (UnsafeBase Safe E2) E1 Int ... UnsafeBase (UnsafeBase Safe E1) E2  Int  <= (LiftErr (UnsafeBase (UnsafeBase Safe E2) E1) (UnsafeBase Safe E2) rx, Functor rx, LiftErr rx (UnsafeBase Safe E1) out)
        --instance LiftErr (UnsafeBase base1                e1)       (UnsafeBase base2                e2) out  <= (LiftErr (UnsafeBase base1 e1) (UnsafeBase Safe e2) rx, Functor rx, LiftErr rx base2 out, Functor base1) where
        --    liftErr (sf :: UnsafeBase base1 e1 (a->b)) (sa :: UnsafeBase base2 e2 a) = case sa of
        --        Value a -> liftErr (fmap const $ liftErr sf (Value a :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
        --        Error e -> liftErr (fmap const $ liftErr sf (Error e :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
        --        Other o -> liftErr (liftErr (fmap const sf) (undefined :: UnsafeBase Safe e2 a)) (o :: base2 a)


                  --UnsafeBase (UnsafeBase Safe E2) E1 Int ... UnsafeBase (UnsafeBase Safe E1) E2  Int  <= (LiftErr (UnsafeBase (UnsafeBase Safe E2) E1) (UnsafeBase Safe E2) rx, Functor rx, LiftErr rx (UnsafeBase Safe E1) out)
--instance LiftErr (UnsafeBase base1                e1)       (UnsafeBase base2                e2) out <= (Monad base1, LiftErr base1 (UnsafeBase Safe e2) dstBase, Functor dstBase, LiftErr (UnsafeBase dstBase e1) base2 out) where
--    liftErr (sf :: UnsafeBase base1 e1 (a->b)) (sa :: UnsafeBase base2 e2 a) = case sf of
--        Value f -> case sa of
--            Value a -> liftErr (Other $ fmap const $ liftErr (return f :: base1 (a->b)) (Value a :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
        --Error e -> liftErr (fmap const $ liftErr sf (Error e :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
        --Other o -> liftErr (liftErr (fmap const sf) (undefined :: UnsafeBase Safe e2 a)) (o :: base2 a)


--instance InjectType (UnsafeBase base1 e1) (UnsafeBase base2 e2) out <= (InjectType base1 (UnsafeBase Safe e2) dstBase, InjectType (UnsafeBase dstBase e1) base2 out) where
--    injectType a (i :: UnsafeBase base2 e2 a) = injectType (injectType a (undefined :: UnsafeBase Safe e2 a)) (undefined :: base2 a) 




    --instance LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) (UnsafeBase dstBase dstE) <= (Monad base1, LiftErr base1 (UnsafeBase base2 e2) dstBase) where
    --    liftErr (sf :: UnsafeBase base1 e1 (a->b)) sa = case sf of
    --        Value f -> case sa of
    --            Error e -> Other $ liftErr (return f :: base1 (a->b)) sa
    --        --Error e -> Error e



--instance LiftErr (UnsafeBase (UnsafeBase base e0) e1) (UnsafeBase Safe e2) (UnsafeBase dstBase e1) <= (LiftErr (UnsafeBase base e0) (UnsafeBase Safe e2) dstBase, LiftErr (UnsafeBase (UnsafeBase base e0) e1) Safe dstBase) where
--    liftErr (sf :: UnsafeBase (UnsafeBase base e0) e1 (a->b)) sa = case sf of
--        Value f -> case sa of
--            Value a -> Value $ f a
--            Error e -> Other $ liftErr (Value f :: UnsafeBase base e0 (a->b)) sa
--            Other o -> Other $ liftErr sf o  -- no Other?
--        Error e -> Error e
--        Other o -> Other $ liftErr o sa




class LiftErr' m1 m2 m3 | m1 m2 -> m3 where
    liftErr' :: m1 (a->b) -> m2 a -> m3 b


--instance LiftErr' a Safe a where
--    liftErr' a _ = a

instance LiftErr' Safe Safe Safe where
    liftErr' (Safe f) (Safe a) = Safe (f a)

instance LiftErr' Safe (UnsafeBase base e) (UnsafeBase base e) <= (LiftErr' Safe base base) where
    liftErr' (Safe f) sa = case sa of
        Value a -> Value $ f a
        Error e -> Error e
        Other o -> Other $ liftErr' (Safe f) o

instance LiftErr' (UnsafeBase base e) Safe (UnsafeBase base e) <= (LiftErr' base Safe base) where
    liftErr' sf (Safe b) = case sf of
        Value f -> Value $ f b
        Error e -> Error e
        Other o -> Other $ liftErr' o (Safe b)

--------------------------
--instance LiftErr' (UnsafeBase base e1) (UnsafeBase Safe e2) (UnsafeBase dstBase e1) <= (LiftErr' base (UnsafeBase Safe e2) dstBase, Monad base) where
--    liftErr' (sf :: UnsafeBase base e1 (a->b)) sa = case sf of
--        Value f -> case sa of
--            Value a -> Value $ f a
--            Error e -> Other $ liftErr' (return f :: base (a->b)) sa
--            Other (Safe a) -> Value $ f a
--        Error e -> Error e
--        Other o -> Other $ liftErr' o sa

liftErrxxx :: (LiftErr' base (UnsafeBase Safe t) dstBase, Monad base) => UnsafeBase base e1 (a -> b) -> UnsafeBase Safe t a -> UnsafeBase dstBase e1 b
liftErrxxx (sf :: UnsafeBase base e1 (a->b)) sa = case sf of
    Value f -> case sa of
        Value a -> Value $ f a
        Error e -> Other $ liftErr' (return f :: base (a->b)) sa
        Other (Safe a) -> Value $ f a
    Error e -> Error e
    Other o -> Other $ liftErr' o sa

-- vvv potrzebne?
instance LiftErr' (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (LiftErr' Safe base base, LiftErr' base Safe base, LiftErr' base base base) where
    liftErr' sf sa = case sf of
        Value f -> case sa of
            Value a -> Value $ f a
            Error e -> Error e
            Other o -> Other $ liftErr' (Safe f) o
        Error e -> Error e
        Other o -> case sa of
            Value a  -> Other $ liftErr' o (Safe a)
            Error e  -> Error e
            Other o' -> Other $ liftErr' o o'


instance LiftErr' (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase dstBase e) <= (Monad base1, Monad base2, LiftErr' base1 base2 dstBase) where
    liftErr' (sf :: UnsafeBase base1 e (a->b)) (sa :: UnsafeBase base2 e a) = case sf of
        Value f -> case sa of
            Value a -> Value $ f a
            Error e -> Error e
            Other o -> Other $ liftErr' (return f :: base1 (a->b)) o
        Error e -> Error e
        Other o -> case sa of
            Value a  -> Other $ liftErr' o (return a :: base2 a)
            Error e  -> Error e
            Other o' -> Other $ liftErr' o o'








--instance LiftErr' (UnsafeBase base1 e1) (UnsafeBase base2 e2) out <= (LiftErr' base1 (UnsafeBase Safe e2) dstBase, LiftErr' (UnsafeBase dstBase e1) base2 out) where
--    liftErr' = undefined

    --instance LiftErr' (UnsafeBase base1 e1) (UnsafeBase base2 e2) out  <= (LiftErr' base1 (UnsafeBase Safe e2) dstBase, LiftErr' (UnsafeBase dstBase e1) base2 out, Functor dstBase, LiftErr' dstBase Safe dstBase, Functor base1, Monad base1, Monad base2) where
    --    liftErr' (sf :: UnsafeBase base1 e1 (a->b)) (sa :: UnsafeBase base2 e2 a) = case sa of
    --        Value a -> liftErr' (fmap const $ liftErrxxx sf (Value a :: UnsafeBase Safe e2 a)) (return undefined :: base2 a)
    --        Error e -> liftErr' (fmap const $ liftErrxxx sf (Error e :: UnsafeBase Safe e2 a)) (return undefined :: base2 a)
    --        Other o -> liftErr' (liftErrxxx (fmap const sf) (return undefined :: UnsafeBase Safe e2 a)) (o :: base2 a)
    --        --Value a -> liftErr' (fmap const $ liftErr' sf (Value a :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
    --        --Error e -> liftErr' (fmap const $ liftErr' sf (Error e :: UnsafeBase Safe e2 a)) (undefined :: base2 a)
    --        --Other o -> liftErr' (liftErr' (fmap const sf) (undefined :: UnsafeBase Safe e2 a)) (o :: base2 a)

--instance InjectType (UnsafeBase base1 e1) (UnsafeBase base2 e2) out <= (InjectType base1 (UnsafeBase Safe e2) dstBase, InjectType (UnsafeBase dstBase e1) base2 out) where
--    injectType a (i :: UnsafeBase base2 e2 a) = injectType (injectType a (undefined :: UnsafeBase Safe e2 a)) (undefined :: base2 a) 


liftErr2' f a b       = liftErr' (liftErr' f a) b
liftErrf2' = liftErr2' . Safe


liftErr2 f a b       = liftErr (liftErr f a) b
--liftErr3 f a b c     = liftErr (liftErr2 f a b) c
--liftErr4 f a b c d   = liftErr (liftErr2 f a b c) d
--liftErr5 f a b c d e = liftErr (liftErr2 f a b c d) e

liftErrf0 = Safe
liftErrf1 = liftErr  . Safe
liftErrf2 = liftErr2 . Safe
--liftErrf3 = liftErr3 . Safe
--liftErrf4 = liftErr4 . Safe
--liftErrf5 = liftErr5 . Safe

summe :: Int -> Int -> Int
summe = (+)

--summe' = liftErrf2 summe

summe' = liftErrf2' summe

-- !!!!!!!!!! ite ma dzialac tak, ze jezeli wybieramy NIEWYJATKOWA wartosc, to walimy wyjatek!
--ite cond a b = if cond then a else b
--ite' cond = liftErrf2 (ite cond)

test :: Unsafe E1 Int
test = Value (5::Int)


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


data ReRaise a = ReRaise deriving Show

class Catch value e h result | value e h -> result where
    catch :: (e -> h a) -> value a -> result a


-- === basic catching ===

instance Catch Safe a m Safe where catch _ = id

instance Catch (UnsafeBase base e) e Safe out <= (Monad out, Catch base e Safe out) where
    catch f a = case a of
        Value a -> return a
        Error e -> return . fromSafe $ f e
        Other o -> catch f o

instance Catch (UnsafeBase base e1) e2 Safe (UnsafeBase dstBase e1) <= (Catch base e2 Safe dstBase) where
    catch f sa = case sa of
        Value a -> Value a
        Error e -> Error e
        Other o -> Other $ catch f o


-- === re-raising ===

instance Catch (UnsafeBase base e1) e2 ReRaise (UnsafeBase base e1) where
    catch _ = id


-- === nested raising ===

instance Catch (UnsafeBase base e1) e2 (UnsafeBase base e1) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e1) dstBase) where
    catch f sa = case sa of
        Value a -> Value a
        Error e -> Error e
        Other o -> Other $ catch f o


instance Catch (UnsafeBase base e1) e1 (UnsafeBase base e2) (UnsafeBase base e2) where
    catch f sa = case sa of
        Value a -> Value a
        Error e -> f e
        Other o -> Other o


instance Catch (UnsafeBase base e1) e1 (UnsafeBase base e1) (UnsafeBase base e1) where
    catch f sa = case sa of
        Value a -> Value a
        Error e -> f e
        Other o -> Other o



instance Catch (UnsafeBase base e1) e2 (UnsafeBase base e3) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e3) dstBase) where
    catch f sa = case sa of
        Value a -> Value a
        Error e -> Error e
        Other o -> Other $ catch f o


--instance Catch (UnsafeBase base1 e1) e1 (UnsafeBase base2 e2) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e3) dstBase) where
--    catch f sa = case sa of
--        Value a -> Value a
--        Error e -> f e
--        Other o -> Other $ catch f o


--instance Catch (UnsafeBase base1 e1) e2 (UnsafeBase base3 e3) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e3) dstBase) where
--    catch f sa = case sa of
--        Value a -> Value a
--        Error e -> Error e
--        Other o -> Other $ catch f o




    --instance Catch (UnsafeBase base e1) e2 (UnsafeBase base e1) (UnsafeBase dstBase e1) <= (Catch base e2 (UnsafeBase base e1) dstBase) where
    --    catch f sa = case sa of
    --        Value a -> Value a
    --        Error e -> Error e
    --        Other o -> Other $ catch f o


--instance Catch (UnsafeBase (UnsafeBase base e2) e a)  (e -> Safe a) (UnsafeBase base e2 a)    where
--    catch f a = case a of
--        Value a -> Value a
--        Error e -> Value . fromSafe $ f e
--        Other o -> o
--instance Catch (UnsafeBase (UnsafeBase base e2) e3 a) (e -> Safe a) (UnsafeBase dstBase e3 a) <= (Catch (UnsafeBase base e2 a) (e -> Safe a) (dstBase a)) where
--    catch f base = case base of
--        Value a     -> Value a
--        Error e     -> Error e
--        Other base' -> Other $ catch f base'



a = undefined :: UnsafeBase (UnsafeBase Safe E4) E1 Int
b = undefined :: UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E2) E1 Int

--b = undefined :: UnsafeBase Safe e2 a

main = do
    printTyped $ catch (\E1 -> Safe(0::Int)) (Safe (1::Int))
    printTyped $ catch (\E1 -> Safe(0::Int)) (raise E2 $ Safe (1::Int))
    printTyped $ catch (\E1 -> Safe(0::Int)) (raise E1 $ Safe (1::Int))
    printTyped $ catch (\E1 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int))
    printTyped $ catch (\E2 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int))
    printTyped $ catch (\E4 -> Safe(0::Int)) (raise E2 $ raise E1 $ Safe (1::Int))
    printTyped $ catch (\E2 -> Safe(2::Int)) $ catch (\E1 -> Safe(1::Int)) (raise E2 $ raise E1 $ Safe (1::Int))
    printTyped $ catch (\E1 -> Safe(1::Int)) $ catch (\E2 -> Safe(2::Int)) (raise E2 $ raise E1 $ Safe (1::Int))

    printTyped  $ catch (\E2 -> Safe(2::Int)) $ catch (\E3 -> Safe(3::Int)) $ catch (\E2 -> Safe(2::Int)) $ catch (\E1 -> Safe(1::Int)) (raise E2 $ raise E3 $ Safe (1::Int))
    putStrLn "--------"

    printTyped $ catch (\E1 -> ReRaise) (Safe (1::Int))
    printTyped $ catch (\E1 -> ReRaise) (raise E2 $ Safe (1::Int))

    putStrLn "--------"

    printTyped $ catch (\E1 -> raise E1 $ Safe(0::Int)) (Safe (1::Int))
    printTyped $ catch (\E1 -> raise E1 $ Safe(0::Int)) (raise E1 $ Safe (1::Int))
    printTyped $ catch (\E1 -> raise E2 $ Safe(0::Int)) (raise E1 $ Safe (1::Int))


    --todo ...

    --printTyped $ summe' e12 e21
    --printTyped $ summe' e12 e21
    --print $ summe' e23 e34
    --printType $ summe'' e12 e21
    --printTyped $ summe'' v v
    --printTyped $ summe'' v e12
    --printTyped $ summe'' e12 v
    --printTyped $ summe'' e12 e12
    --printTyped $ summe'' e12 e13
    --printType (injectType a b)
    --printType (injectType b a)
    print "end"





v  = Safe(1::Int)

e1    = raise E1 v    :: UnsafeBase Safe E1 Int
e2    = raise E2 v    :: UnsafeBase Safe E2 Int
e3    = raise E3 v    :: UnsafeBase Safe E3 Int
e4    = raise E4 v    :: UnsafeBase Safe E4 Int
e12   = raise E2 e1   :: UnsafeBase (UnsafeBase Safe E2) E1 Int
e13   = raise E3 e1   :: UnsafeBase (UnsafeBase Safe E3) E1 Int
e21   = raise E1 e2   :: UnsafeBase (UnsafeBase Safe E1) E2 Int
e23   = raise E3 e2   :: UnsafeBase (UnsafeBase Safe E3) E2 Int
e34   = raise E4 e3   :: UnsafeBase (UnsafeBase Safe E4) E3 Int
e35   = raise E5 e3   :: UnsafeBase (UnsafeBase Safe E5) E3 Int
e123  = raise E3 e12  :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int
e1234 = raise E4 e123 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int


v12 = Value 1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int
v123 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int
v1234 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx1 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx2 = Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx3 = Other . Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
--vx4 = Other . Other . Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int

v1x1 = Value 1 :: UnsafeBase Safe E1 Int
v2x2 = Value 1 :: UnsafeBase Safe E2 Int
v3x3 = Value 1 :: UnsafeBase Safe E3 Int
v4x4 = Value 1 :: UnsafeBase Safe E4 Int
v2x12 = Other $ Value 1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int
v4x34 = Other $ Value 1 :: UnsafeBase (UnsafeBase Safe E4) E3 Int

v3x13 = Other $ Value 1 :: UnsafeBase (UnsafeBase Safe E3) E1 Int

v1x21 = Other $ Value 1 :: UnsafeBase (UnsafeBase Safe E1) E2 Int
v1x31 = Other $ Value 1 :: UnsafeBase (UnsafeBase Safe E1) E3 Int




--  print "hello"

--ex1 = raise E1 v
--ex1' = raise E2 v
--ex2 = raise E1 $ raise E2 v
--ex3 = raise E3 $ raise E2 $ raise E1 v
--exa = raise E4 $ raise E3 $ raise E2 v

--ex4 = raise E1 $ raise E2 v :: UnsafeBase (UnsafeBase Safe E1) E2 Int
--ev1 = return 1                  :: UnsafeBase Safe E1 Int

--main = do
--    printTyped $ summe' v2 e12
--    let
        

--        --niepoprawna wartosc - OSTATNIA NIEODKOMENTOWANA INSTANCJA - niepprawne zalozenie, ze w wyniku mamy e1 ...


--        --tval = if (1<2) then matchSafe x ex1 else matchSafe ex1 x

--        --tval = summe' ex1' ev1
--    --    tval = summe' ev1 ex1' 
--        tval :: UnsafeBase (UnsafeBase Safe E1) E2 Int
--        tval = magicMerge $ summe' ev1 ex4

--        --tval2 :: Int
--        --tval2 = magicMerge tval

--        --tval :: Int
--        --tval = summe' ex5 ev1

--    print tval
--    print $ catch (\E2 -> Safe(0::Int)) $ catch (\E1 -> Safe(5::Int)) tval

--    print ex4
--    print $ fmap (+10) ex4
    
--    print $ catch (\E1 -> Safe(0::Int)) $ ex2
--    print $ catch (\E1 -> Safe(0::Int)) 
--          . catch (\E2 -> Safe(0::Int)) 
--          $ ex2