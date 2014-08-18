
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE UndecidableInstances #-}


!{-# LANGUAGE RightSideContexts #-}
!{-# LANGUAGE Python #-}

import Control.Applicative  
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

--import Bind2 (bind, bind2, MonadRebase(..), StateT(..), put,get)

--import Data2

import Utils


foo x y = x

data Err1 = Err1 deriving Show
data Err2 = Err2 deriving Show
data Err3 = Err3 deriving Show
data Err4 = Err4 deriving Show
data Err5 = Err5 deriving Show


newtype Safe a = Safe a deriving Show

fromSafe (Safe a) = a


instance Functor Safe where
    fmap f (Safe a) = Safe (f a)


data UnsafeBase base err val = Value val
                             | Error err
                             | Other (base val)
                             deriving Show


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


class Raise e a b | e a -> b where
    raise :: e -> a -> b 


instance Raise e (Safe a)               (UnsafeBase Safe e a)     where raise e (Safe a) = Error e
instance Raise e (UnsafeBase base e a)  (UnsafeBase base e a)     where raise e a = Error e
instance Raise e (UnsafeBase base be a) (UnsafeBase outBase be a) <= (Raise e (base a) (outBase a)) where
    raise e base = case base of
        Value val   -> Value val
        Error err   -> Error err
        Other base' -> Other $ raise e base'


----------------------------------------------------------------------------


class Catch value handler result | value handler -> result where
    catch :: handler -> value -> result


instance Catch (Safe a)                               f             (Safe a)                  where catch _ a = a
instance Catch (UnsafeBase Safe e a)                  (e -> Safe a) (Safe a)                  where
    catch f a = case a of
        Value a -> Safe a
        Error e -> f e
instance Catch (UnsafeBase (UnsafeBase base e2) e a)  (e -> Safe a) (UnsafeBase base e2 a)    where
    catch f a = case a of
        Value a -> Value a
        Error e -> Value . fromSafe $ f e
        Other o -> o
instance Catch (UnsafeBase (UnsafeBase base e2) e3 a) (e -> Safe a) (UnsafeBase dstBase e3 a) <= (Catch (UnsafeBase base e2 a) (e -> Safe a) (dstBase a)) where
    catch f base = case base of
        Value a     -> Value a
        Error e     -> Error e
        Other base' -> Other $ catch f base'


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

instance LiftErr Safe Safe Safe where
    liftErr (Safe f) (Safe a) = Safe (f a)

instance LiftErr Safe (UnsafeBase base e) (UnsafeBase base e) <= Functor base where
    liftErr (Safe f) b = f <$> b

instance LiftErr (UnsafeBase base e) Safe (UnsafeBase base e) <= (LiftErr base Safe base) where
    liftErr sf (Safe b) = case sf of
        Value f -> Value $ f b
        Error e -> Error e
        Other o -> Other $ liftErr o (Safe b)

instance LiftErr (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (Functor base, TransApplicative (UnsafeBase base e) base (UnsafeBase base e)) where
    liftErr f a = f <*> a

instance LiftErr (UnsafeBase base e1) (UnsafeBase base e2) (UnsafeBase (UnsafeBase dstBase e2) e1) <= (LiftErr (UnsafeBase base e1) base (UnsafeBase dstBase e1), LiftErr base (UnsafeBase base e2) (UnsafeBase dstBase e2)) where
    liftErr sf sa = case sf of
        Value f -> case sa of
            Value a -> Value $ f a
            Error e -> Other $ Error e
            Other o -> case liftErr sf o of
                Value a -> Value a
                Error e -> Error e
                Other o' -> Other $ Other o'
        Error e -> Error e
        Other o -> Other $ liftErr o sa


--instance LiftErr (UnsafeBase base1 e) (UnsafeBase base2 e) out where
--    liftErr sf sa = undefined




--instance LiftErr (UnsafeBase base e1) (UnsafeBase base e2) (UnsafeBase (UnsafeBase base0 e2) e1) where
--    liftErr sf sa = case sf of
--        Value f -> case sa of
--            Value a -> Value $ f a
--            Error e -> Other $ Error e
--            Other o -> liftErr sf o








--type family TestF (a :: * -> * ) (b :: * -> * ) where
--    TestF a a = Int
--    TestF a b = Char

--instance LiftErr (Either e1) (Either e2) (EitherError e1 e2) where
--    liftErr a b = case a of
--        Left e    -> JustError e
--        Right val -> EitherError (val <$> b)

--instance LiftErr (Either e1) (EitherError e2 e3) where
--    liftErr a b = case a of
--        Left e    -> JustError e
--        Right val -> EitherError (val <$> b)

--instance LiftErr (Either e) Safe (Either e) where
--    liftErr f (Safe a) = f <*> pure a 

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

summe' = liftErrf2 summe

-- !!!!!!!!!! ite ma dzialac tak, ze jezeli wybieramy NIEWYJATOWA wartosc, to walimy wyjatek!
--ite cond a b = if cond then a else b
--ite' cond = liftErrf2 (ite cond)

test :: Unsafe Err1 Int
test = Value (5::Int)




main = do
    let x = Safe(1::Int)
        ex1 = raise Err1 x
        ex1' = raise Err2 x
        ex2 = raise Err1 $ raise Err2 x
        ex3 = raise Err3 $ raise Err2 $ raise Err1 x
        ex4 = raise Err4 $ raise Err3 $ raise Err2 $ raise Err1 x
        exa = raise Err4 $ raise Err3 $ raise Err2 x

        ev1 = return 1 :: Unsafe Err1 Int


        --tval = if (1<2) then matchSafe x ex1 else matchSafe ex1 x

        --tval = summe' ex1' ev1
        tval = summe' ev1 ex1' 

    print tval
    print $ catch (\Err2 -> Safe(0::Int)) $ catch (\Err1 -> Safe(5::Int)) tval

    --print ex4
    --print $ fmap (+10) ex4
    
    --print $ catch (\Err1 -> Safe(0::Int)) $ ex2
    --print $ catch (\Err1 -> Safe(0::Int)) 
    --      . catch (\Err2 -> Safe(0::Int)) 
    --      $ ex2

    print "end"
