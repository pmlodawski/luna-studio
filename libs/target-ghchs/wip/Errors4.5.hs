
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


!{-# LANGUAGE RightSideContexts #-}
!{-# LANGUAGE Python #-}

import Control.Applicative  
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Typeable


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
                             deriving (Show, Typeable, Eq)


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


instance TransMonad (UnsafeBase base err1) (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2) <= (TransMonad base (UnsafeBase (UnsafeBase base err1) err2) (UnsafeBase (UnsafeBase base err1) err2)) where
    ma >>=~ f = case ma of
        Value a -> f a
        Error a -> Other $ Error a
        Other o -> o >>=~ f


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

--instance LiftErr (UnsafeBase base e) (UnsafeBase base e) (UnsafeBase base e) <= (Functor base, TransApplicative (UnsafeBase base e) base (UnsafeBase base e)) where
--    liftErr f a = f <*> a

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

instance LiftErr (UnsafeBase base1 e) (UnsafeBase base2 e) (UnsafeBase dstBase e) <= (Monad base1, Monad base2, LiftErr base1 base2 dstBase) where
    liftErr (sf :: UnsafeBase base1 e (a->b)) (sa :: UnsafeBase base2 e a) = case sf of
        Value f -> case sa of
            Value a -> Value $ f a
            Error e -> Error e
            Other o -> Other $ liftErr (return f :: base1 (a->b)) o
        Error e -> Error e
        Other o -> case sa of
            Value a  -> Other $ liftErr o (return a :: base2 a)
            Error e  -> Error e
            Other o' -> Other $ liftErr o o'


instance LiftErr (UnsafeBase base1 e1) (UnsafeBase base2 e2) (UnsafeBase dstBase e1) <= (Monad base1, (LiftErr base1 (UnsafeBase base2 e2) dstBase)) where
    liftErr (sf :: UnsafeBase base1 e1 (a->b)) sa = case sf of
        Value f -> case sa of
            Value a -> Value $ f a
            Error e -> Other $ liftErr (return f :: base1 (a->b)) sa
        Error e -> Error e


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

shouldBeT :: (Typeable a, Typeable b, Show b, Eq b) => a -> b -> Expectation
shouldBeT a b = maybe (typeOf a `shouldBe` typeOf b) (\b' -> b' `shouldBe` b) (cast a)

--class HSpecCmp a b where
--    hscmp :: a -> b -> Expectation

--instance (Show a, Eq a) => HSpecCmp a a where
--    hscmp = shouldBe

--instance (Show a, Eq a, Show b, Eq b, Typeable a, Typeable b) => HSpecCmp a b where
--    hscmp = shouldBeT

--main = print $ (summe' v v :: Int)

main = do
  hspec $ do
      describe "Raising errors" $ do
        it "e1"          $ raise E1 v                                    `shouldBeT` (Error E1 :: UnsafeBase Safe E1 Int)
        it "e1 e2"       $ (raise E2 . raise E1) v                       `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
        it "e1 e2 e1"    $ (raise E1 . raise E2 . raise E1) v            `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
        it "e3 e1 e2 e1" $ (raise E3 . raise E1 . raise E2 . raise E1) v `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)

      describe "LiftErr testing" $ do
        describe "Simple value lifting" $ do
            it "sum v v"     $ summe' v v     `shouldBeT` (Safe (2::Int))
            it "sum v e1"    $ summe' v e1    `shouldBeT` (Error E1 :: UnsafeBase Safe E1 Int)
            it "sum e1 v"    $ summe' e1 v    `shouldBeT` (Error E1 :: UnsafeBase Safe E1 Int)
        describe "Complex value lifting" $ do
            it "sum v e123"  $ summe' v e123  `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
            it "sum e123 v"  $ summe' e123 v  `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int)
        describe "Simple Errors lifting" $ do
            it "sum e1 e2"   $ summe' e1 e2   `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "sum e2 e1"   $ summe' e2 e1   `shouldBeT` (Error E2 :: UnsafeBase (UnsafeBase Safe E1) E2 Int)
            it "sum e1 e1"   $ summe' e1 e1   `shouldBeT` (Error E1 :: UnsafeBase Safe E1 Int)
        describe "Complex Errors lifting" $ do
            it "sum e12 e12"  $ summe' e12 e12  `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int)
            it "sum e123 e34" $ summe' e123 e34 `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int)
            it "sum e123 e34" $ summe' e123 e34 `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int)
        describe "Unsafe values lifting" $ do
            it "sum v2 e12"   $ magicMerge (summe' v2 e12) `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int) 
            it "sum e12 v2"   $ summe' e12 v2              `shouldBeT` (Error E1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int) 


v  = Safe(1::Int)

e1 = raise E1 v
e2 = raise E2 v
e3 = raise E3 v
e4 = raise E4 v
e12 = raise E2 e1
e34 = raise E4 e3
e123 = raise E3 e12
e1234 = raise E4 e123

v1 = Value 1 :: UnsafeBase Safe E1 Int
v2 = Value 1 :: UnsafeBase Safe E2 Int
v3 = Value 1 :: UnsafeBase Safe E3 Int
v4 = Value 1 :: UnsafeBase Safe E4 Int
v12 = Value 1 :: UnsafeBase (UnsafeBase Safe E2) E1 Int
v123 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase Safe E3) E2) E1 Int
v1234 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
vx1 = Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
vx2 = Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
vx3 = Other . Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int
vx4 = Other . Other . Other $ Value 1 :: UnsafeBase (UnsafeBase (UnsafeBase (UnsafeBase Safe E4) E3) E2) E1 Int




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