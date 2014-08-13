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

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

import Bind2 (bind, bind2, MonadRebase(..), StateT(..), put,get)

import Data

import Utils





------------------------------------------------------------------------
-- LiftEnv
------------------------------------------------------------------------

class LiftEnv m1 m2 m3 | m1 m2 -> m3 where
    liftenv :: m1 (a -> b) -> m2 a -> m3 b


instance LiftEnv Pure Pure Pure where
     liftenv (Pure f) (Pure a) = Pure $ f a

instance LiftEnv IO Pure IO where
     liftenv f (Pure a) = f <*> pure a

instance LiftEnv Pure IO IO where
     liftenv (Pure f) a = f <$> a

instance LiftEnv IO IO IO where
     liftenv f a = f <*> a


liftenv2 f a b       = liftenv (liftenv f a) b
liftenv3 f a b c     = liftenv (liftenv2 f a b) c
liftenv4 f a b c d   = liftenv (liftenv2 f a b c) d
liftenv5 f a b c d e = liftenv (liftenv2 f a b c d) e


liftf0 = Pure
liftf1 = liftenv  . Pure
liftf2 = liftenv2 . Pure
liftf3 = liftenv3 . Pure
liftf4 = liftenv4 . Pure
liftf5 = liftenv5 . Pure

liftfM1 a = flattenEnv `dot1` liftf1 a
liftfM2 a = flattenEnv `dot2` liftf2 a
liftfM3 a = flattenEnv `dot3` liftf3 a
liftfM4 a = flattenEnv `dot4` liftf4 a
liftfM5 a = flattenEnv `dot5` liftf5 a


------------------------------------------------------------------------
-- LiftEnv'
------------------------------------------------------------------------

class LiftEnv' m1 m2 m3 | m1 m2 -> m3 where
    liftenv' :: (a -> b) :> m1 -> a :> m2 -> b :> m3


instance LiftEnv' Pure Pure Pure where
     liftenv' (unliftCtx -> Pure f) (unliftCtx -> Pure a) = liftCtx . Pure $ f a

instance LiftEnv' IO Pure IO where
     liftenv' (unliftCtx -> f) (unliftCtx -> Pure a) = liftCtx $ f <*> pure a

instance LiftEnv' Pure IO IO where
     liftenv' (unliftCtx -> Pure f) (unliftCtx -> a) = liftCtx $ f <$> a

instance LiftEnv' IO IO IO where
     liftenv' (unliftCtx -> f) (unliftCtx -> a) = liftCtx $ f <*> a


liftenv2' f a b       = liftenv' (liftenv' f a) b
liftenv3' f a b c     = liftenv' (liftenv2' f a b) c


liftf0' = Pure
liftf1' = liftenv'  . (liftCtx . Pure)
liftf2' = liftenv2' . (liftCtx . Pure)
liftf3' = liftenv3' . (liftCtx . Pure)
--liftf4 = liftenv4 . Pure
--liftf5 = liftenv5 . Pure

liftfM1' a = flattenEnv' `dot1` liftf1' a
liftfM2' a = flattenEnv' `dot2` liftf2' a
liftfM3' a = flattenEnv' `dot3` liftf3' a
--liftfM4 a = flattenEnv `dot4` liftf4 a
--liftfM5 a = flattenEnv `dot5` liftf5 a


------------------------------------------------------------------------
-- FlattenEnv
------------------------------------------------------------------------

class FlattenEnv m1 m2 m3 | m1 m2 -> m3 where
    flattenEnv :: m1 (m2 a) -> m3 a

instance FlattenEnv Pure Pure Pure where
    flattenEnv (Pure a) = a

instance FlattenEnv Pure IO IO where
    flattenEnv (Pure a) = a

instance FlattenEnv IO Pure IO where
    flattenEnv a = do Pure val <- a
                      return val

instance FlattenEnv IO IO IO where
    flattenEnv a = do val <- a
                      val


------------------------------------------------------------------------
-- FlattenEnv'
------------------------------------------------------------------------

class FlattenEnv' m1 m2 m3 | m1 m2 -> m3 where
    flattenEnv' :: (a :> m2) :> m1 -> a :> m3

instance FlattenEnv' Pure Pure Pure where
    flattenEnv' (unliftCtx -> Pure a) = a

instance FlattenEnv' Pure IO IO where
    flattenEnv' (unliftCtx -> Pure a) = a

instance FlattenEnv' IO Pure IO where
    flattenEnv' (unliftCtx -> a) = liftCtx $ do Pure val <- unliftCtx <$> a
                                                return val

instance FlattenEnv' IO IO IO where
    flattenEnv' (unliftCtx -> a) = liftCtx $ do val <- unliftCtx <$> a
                                                val




myPrint = liftfM1 print

myPrint' = liftfM1' (liftCtx . print)

sumInt :: Int -> Int -> Int
sumInt = (+)


sumInt' = liftf2 sumInt

------------------------------------------------------------------------------------------


class Pipe a b c where
    pipe :: a -> b -> c

(>>>) = pipe


--instance out~b => Pipe (a->b) a out where
--    pipe f a = f a

--instance (Monad m, out~(m b)) => Pipe (a->b) (m a) out where
--    pipe f ma = do
--        a <- ma
--        return $ f a


--instance (Monad m, out~b) => Pipe (m a->b) a out where
--    pipe f a = f (return a)


--instance (out~b, a1~a2) => Pipe (a1->b) a2 out where
--    pipe f a = f a


--instance (Monad m, out~(m b)) => Pipe (m (a->b)) a out where
--    pipe (mf) a = do
--        f <- mf
--        return $ f a

--instance (Monad m1, Monad m2, m1~m2, out~(m2 b)) => Pipe (m1 (a->b)) (m2 a) out where
--    pipe (mf) ma =  do
--        f <- mf
--        a <- ma
--        return $ f a

--instance (Monad m, a~b, out~(c :> m)) => Pipe (b->c) (a :> m) out where
--    pipe f (InContext ma) = InContext $ do
--        a <- ma
--        return $ f a

--instance (Monad m, a~b, out~(c :> m)) => Pipe ((b->c) :> m) a out where
--    pipe (InContext mf) a = InContext $ do
--        f <- mf
--        return $ f a

--instance (Monad m, a~b, out~(c :> m)) => Pipe ((b->c) :> m) (a :> m) out where
--    pipe (InContext mf) (InContext ma) = InContext $ do
--        f <- mf
--        a <- ma
--        return $ f a



--instance (Monad m, a~b, out~(c :> m)) => Pipe (b -> (c :> m)) (a :> m) out where
--    pipe f (InContext ma) = InContext $ do
--        a <- ma
--        unliftCtx $ f a


--instance (Monad m, Monad (mt m), MonadTrans mt, a~b, out~(c :> mt m)) => 
--         Pipe (b -> (c :> m)) (a :> mt m) out where
--    pipe f (InContext ma) = InContext $ do
--        a <- ma
--        lift . unliftCtx $ f a

----------------




--instance (a~b, out~(c :> IO)) => Pipe (b -> (c :> IO)) (a :> Pure) out where
--    pipe f a = a `bind2` (f . fromPure . unliftCtx)



    --instance (a1~a2, ma1~ma2, out~b) => Pipe (a1 :> ma1 -> b) (a2 :> ma2) out where
    --    pipe f a = f a


    ---- arg lifting

    --instance (a1~a2, ma1~ma2, out~b) => Pipe (m a1 :> ma1 -> b) (m a2 :> ma2) out where
    --    pipe f a = f a


    --instance (a1~a2, ma1~ma2, out~b, Monad m, Functor ma2) => Pipe (m a1 :> ma1 -> b) (a2 :> ma2) out where
    --    pipe f a = f $ ctxmap return a

    ---- === func lifting ===

    ---- only typed functions
    ----
    ---- mulBy2 = liftf1' (*2)
    ---- pseudo usage: mulBy2 ([1:>Pure, 2:>Pure] :> Pure)
    ----
    ---- it is possible to lift typed functions as:
    ----    mulBy2 :: (Int :> Pure) -> (Int :> Pure)
    ----    
    ----
    ---- but not untyped, like:
    ----    mulBy2 :: (LiftEnv' Pure m2 m3, Num b) => (b :> m2) -> (b :> m3)
    ---- because Haskell's type checker cannot reject instance based on not meeting Num predicate
    ---- and it chooses -> instance (a1~a2, ma1~ma2, out~b) => Pipe (a1 :> ma1 -> b) (a2 :> ma2) out

    --instance (a1~a2, ma1~ma2, Functor f, Monad m, out ~ (f b :> m)) => Pipe (a1 :> ma1 -> b) (f (a2 :> ma2) :> m) out where
    --    pipe f (unliftCtx -> ma) = liftCtx $ do
    --        a <- ma
    --        return $ fmap f a


---
    --instance (a1~a2, ma1~mb ,out~(b :> ma2 mb), Monad(ma2 mb), MonadTrans ma2, Monad mb) => 
    --         Pipe (a1 :> ma1 -> b :> mb) (a2 :> ma2 mb) out where
    --    pipe f (unliftCtx -> ma) = liftCtx $ do
    --        a <- ma
    --        lift . unliftCtx $ f a
---

    --instance (out~(String :> StateT String IO)) => 
    --         Pipe (String :> Pure -> String :> IO) (String :> StateT String Pure) out where
    --    pipe f (unliftCtx -> ma) = liftCtx $ do
    --        a <- rebase ma
    --        lift . unliftCtx $ f (ctxPure a)


instance (a1~a2, ma1~ma2, out~(b :> t mb), Monad ma1, MonadRebase t ma1 mb, Monad (t mb), MonadTrans t) => 
         Pipe (a1 :> ma1 -> b :> mb) (a2 :> t ma2) out where
    pipe f (unliftCtx -> ma) = liftCtx $ do
        a <- rebase ma
        lift . unliftCtx $ f (liftCtx $ return a)



--(m a -> m b) -> (t m a)

--instance (a1~a2, ma1~ma2, out~m b) => Pipe (a1 :> ma1 -> b) (m a2 :> ma2) out where
--    pipe f a = do
--        let f' (unliftCtx -> mma :: ma2 (m a2)) = do
--            ma <- mma :: m a2


--([a] :> m2) -> [a] :> m3    ->   [Int] :> Pure

--tst2 :: (IO Int -> IO Int) -> (IO [Int]) -> [IO Int]
--tst2 f a = 


--tst3 :: (m a -> m a) -> m (a -> a)
--tst3 f = do

--tst4 :: IO [Int] -> [IO Int]

--tst :: (m a -> b)  ->   (m (f a))  ->    (f b)
--tst f ma = do
--    fa <- ma
--    fmap (f.return) fa

get' :: StateT a Pure a
get' = get

concatMe = liftf2' (++)

concatMeSimple a = liftf2' (++) a a

--time :: Monad m => a :> StateT a m
--time :: a :> StateT a Pure
time = liftCtx get'

readImageBase :: Show a => a -> String :> IO
readImageBase name = ctxIO "image"

--readImage :: (LiftEnv' Pure m2 m1, FlattenEnv' m1 IO m3, Show a) =>
--             (a :> m2) -> String :> m3

--readImage :: (String :> Pure) -> String :> IO
--readImage :: (LiftEnv' Pure m2 m1, FlattenEnv' m1 IO m3, Show a) =>
--             (a :> m2) -> String :> m3
readImage = liftfM1' readImageBase



test :: () :> IO
test = do
    liftCtx $ print "hello"

valPure :: String :> Pure
valPure = InContext $ return "test"

valIO :: String :> IO
valIO = InContext $ return "test"

addMe a b = a ++ b

ctxPrint :: String :> Pure -> () :> IO
ctxPrint s = do
    liftCtx $ print (fromPure $ unliftCtx s)


strInIO :: String :> IO
strInIO = liftCtx $ return "test"


testStateIO :: String :> StateT Float IO
testStateIO = InContext $ do
    --lift $ print "!"
    x <- get
    put (x+1)
    return "ala"



testStatePure :: String :> StateT Float Pure
testStatePure = InContext $ do
    x <- get
    put (x+1)
    return "ala"








--testStateIO2 :: String :> (IO :>> StateT Float)
--testStateIO2 = undefined

--mulBy2 :: (LiftEnv' Pure m2 m3, Num b) => (b :> m2) -> b :> m3
--mulBy2 :: Int :> Pure -> Int :> Pure
mulBy2 = liftf1' (*(2))


--newtype (a :: * -> *) :>> (m :: (* -> *) -> * -> *) = InContext2 (m a)

t2 :: () -> Int
t2 _ = 5

sme :: [Int] -> [Int] -> [Int]
sme = (++) 

main = do
        --print $ concatMeSimple >>> (ctxPure ([1,2,3] :: [Int]))
        --print $ concatMeSimple >>> (ctxPure (1 :: Int))
        --print $ concatMe >>> (ctxPure ([1,2,3] :: [Int])) >>> (ctxPure ([1,2,3] :: [Int]))
        --print $ concatMe >>> (ctxPure ([1,2,3] :: [Int])) >>> (ctxPure (1 :: Int))
        --print $ concatMe >>> (ctxPure (1 :: Int)) >>> (ctxPure (1 :: Int))


        --print $ mulBy2 >>> (ctxPure [ctxIO (2::Int),ctxIO (3::Int)])


    let x = readImage >>> time
        x :: String :> StateT String IO



        --x :: String :> StateT Double IO
    --print $ (x :: String :> Pure)

    --let x = addMe >>> valPure >>> valIO
    --print x
    --unliftCtx $ ctxPrint >>> valIO

    --print $ sme >>> [(1::Int)] >>> (2::Int)

    --print =<< (unliftCtx $ t2 >>> test)

    --print =<< (unliftCtx $ ctxPrint >>> strInIO)

    ----print =<< runStateT (unliftCtx $ ctxPrint >>> testStatePure) 5
    ----let x = runStateT (unliftCtx $ ctxPrint >>> testStatePure) 5
    --let x = (unliftCtx $ ctxPrint >>> valPure)
    --    --x :: Int

    --putStrLn "---"

    --x

    --unliftCtx $ myPrint' (liftCtx (Pure 1) :: Int :> Pure)

    putStrLn "--- end ---"