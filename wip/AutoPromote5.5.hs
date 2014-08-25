{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances       #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TypeFamilies              #-}

--{-# LANGUAGE AllowAmbiguousTypes #-}

newtype Monadic m a = Monadic { fromMonadic :: m a } deriving Show

newtype Context a = Context { fromContext :: a } deriving Show
instance Monad Context where
    return = Context
    (Context a) >>= f = f a



class Pipe a b c where
    pipe :: a -> b -> c


instance out~(Context b) => Pipe (Context (a->b)) a out where
    pipe (Context f) a = Context $ f a

instance (Monad m, out~(Context (m b))) => Pipe (Context (a->b)) (m a) out where
    pipe (Context f) ma = Context $ do
        a <- ma
        return $ f a


instance (out~(Context b), a1~a2) => Pipe (Context (a1->b)) a2 out where
    pipe (Context f) a = Context $ f a


instance (Monad m, out~(Context (m b))) => Pipe (Context (m (a->b))) a out where
    pipe (Context mf) a = Context $ do
        f <- mf
        return $ f a

instance (Monad m1, Monad m2, m1~m2, out~(Context (m2 b))) => Pipe (Context (m1 (a->b))) (m2 a) out where
    pipe (Context mf) ma = Context $ do
        f <- mf
        a <- ma
        return $ f a




testT :: Int -> (Int, Int)
testT a = (a,a)

testNT a = (a,a)


test2T :: Int -> Int -> (Int,Int)
test2T a b = (a,b)

test2NT a b = (a,b)

--myfun :: (Pipe (Context a) b m c, Monad m) => a -> b -> m c
--myfun f v = Context f `pipe` v


--myfun2 :: (Pipe (Context a) b m c, Monad m, UnContext (m c) out) => a -> b -> out
--myfun2 f v = fromContext $ Context f `pipe` v

--myfun :: Int -> Int -> (Int,Int)
myfun a b = fromContext $ Context test2T `pipe` a `pipe` b


myfun2 a b = fromContext $ Context myfun `pipe` a `pipe` b

getD a b = TestD

data TestD = TestD deriving Show

class TestC a b | a -> b where
    testC :: b

instance TestC TestD (Int -> (Int,Int)) where
    testC = testT

myfun3 a b = fromContext $ Context getD `pipe` a `pipe` b

--myfun4 :: (Pipe (Context a) b m c, Monad m, UnContext (m c) out) => a -> b -> out
--myfun4 f a = fromContext $ Context f `pipe` a

--myfun4 f a = fromContext2 $ Context f `pipe` a

myfun4 f a = fromContext $ Context f `pipe` a

autoLift1 :: Pipe (Context a1) b (Context a) => a1 -> b -> a
autoLift1 f t1 = fromContext $ Context f `pipe` t1

--autoLift2 :: (Pipe (Context a) b (Context out1), Pipe (Context out1) c (Context d)) => a -> b -> c -> d
--autoLift2 f t1 = undefined
--autoLift2 f t1 t2 = fromContext $ Context f `pipe` t1 `pipe` t2


main = do
    print $ autoLift1 testT (1::Int)
    print $ autoLift1 testT (Just(1::Int))

    putStrLn "-------------"

    print $ autoLift1 testNT (1::Int)
    print $ autoLift1 testNT (Just(1::Int))

    putStrLn "-------------"

    print $ fromContext $ Context test2T `pipe` (1::Int) `pipe` (2::Int)
    print $ fromContext $ Context test2T `pipe` (1::Int) `pipe` (Just(2::Int))
    print $ fromContext $ Context test2T `pipe` (Just(1::Int)) `pipe` (2::Int)
    print $ fromContext $ Context test2T `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print $ fromContext $ Context test2NT `pipe` (1::Int) `pipe` (2::Int)
    print $ fromContext $ Context test2NT `pipe` (1::Int) `pipe` (Just(2::Int))
    print $ fromContext $ Context test2NT `pipe` (Just(1::Int)) `pipe` (2::Int)
    print $ fromContext $ Context test2NT `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print $ fromContext $ Context myfun2 `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print =<< (fromContext $ Context testT `pipe` (return(1::Int)))

    print =<< (fromContext $ Context test2T `pipe` (return(1::Int)) `pipe` (return(1::Int)))

    print $ (fromContext $ Context myfun3 `pipe` (1::Int) `pipe` (1::Int))

    print $ (fromContext $ Context testNT `pipe` ((fromContext $ Context myfun3 `pipe` (1::Int) `pipe` (1::Int))))

    print "end"

    --class Pipe a b c | a b -> c where
    --    pipe :: a -> b -> c


    --instance Monad mout => PipeMeta (Context (a -> b)) (m a) b where
    --    pipeMeta = undefined


    ----instance (a1~a2) => PipeMeta (Context (a1 -> b)) a2 (Context b) where
    ----    pipeMeta = pipe


    --instance (a1~a2) => PipeMeta (Context (a1 -> b)) a2 (Context b) where
    --    pipeMeta = pipe


    --instance Pipe (Context (a -> b)) a (Context b) where
    --    pipe (Context f) a = Context $ f a


    --instance Pipe (Context (a -> b)) (m a) (Context (m b)) where
    --    pipe = undefined


    ----instance (a1~a2) => Pipe (Context (a1 -> b)) a2 (Context b) where
    ----    pipe (Context f) a = Context $ f a

    ----type family PipeResult a b where
    ----  PipeResult (a->b) a = b

    ----instance Pipe (a -> b) (m a) b where
    ----    pipe = ($)

    ----instance (Monad m, a1~a2) => Pipe (a1 -> b) (Monadic m a2) (Monadic m b) where
    ----    pipe f ma = undefined


    --    --Monadic do
    --    --    a <- fromMonadic ma
    --    --    return $ f a



