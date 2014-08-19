{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

--{-# LANGUAGE AllowAmbiguousTypes #-}

newtype Monadic m a = Monadic { fromMonadic :: m a } deriving Show

newtype Context a = Context { fromContext :: a } deriving Show
instance Monad Context where
    return = Context
    (Context a) >>= f = f a



class Pipe a b c where
    pipe :: a -> b -> c

(>>>) = pipe

instance out~b => Pipe (a->b) a out where
    pipe f a = f a

instance (Monad m, out~(m b)) => Pipe (a->b) (m a) out where
    pipe f ma = do
        a <- ma
        return $ f a


--instance (Monad m, out~b, a1~a2) => Pipe (m a1->b) a2 out where
--    pipe f a = f (return a)


instance (Monad m, out~b) => Pipe (m a->b) a out where
    pipe f a = f (return a)


instance (out~b, a1~a2) => Pipe (a1->b) a2 out where
    pipe f a = f a


instance (Monad m, out~(m b)) => Pipe (m (a->b)) a out where
    pipe (mf) a = do
        f <- mf
        return $ f a

instance (Monad m1, Monad m2, m1~m2, out~(m2 b)) => Pipe (m1 (a->b)) (m2 a) out where
    pipe (mf) ma =  do
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

myfun a b = test2T `pipe` a `pipe` b

myfun2 a b = myfun `pipe` a `pipe` b

myfun3 a b = getD `pipe` a `pipe` b


getD a b = TestD

data TestD = TestD deriving Show


test f a = f `pipe` a
--test2 f a b = f `pipe` a `pipe` b 
--WPROWADZIC DATATYPE DLA WYNIKU PIPE!!! ZEBY ROZROZNIAC WYWOLANIA FUNKCJI OD NIE-WYWOLAN!

sme :: [Int] -> [Int] -> [Int]
sme = (++) 


--tescik :: a -> b -> (a,b)
tescik :: Int -> Int -> Int -> (Int, Int, Int)
tescik a b c = (a,b,c)

main = do
    print $ tescik >>> (1::Int) >>> [(4::Int)] >>> [(5::Int)]

    --print $ testT >>> (1::Int)
    --print $ testT >>> (Just(1::Int))

    --putStrLn "-------------"

    --print $ testNT >>> (1::Int)
    --print $ testNT >>> (Just(1::Int))

    --putStrLn "-------------"

    --print $ test2T >>> (1::Int) >>> (2::Int)
    --print $ test2T >>> (1::Int) >>> (Just(2::Int))
    --print $ test2T >>> (Just(1::Int)) >>> (2::Int)
    --print $ test2T >>> (Just(1::Int)) >>> (Just(2::Int))

    --putStrLn "-------------"

    --print $ test2NT >>> (1::Int) >>> (2::Int)
    --print $ test2NT >>> (1::Int) >>> (Just(2::Int))
    --print $ test2NT >>> (Just(1::Int)) >>> (2::Int)
    --print $ test2NT >>> (Just(1::Int)) >>> (Just(2::Int))

    --putStrLn "-------------"

    --print $ myfun2 >>> (Just(1::Int)) >>> (Just(2::Int))

    --putStrLn "-------------"

    --print =<< (testT >>> (return(1::Int)))

    --print =<< (test2T >>> (return(1::Int)) >>> (return(1::Int)))
    
    --print $ (myfun3 >>> (1::Int) >>> (1::Int))

    --print $ (testNT >>> ((myfun3 >>> (1::Int) >>> (1::Int))))

    --print $ (++) >>> [(1::Int),(2::Int)] >>> [(3::Int),(4::Int)]
    --print $ (++) >>> [(1::Int),(2::Int)] >>> (3::Int)
    ----print $ (++) >>> (3::Int) >>> [(1::Int),(2::Int)]

    --print $ sme >>> (1::Int) >>> (2::Int)

    --print $ (++) >>> ['a','b'] >>> ['c']

    --print $ (++) >>> "ab" >>> "c"

    --print $ (+(1::Int)) >>> ([(5::Int), (6::Int), (7::Int)])

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



