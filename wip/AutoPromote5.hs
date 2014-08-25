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


newtype ContextT m a = ContextT { fromContextT :: m a } deriving Show

instance Monad a => Monad (ContextT a) where
    return = ContextT . return
    (ContextT ma) >>= f = ContextT $ do
        a <- ma
        fromContextT $ f a

newtype ContextM a = ContextM { fromContextM :: a } deriving Show


class UnContext2 a b where
    unContext2 :: a -> b

class UnContext a b | a -> b where
    unContext :: a -> b

instance UnContext (Context a) a where
    unContext (Context a) = a

instance UnContext (ContextT m a) (m a) where
    unContext (ContextT ma) = ma


class Pipe a b m c | a b -> c where
    pipe :: Monad m => a -> b -> m c


instance mout~Context => Pipe (Context (a->b)) a mout b where
    pipe (Context f) a = Context $ f a

instance (Monad m, mout~(ContextT m)) => Pipe (Context (a->b)) (m a) mout b where
    pipe (Context f) ma = ContextT $ do
        a <- ma
        return $ f a


instance (mout~Context, a1~a2) => Pipe (Context (a1->b)) a2 mout b where
    pipe (Context f) a = Context $ f a


instance (Monad m, mout~(ContextT m)) => Pipe (ContextT m (a->b)) a mout b where
    pipe (ContextT mf) a = ContextT $ do
        f <- mf
        return $ f a

instance (Monad m1, Monad m2, m1~m2, mout~(ContextT m2)) => Pipe (ContextT m1 (a->b)) (m2 a) mout b where
    pipe (ContextT mf) ma = ContextT $ do
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
--myfun2 f v = unContext $ Context f `pipe` v

--myfun :: Int -> Int -> (Int,Int)
myfun a b = unContext $ Context test2T `pipe` a `pipe` b


myfun2 a b = unContext $ Context myfun `pipe` a `pipe` b

getD a b = TestD

data TestD = TestD deriving Show

class TestC a b | a -> b where
    testC :: b

instance TestC TestD (Int -> (Int,Int)) where
    testC = testT

myfun3 a b = unContext $ Context getD `pipe` a `pipe` b

--myfun4 :: (Pipe (Context a) b m c, Monad m, UnContext (m c) out) => a -> b -> out
--myfun4 f a = unContext $ Context f `pipe` a


main = do
    print $ unContext $ Context testT `pipe` (1::Int)
    print $ unContext $ Context testT `pipe` (Just(1::Int))

    putStrLn "-------------"

    print $ unContext $ Context testNT `pipe` (1::Int)
    print $ unContext $ Context testNT `pipe` (Just(1::Int))

    putStrLn "-------------"

    print $ unContext $ Context test2T `pipe` (1::Int) `pipe` (2::Int)
    print $ unContext $ Context test2T `pipe` (1::Int) `pipe` (Just(2::Int))
    print $ unContext $ Context test2T `pipe` (Just(1::Int)) `pipe` (2::Int)
    print $ unContext $ Context test2T `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print $ unContext $ Context test2NT `pipe` (1::Int) `pipe` (2::Int)
    print $ unContext $ Context test2NT `pipe` (1::Int) `pipe` (Just(2::Int))
    print $ unContext $ Context test2NT `pipe` (Just(1::Int)) `pipe` (2::Int)
    print $ unContext $ Context test2NT `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print $ unContext $ Context myfun2 `pipe` (Just(1::Int)) `pipe` (Just(2::Int))

    putStrLn "-------------"

    print =<< (unContext $ Context testT `pipe` (return(1::Int)))

    print =<< (unContext $ Context test2T `pipe` (return(1::Int)) `pipe` (return(1::Int)))

    print $ (unContext $ Context myfun3 `pipe` (1::Int) `pipe` (1::Int))

    print $ (unContext $ Context testNT `pipe` ((unContext $ Context myfun3 `pipe` (1::Int) `pipe` (1::Int))))

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



