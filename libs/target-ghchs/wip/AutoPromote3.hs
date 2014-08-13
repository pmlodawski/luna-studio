{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}


class Pipe a b c | a b -> c where
    pipe :: a -> b -> c


class Pipe2 m a b mc c out | m a mc -> c, m a b mc c -> out where
    pipe2 :: m (a -> b) -> mc c -> out


newtype Pure a = Pure a deriving Show
newtype PureT a = PureT a deriving Show

instance Pipe2 Context a b Pure a (Context b) where
    pipe2 (Context f) a = Context $ f a


instance Monad m => Pipe2 Context a b a (Context (m b)) where
    pipe2 (Context f) a = undefined



newtype Context a = Context a deriving Show
newtype ContextM a = ContextM a deriving Show

--instance Pipe (Context (a->b)) a (Context b) where
--    pipe (Context f) a = Context $ f a

instance Monad m => Pipe (Context (a->b)) (m a) (ContextM (m b)) where
    pipe (Context f) ma = ContextM $ do
        a <- ma
        return $ f a

instance Monad m => Pipe (ContextM (m (a->b))) (a) (ContextM (m b)) where
    pipe (ContextM mf) a = ContextM $ do
        f <- mf
        return $ f a


--instance (a1~a2) => Pipe (Context (a1->b)) a2 (Context b) where
--    pipe (Context f) a = Context $ f a

--test :: Int -> (Int,Int)
test a = (a,a)

main = do
    print $ Context test `pipe2` (5::Int)
    --print $ Context (+(1::Int)) `pipe` (5::Int)
    --print $ Context (+(1::Int)) `pipe` (Just(5::Int))

    --print $ Context test `pipe` (5::Int)
    print "end"