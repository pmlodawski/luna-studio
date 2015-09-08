{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}


    ----data family F a
     
    ----f :: F a -> F a
    ----f = undefined
     
    ----g :: F Int -> F Int
    ----g x = f x



    --type family F a
     
    --f :: F a -> F a
    --f = undefined
     
    ----g :: F Int -> F Int
    ----g :: F a -> F a
    --g x = f x


newtype Monadic m a = Monadic { fromMonadic :: m a } deriving Show

newtype Context a = Context { fromContext :: a } deriving Show
newtype ContextM a = ContextM { fromContextM :: a }


class Pipe a b where
    pipe :: a -> b -> PipeResult a b


--instance Pipe (Context (a -> b)) a (Context b) where
--    pipe (Context f) a = Context $ f a

--instance (a1~a2) => Pipe (Context (a1 -> b)) a2 (Context b) where
--    pipe (Context f) a = Context $ f a

--instance Pipe (Context (a -> b)) (m a) (Context (m b)) where
--    pipe = undefined


instance Pipe (Context (a -> b)) a where
    pipe (Context f) a = Context $ f a

--instance (a1~a2) => Pipe (Context (a1 -> b)) a2 where
--    pipe cf a = pipe cf a

--instance Monad m => Pipe (Context (a -> b)) (m a) where
--    pipe (Context f) ma = Context $ do
--        a <- ma
--        return $ f a


type family PipeResult a b where
  PipeResult (Context (a->b)) (Monadic m a) = (Context (m b))
  PipeResult (Context (a->b)) a             = (Context b)
  --PipeResult (Context (a1->b)) a2   = (Context b)


--instance Pipe (a -> b) (m a) b where
--    pipe = ($)

--instance (Monad m, a1~a2) => Pipe (a1 -> b) (Monadic m a2) (Monadic m b) where
--    pipe f ma = undefined


    --Monadic do
    --    a <- fromMonadic ma
    --    return $ f a

testT :: Int -> (Int,Int)
testT a = (a,a)

testNT a = (a,a)

main = do
    --print $ Context testT `pipe` (1::Int)
    --let x :: Int
    --    x = Context testT `pipe` (Just (1::Int))
    --print $ Context testT `pipe` (Just (1::Int))

    --print $ Context testNT `pipe` (1::Int)
    --print $ Context testNT `pipe` (Just (1::Int))
    print "end"


