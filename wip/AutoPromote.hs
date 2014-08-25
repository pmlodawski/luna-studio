{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances      #-}


class Pipe a b c | a b -> c where
    pipe :: a -> b -> c


-- Func is needed, because (-> a) is monad and we are getting func deps conflict
newtype Func a = Func { fromFunc :: a } deriving Show

apply (Func f) a = Func $ f a

-- instances

----------------------------------------------

--instance Monad m => Pipe (Func (a -> b)) (m a) (m (Func b)) where
--    pipe f ma = do
--        a <- ma
--        return $ apply f a


----instance Monad m => Pipe (Func (m a -> b)) (a) (Func b) where
----    pipe (Func f) ma = do
----        a <- ma
----        return $ apply f a


--instance Monad m => Pipe (m (Func (a -> b))) (a) (m (Func b)) where
--    pipe mf a = do
--        f <- mf
--        return $ apply f a


--instance Monad m => Pipe (m (Func (a -> b))) (m a) (m (Func b)) where
--    pipe mf ma = do
--        a <- ma
--        f <- mf
--        return $ apply f a



--instance Pipe (Func (a -> b)) (a) (Func b) where
--    pipe f a = apply f a

----------------------------------------------

---- Poniższa implementacja nie działa.
---- (x -> (a -> b)) <=> ((-> x) (a -> b)) <=> (m (a -> b)) !

--class Context m where
--  (>>=) :: m a -> (a -> m b) -> m b
--  return :: a -> m a

--instance (Context m, Functor m) => Pipe (a -> b) (m a) (m b) where
--    pipe f ma = fmap f ma


--instance Context m => Pipe (m (a -> b)) a (m b) where
--    pipe mf a = undefined

----------------------------------------------

--class Context m where
--  (>>=) :: m a -> (a -> m b) -> m b
--  return :: a -> m a

--instance (Context m, Functor m) => Pipe (a -> b) (m a) (m b) where
--    pipe f ma = fmap f ma

----------------------------------------------



type Image = String

newtype InTime a = InTime a deriving Show

instance Monad InTime where
    return           = InTime
    (InTime a) >>= f = f a


blur :: Image -> Image
blur = (++"!")


blur2 :: InTime Image -> Image
blur2 (InTime a) = a ++ "~"


blur3 :: Image -> Image -> Image
blur3 x y = x ++ "-" ++ y ++ "!"

append :: [Int] -> [Int] -> [Int]
append = (++)

mulBy2 :: Int -> Int
mulBy2 = (*2)


main = do
    --print $ (Func blur2) `pipe` (InTime "ala")
    --print $ (Func blur3) `pipe` "ala" `pipe` "ola"
    --print $ (Func blur3) `pipe` (InTime "ala") `pipe` "ola"
    --print $ (Func blur3) `pipe` "ala" `pipe` (InTime "ola")
    --print $ (Func blur3) `pipe` (InTime "ala") `pipe` (InTime "ola")

    ----print $ (Func $ append [1,2,3]) `pipe` (2::Int)
    --print $ (Func $ mulBy2) `pipe` [2::Int, 3::Int]

    print "end"

-- Autopromote promotes an ordinary function to the input context
-- for example, having function `blur :: Image -> Image` we can use
-- `Image in Time` as an input
