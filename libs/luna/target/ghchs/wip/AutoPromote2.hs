{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}


class Pipe a b c | a b -> c where
    pipe :: a -> b -> c


class Pipe2 a b c where
    pipe2 :: a -> b -> c


----------------------------------------------

instance Monad m => Pipe (a -> b) (m a) (m b) where
    pipe f ma = do 
        a <- ma 
        return $ f a


instance Pipe (a -> b) a b where
    pipe = ($)


--instance Pipe (a -> b) [a] [b] where
--    pipe = fmap

--instance Monad m => Pipe (a -> b) (m [a]) (m [b]) where
--    pipe f ma = do
--        a <- ma
--        return $ fmap f a

----------------------------------------------

instance (Monad m, out~(m b)) => Pipe2 (a -> b) (m a) out where
    pipe2 f ma = do 
        a <- ma 
        return $ f a


--instance (Monad m, out~(m b)) => Pipe2 (a -> b) (m a) out where
--    pipe2 f ma = do 
--        a <- ma 
--        return $ f a


--instance Pipe2 (a -> b) a b where
--    pipe2 = ($)


--instance Pipe (a -> b) [a] [b] where
--    pipe = fmap

--instance Monad m => Pipe (a -> b) (m [a]) (m [b]) where
--    pipe f ma = do
--        a <- ma
--        return $ fmap f a

----------------------------------------------


newtype Context a = Context { runContext :: a } deriving Show


class Pipe3 a b c where
    pipe3 :: a -> b -> c


instance (c~a, out~(Context b)) => Pipe3 (a -> b) (Context c) out where
    pipe3 f (Context a) = Context $ f a

instance (a~c, out~b) => Pipe3 (a -> b) c out where
    pipe3 = ($)


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

--mulBy2 :: Int -> Int
mulBy2 = (*(2))


main = do
    --print $ blur `pipe` (Just "ala")
    print $ mulBy2 `pipe3` (2::Int)
    print $ mulBy2 `pipe3` (Context (2::Int))
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