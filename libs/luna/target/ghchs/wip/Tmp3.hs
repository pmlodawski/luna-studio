{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

--class Fst' a b | a -> b where
--    fst' :: a -> b

--instance Fst' (a,b) a where
--    fst' ~(a, b) = a


data Foo a = Foo a deriving Show

--class Fst' a b out | a -> out where
--    fst' :: (a,b) -> out

--instance Fst' a b a where
--    fst' = undefined

data Nop = Nop deriving Show

class MkNop m where
    mkNop :: m a -> Nop

instance MkNop Foo where
    mkNop _ = Nop

--map2 f (a,b) = (a,f b)

test (x :: (a,b)) = 5

main = do
    print $ test (1,return 5)
    --print $ mkNop $ Foo (return 2)

    print "end"
    --print $ fst' (1,2)
