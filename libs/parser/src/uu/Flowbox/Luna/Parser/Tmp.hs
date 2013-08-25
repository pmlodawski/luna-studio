{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.Parser.Tmp where

--import Debug.Trace


--f = trace("f") 5

--g x = trace("g") 5

--main = do
--    let a = 5
--        x = 1 :: Int
--        y = 2 :: Int
--    --print $ f
--    --print $ f
--    --print $ g 0
--    --print $ g 0
    
--    print $ add (x,(y,()))
--    print $ add (x,())
--    return ()


--class Cadd a b | a -> b where
--    add :: a -> b

--instance Cadd (Int,(Int,())) (Int,()) where
--    add (x,(y,())) = (x+y,())

--instance Cadd (Int,()) (Int,()) where
--    add (x,()) = add (x,(0::Int,()))