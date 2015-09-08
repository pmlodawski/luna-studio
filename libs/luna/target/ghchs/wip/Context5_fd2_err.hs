{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------

data Pure a = Pure a deriving Show

instance Monad Pure where
    return = Pure
    (Pure a) >>= f = f a

class Pipe a b c | a b -> c where
    pipe :: a -> b -> c

-- FOLLOWING INSTANCE WORKS
--instance Pipe (a -> b) (StateT Int m2 a2) (StateT Int m2 b)  where
--    pipe f ca = undefined

--THIS ONE DOES NOT:
instance Pipe (a -> b) (c m2 a2) (c m2 b)  where
    pipe f ca = undefined

-- because ghc infers c as (* -> * -> *)
-- where it should be      (* -> (* -> *) -> *) instead

testMe :: Int -> Int
testMe a = a

time ::  StateT Int Pure Int
time = return 5

main = do
    let f = testMe `pipe` time :: StateT Int Pure Int
    print "end"
