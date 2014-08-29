{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State

-- lift :: Monad m => m a -> t m a

cls :: a
cls = undefined

ofType :: a -> a -> a
ofType = const

data Pure a = Pure { fromPure :: a } deriving (Show)

instance Monad Pure where
    return = Pure
    (Pure a) >>= f = f a

instance Functor Pure where
    fmap f (Pure a) = Pure (f a)

instance Applicative Pure where
    pure = Pure
    (Pure f) <*> (Pure a) = Pure $ f a

--test1 :: Pure Int
test1 = do
    return 5


test2 f = do
    f
    return 5

--main = do
--    print (test1 `ofType` (cls :: Pure a))
--    print (test2 test1 `ofType` (cls :: Pure a))


simpleIO :: IO Int
simpleIO = do
    print "hello"
    return 5

--testState :: StateT Int Pure Int
testState :: StateT Int IO Int
testState = do
    x <- get
    put (x+1)
    return 1

testIO :: IO Int
testIO = do
    x <- simpleIO
    return (x+1)

testIOState :: StateT Int IO Int
testIOState = do
    x <- testState
    y <- lift testIO
    return (x+y)


main = do
    print =<< runStateT testIOState 5





