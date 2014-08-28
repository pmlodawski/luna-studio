{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE PolyKinds                 #-}


module Data3 where

import Bind2 (StateT (..), get, put)

import Control.Applicative
import Control.Monad.Trans

-------------------------------

data Pure a = Pure a deriving (Show)

fromPure (Pure a) = a

instance Monad Pure where
    return = Pure
    (Pure a) >>= f = f a

instance Functor Pure where
    fmap f (Pure a) = Pure (f a)

instance Applicative Pure where
    pure = Pure
    (Pure f) <*> (Pure a) = Pure $ f a

-------------------------------


--newtype a :> m = InContext (m a) deriving Show

--ctxmap f (InContext a) = InContext $ fmap f a

--liftCtx                 = InContext
--unliftCtx (InContext a) = a


--ctxPure :: a -> a :> Pure
--ctxPure = liftCtx . Pure

--ctxIO :: a -> a :> IO
--ctxIO = liftCtx . return

-------------------------------

newtype Value m v = Value (m v) deriving Show

fromValue (Value a) = a

instance Monad m => Monad (Value m) where
    return = Value . return
    Value ma >>= f = Value $ do
        a <- ma
        fromValue $ f a

instance Functor m => Functor (Value m) where
    fmap f (Value a) = Value $ fmap f a

instance (Functor m, Monad m) => Applicative (Value m) where
    pure  = Value . return
    (Value mf) <*> (Value ma) = Value $ do
        f <- mf
        a <- ma
        return $ f a

-------------------------------

newtype IC t (m :: * -> *) v = IC { fromIC :: t m v }

instance (Monad m, Monad (t m), MonadTrans t) => Monad (IC t m) where
    return = IC . return
    IC tma >>= f = IC $ do
        a <- tma
        fromIC $ f a


--runIC :: (Monad m, Monad (t m), MonadTrans t) => IC t m v -> t m v
--runIC (IC tma) = do
--    Value ma <- tma
--    a        <- lift ma
--    return a


--icmap :: (Functor (t m)) => (Value m a -> Value m b) -> IC t m a -> IC t m b
--icmap f (IC a) = IC $ fmap f a


--icmap2 :: (Functor (t m)) => (Value m a -> Value m b) -> IC t m a -> IC t m b
--icmap2 :: (Monad m, Monad (t m), MonadTrans Value) => (Value Pure a -> b) -> IC Value m a -> IC t m b
--icmap2 :: (Monad (t m), MonadTrans t, Monad m, Monad mm) => (Value mm a -> Value m b) -> IC t m a -> IC t m b
--icmap2 f ca = IC $ do
--    a <- (runIC ca)
--    return $ f $ (Value . return) a

instance (Functor (t m), Functor m) => Functor (IC t m) where
    fmap f (IC a) = IC $ fmap f a

instance (Functor (t m), Functor m) => Applicative (IC t m) where
    pure = undefined
    (<*>) = undefined

liftCtx :: (MonadTrans t, Monad m) => Value m a -> IC t m a
liftCtx = IC . lift . fromValue

-------------------------------


type (:>) v m = Value m v

    --data P a = P a deriving Show

    --instance Monad P where
    --    return = P
    --    (P a) >>= f = f a

    --test :: StateT Int P Int
    --test = return 5

    --incme :: StateT Int P ()
    --incme = do
    --    x <- get
    --    put (x+1)

    --mulme :: StateT Int P ()
    --mulme = do
    --    x <- get
    --    put (x*3)

    --t2 :: StateT Int P ()
    --t2 = do
    --    incme


    --t2' :: StateT Int P ()
    --t2' = do
    --    mulme

    --t3 = liftCtx (Value t2)
    --t3' = liftCtx (Value t2')

    --t4 :: IC (StateT Int) (StateT Int P) ()
    --t4 = do
    --    t3
    --    t3'

    --main = do
    --    print $ runStateT t2 0
    --    print $ runStateT (runStateT (runIC t4) 7) 7

--type (:>) (Value mv) m = m

--test :: Value Pure Int
--test :: Int :> Pure
--test = Value $ Pure 5


--type family (:>>) a (b :: k) where
--    Value m v :>> t  = IC t m v
--    IC t m v  :>> t2 = IC t2 (t m) v
--    a :>> b = Value b a



--liftCtx :: Monad (t m) => Value m a -> IC t m a
--liftCtx = IC . return




--test :: Int :> IO
--test = liftCtx $ Pure (5::Int)


--test :: Value IO Int
--test = Value $ do
--    print "test"
--    return 5


--testState :: IC (StateT Int) IO Int
--testState = do
--    --liftCtx $ test
--    liftCtx $ (Value $ print "hello")
--    return 15


--main = do

--    print =<< runStateT (runIC testState) 0

--    print "end"
