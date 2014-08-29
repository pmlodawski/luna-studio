{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

!{-# LANGUAGE RightSideContexts #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RebindableSyntax #-}

--{-# LANGUAGE DysfunctionalDependencies #-}




module Bind2 where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.PolyApplicative
import Control.PolyApplicative.App

import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Error

import Luna.Target.HS.Utils.BaseMonads

import Control.Monad.Morph
import Flowbox.Utils
import Data.Typeable (Typeable, Proxy(..))

import Data.TypeLevel
import Data.Wrap


import Luna.Target.HS.Control.Context.Rebindable
------------------------------------------------------------------------------------------


tst :: MonadCtx IO (Proxy StateT,(Proxy ReaderT, ())) m () <= (MonadIO m, MonadState s0 m, MonadReader a0 m, Num s0)
tst = do
    Pure x <- getX
    y <- askX
    returnIO 1
    putX (x+1)


tst2 :: MonadCtx IO (Proxy StateT,(Proxy ReaderT, ())) m (Safe Int) <= (MonadIO m, MonadState s0 m, MonadReader a0 m)
tst2 = do
    Pure x <- getX
    y <- askX
    returnIO (Safe 5)

--addMe :: Int -> Int -> Int
addMe = (+)

concatMe = (++)


val = Pure . Safe



--liftenv2 f a b   = liftenv (liftenv f a) b
--liftenv3 f a b c = liftenv (liftenv2 f a b) c

--liftErr1 f a = f <<*>> a

--app2f2 f g = app2 . f . app2  . g

liftf0 = Pure . Safe
liftf1 = app1 . Pure . app1  . Safe
liftf2 = app2 . Pure . app2  . Safe
--liftf2 = liftenv2 . Pure . liftErr2 . Safe
--liftf3 = liftenv3 . Pure . liftErr3 . Safe

--main = print $ liftf2 addMe (Pure(Safe(1))) (Pure(Safe(1)))
--main = print =<< ( (flip runReaderTX 0) $ (flip runStateTX 10) $ liftf2 addMe tst2 tst2 )

main = print $ liftf2 concatMe (Pure(Safe([1]))) (Pure(Safe([2])))

--main = print =<< runStateTX (runReaderTX tst 5) 0

--main = print $ ( (val addMe) <<*>> (val 1) <<*>> (val 1))
--main = print $ (flip runStateTX 0) ( (val addMe) <<*>> (val 1) <<*>> (val 1))
--newtype MonadCtx (base :: * -> *) set m val = MonadCtx (m val) deriving (Show, Typeable)


--main = do
--    --print $ runStateTX getX 0
--    print =<< ( flip runStateTX 0 $ getX `bindEnv_` (return 5 :: IO Int) )

--    print $ flip runStateTX 0 $ getX `bindEnv_` getX
--    print $ flip runReaderTX (5::Int) $ flip runStateTX (0::Int) $ getX `bindEnv_` askX
--    print $ flip runReaderTX 5 $ flip runStateTX 0 $ getX `bindEnv_` askX `bindEnv_` (return 1 :: Num a => Pure a)
--    print =<< (flip runStateTX 0 $ flip runReaderTX 0 $ getX `bindEnv_` askX `bindEnv_` (return 1 :: Num a => IO a))
--    print $ flip runReaderTX 5 $ flip runStateTX 0 $ (return 1 :: Num a => Pure a) `bindEnv_` getX `bindEnv_` askX
--    print =<< (flip runStateTX 0 $ (return 1 :: Num a => IO a) `bindEnv_` getX)

--       print $ (flip runStateTX 0 $ flip runReaderTX 0 $ (return 1 :: Pure Int) `bindEnv_` getX `bindEnv_` askX)
--    print =<< (flip runStateTX 0 $ flip runReaderTX 0 $ (return 1 :: Num a => IO a) `bindEnv_` getX `bindEnv_` askX)

--    print =<< ( flip runReaderTX 5 $ flip runStateTX 0 $ getX `bindEnv_` askX `bindEnv_` (return 1 :: IO Int) )
--        --print $ tstM3
--    -- printType $ getX `bindEnv_` askX
--    -- printType $ (undefined :: Pure Int) `bindEnv_` (undefined :: Pure Int)
--    -- printType $ (undefined :: StateT Int Pure Int) `bindEnv_` (undefined :: StateT Int Pure Int)
--    -- printType $ (undefined :: StateT Int Pure Int) `bindEnv_` (undefined :: Pure Int)
--    -- printType $ (undefined :: StateT Int Pure Int) `bindEnv_` (undefined :: ReaderT Int Pure Int)

--    putStrLn "----"

--    -- printType $ findBase (undefined :: Pure Int)
--    -- print $ runStateT (fStatePure `bindEnv_` fStatePure) (0::Int)

--    print "end"




--testStatePure :: (MonadState t (t1 Pure), MonadTrans t1) => IC2 (t1 Pure) Int
--testStatePure = do
--    x <- get'
--    IC2 $ lift (Pure (5::Int))

----
--testReaderPure :: (MonadReader t (t1 Pure), MonadTrans t1) => IC2 (t1 Pure) Int
--testReaderPure = do
--    x <- ask'
--    IC2 $ lift (Pure (5::Int))

----instance MonadState s (IC (t s) m) <= (MonadTrans (t s), Monad (t s m), Monad m, MonadState s (t s m)) where
--ask' = IC2 $ ask
--get' = IC2 $ get
--put' = IC2 . put



