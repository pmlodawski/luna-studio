{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

!{-# LANGUAGE RightSideContexts #-}

--{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE OverlappingInstances #-}

--{-# LANGUAGE DysfunctionalDependencies #-}


module Bind2 where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

import Luna.Target.HS.Monad

--import Control.Monad.Trans.State
import Luna.Target.HS.Utils.BaseMonads

import Control.Monad.Morph

------------------------------------------------------------------------------------------

class MonadMorph m n where
    morph :: m a -> n a

instance MonadMorph IO IO where
    morph = id

instance MonadMorph Pure Pure where
    morph = id

instance MonadMorph Pure IO where
    morph = return . fromPure



--instance Class Data where
--    func =


class BindEnv m1 m2 m3 | m1 m2 -> m3 where
    bindEnv :: m1 a -> (Pure a -> m2 b) -> m3 b

instance BindEnv Pure Pure Pure where
    bindEnv a f = f a

instance BindEnv IO Pure IO where
    bindEnv ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b

instance BindEnv Pure IO IO where
    bindEnv a f = f a

instance BindEnv IO IO IO where
    bindEnv ma f = do
        a <- ma
        f (Pure a)

-----


instance BindEnv (t1 Pure) (t2 Pure) (t1 Pure) <= (Monad (t1 Pure), t1~t2) where
    bindEnv ma f = do
        a <- ma
        f (Pure a)


instance BindEnv (t1 IO) (t2 IO) (t1 IO) <= (Monad (t1 IO), t1~t2) where
    bindEnv ma f = do
        a <- ma
        f (Pure a)

instance BindEnv (t1 Pure) (t2 IO) (t1 IO) <= (Monad (t1 IO), t1~t2, MFunctor t2) where
    bindEnv ma f = do
        a <- hoist morph ma
        f (Pure a)

instance BindEnv (t1 IO) (t2 Pure) (t1 IO) <= (Monad (t1 IO), t1~t2, MFunctor t2) where
    bindEnv ma f = do
        a <- ma
        hoist morph $ f (Pure a)

-----

instance BindEnv Pure (t Pure) (t Pure) where
    bindEnv a f = f a


instance BindEnv Pure (t IO) (t IO) where
    bindEnv a f = f a


instance BindEnv IO (t Pure) (t IO) <= (Monad (t IO), MonadTrans t, MFunctor t) where
    bindEnv ma f = do
        a <- lift ma
        hoist morph $ f (Pure a)


instance BindEnv IO (t IO) (t IO) <= (Monad (t IO), MonadTrans t) where
    bindEnv ma f = do
        a <- lift ma
        f (Pure a)

-----

instance BindEnv (t Pure) Pure (t Pure) <= Monad (t Pure) where
  bindEnv ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b


instance BindEnv (t Pure) IO (t IO) <= (Monad (t IO), MonadTrans t, MFunctor t) where
    bindEnv ma f = do
        a <- hoist morph ma
        lift $ f (Pure a)


instance BindEnv (t IO) Pure (t IO) <= Monad (t IO) where
    bindEnv ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b

instance BindEnv (t IO) IO (t IO) <= (Monad (t IO), MonadTrans t) where
    bindEnv ma f = do
        a <- ma
        lift $ f (Pure a)

-------------------------------

bindCtx :: ca ma a -> (Pure a -> cb mb b) -> (CtxMerge ca cb) mout b <= (BindEnv ma mb mout, Context ca, Context cb, Context(CtxMerge ca cb))
bindCtx a f = wrapCtx $ bindEnv (fromCtx a) (fromCtx . f)







-- dodac closed type family do bindCtx po to by determinowac ctx w rezultacie,
-- obecnie funkcja tst zwraca Context c => c IO Int!
-- a potem sprawdzic z reszta funkcji


--testIO :: IO Int
--testIO = do
--    liftIO $ print "dupa jeza"
--    return 5

--testStateT :: StateT Int IO Int
--testStateT = do
--    liftIO $ print "dupa jeza"
--    x <- get
--    put(x+1)
--    return 5


--testStateT2 :: Pure Int -> StateT Int Pure Int
--testStateT2 (Pure x) = do
--    return (x*2)


testPure :: Int :> Pure
testPure = return 5

testIO :: Int :> IO
testIO = return 5

--testStatePure :: IC2 (StateT Int Pure) Int
--testStatePure :: (MonadState t m, Num b, Monad (IC2 m)) => IC2 m b
testStatePure :: (MonadState t (t1 Pure), MonadTrans t1) => IC2 (t1 Pure) Int
testStatePure = do
    x <- get'
    IC2 $ lift (Pure (5::Int))


testStateIO :: (MonadState t (t1 IO), MonadTrans t1) => IC2 (t1 IO) Int
testStateIO = do
    x <- get'
    IC2 $ lift (return (5::Int) :: IO Int)

testReaderPure :: (MonadReader t (t1 Pure), MonadTrans t1) => IC2 (t1 Pure) Int
testReaderPure = do
    x <- ask'
    IC2 $ lift (Pure (5::Int))


tstM = do
    x <- get
    y <- ask
    return (5::Int)

--class (Monad m) => MonadState s m | m -> s where
--    get :: m s
--    put :: s -> m ()

--instance MonadState s (IC (t s) m) <= (MonadTrans (t s), Monad (t s m), Monad m, MonadState s (t s m)) where
ask' = IC2 $ ask
get' = IC2 $ get
put' = IC2 . put


tst = testPure `bindCtx` (\_ ->
        testIO
    )

tst2 = testPure `bindCtx` (\_ ->
       testStatePure
   )

tst3 = testPure `bindCtx` (\_ ->
       testStateIO
   )

tst4 = testStatePure `bindCtx` (\_ ->
       testStatePure
   )

tst5 = testStatePure `bindCtx` (\_ ->
       testStateIO
   )
--
-- tst6 = testStatePure `bindCtx` (\_ ->
--        testReaderPure
--    )


--printCtx :: Show s => s :> Pure -> () :> IO
--printCtx s = liftCtx $ print (fromPure $ unliftCtx s)


bindEnv_ a b = bindEnv a (\_ -> b)


main = do
    -- print $ runReaderT (runStateT (fromIC2 tst5) (0::Int)) (1::Int)
    --let x = testStateT `bindEnv` testStateT2
        --let x = testIO `bindEnv` testStateT2
        --print =<< runStateT x 5

    --let y = testPure `bind2` printCtx
    --unliftCtx y

    --let y = testStateT
    --    z = rebase y :: StateT Int IO Int
    print "end"






----------------------------------------

--class Bind2 m1 m2 m3 | m1 m2 -> m3 where
--    bind2 :: a :> m1 -> (a :> Pure -> b :> m2) -> b :> m3

--instance Bind2 Pure Pure Pure where
--    bind2 a f = f a

--instance Bind2 IO Pure IO where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
--        return b

--instance Bind2 Pure IO IO where
--    bind2 a f = f a

--instance Bind2 IO IO IO where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        unliftCtx $ f (liftCtx $ Pure a)

-----

--instance (Monad (m1 Pure), m1~m2) => Bind2 (m1 Pure) (m2 Pure) (m1 Pure) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        unliftCtx $ f (liftCtx $ Pure a)

--instance (Monad (m1 IO), m1~m2) => Bind2 (m1 IO) (m2 IO) (m1 IO) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        unliftCtx $ f (liftCtx $ Pure a)

--instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind2 (m1 Pure) (m2 IO) (m1 IO) where
--    bind2 ma f = liftCtx $ do
--        a <- rebase $ unliftCtx ma
--        unliftCtx $ f (liftCtx $ Pure a)

--instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind2 (m1 IO) (m2 Pure) (m1 IO) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        rebase . unliftCtx $ f (liftCtx $ Pure a)

-----

--instance Bind2 Pure (m Pure) (m Pure) where
--    bind2 a f = f a


--instance Bind2 Pure (m IO) (m IO) where
--    bind2 a f = f a


--instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind2 IO (m Pure) (m IO) where
--    bind2 ma f = liftCtx $ do
--        a <- lift . unliftCtx $ ma
--        rebase . unliftCtx $ f (liftCtx $ Pure a)


--instance (Monad (m IO), MonadTrans m) => Bind2 IO (m IO) (m IO) where
--    bind2 ma f = liftCtx $ do
--        a <- lift . unliftCtx $ ma
--        unliftCtx $ f (liftCtx $ Pure a)

-----

--instance Monad (m Pure) => Bind2 (m Pure) Pure (m Pure) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
--        return b


--instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind2 (m Pure) IO (m IO) where
--    bind2 ma f = liftCtx $ do
--        a <- rebase . unliftCtx $ ma
--        lift . unliftCtx $ f (liftCtx $ Pure a)


--instance Monad (m IO) => Bind2 (m IO) Pure (m IO) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx ma
--        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
--        return b

--instance (Monad (m IO), MonadTrans m) => Bind2 (m IO) IO (m IO) where
--    bind2 ma f = liftCtx $ do
--        a <- unliftCtx $ ma
--        lift . unliftCtx $ f (liftCtx $ Pure a)
