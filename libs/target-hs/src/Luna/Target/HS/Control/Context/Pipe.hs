---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverlappingInstances #-}
!{-# LANGUAGE RightSideContexts #-}

--{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Control.Context.Pipe where

import Control.PolyMonad
import Control.Applicative
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Luna.Target.HS.Control.Context.Monad
import Luna.Target.HS.Control.Context.App
import Data.TypeLevel
import Control.Monad.IO.Class
import Control.PolyApplicative.App
import Luna.Target.HS.Utils.BaseMonads
import Data.Typeable
import Luna.Target.HS.Control.Context.Bind
import Luna.Target.HS.Control.Context.MonadCtx

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class Pipe a b c | a b -> c where
    pipe :: a -> b -> c

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------




--class LiftCtx m mout where
--    liftCtx :: m a -> mout a

--instance LiftCtx IO m <= MonadIO m where
--    liftCtx = liftIO

--instance LiftCtx Pure m <= Monad m where
--    liftCtx = return . fromPure


class LiftCtx m where
    liftCtx :: MonadIO mout => m a -> mout a

instance LiftCtx IO where
    liftCtx = liftIO

instance LiftCtx Pure where
    liftCtx = return . fromPure


instance Pipe (Value m1 a1 -> b) (Value m2 a2) b <= (a1~a2, m1~m2) where
    pipe = ($)

--instance Pipe (MonadCtx env1 set m1 a1 -> b) (MonadCtx env2 set m2 a2) b <= (env1~env2, m1~m2, a1~a2) where
--    pipe = ($)

--instance Pipe (MonadCtx env1 set m1 a1 -> Value mb b) (MonadCtx env2 set m2 a2) (Value mb b) <= (env1~env2, m1~m2, a1~a2) where
--    pipe = ($)


instance Pipe (MonadCtx env set m2 a2 -> b) (Value m1 a1) b <= (a1~a2, env~m1, LiftValue m1 m2, Monad m1) where
    pipe f a = f . MonadCtx $ liftValue a

-- dziala dla (!) addMeSingleS' >>> getX (?)
--instance Pipe (MonadCtx env1 set1 m1 a1 -> mout b) (MonadCtx env2 set2 m2 a2) (MonadCtx env1 setout mout b) <= (env1~env2, m1~m2, a1~a2, setout ~ Difference set2 set1) where
--    pipe f a = MonadCtx (f . MonadCtx $ fromMonadCtx a)

instance Pipe (MonadCtx env1 set1 m1 a1 -> Value mb b) (MonadCtx env2 set2 m2 a2) (MonadCtx env1 setout mb b) <= (env1~env2, m1~m2, a1~a2, setout ~ Difference set2 set1) where
    pipe = undefined

(>>>) = pipe


liftf0 = Pure
liftf1 = app1 . Pure
liftf2 = app2 . Pure

--addMe :: Int -> Int -> Int
addMe = (+)

addMeSingle :: Int -> Int
addMeSingle a = a + a

addMe' = liftf2 addMe
addMeSingle' a = liftf1 addMeSingle $ fromValue a

addMeSingle'2 a = Value $ addMeSingle' a
addMeSingleS' (a :: MonadCtx env (Proxy StateT,()) (StateT Int mb) Int ) = addMeSingle'2 $ fmap fst $ runStateTX a (0::Int)

--instance MonadIO Pure where
--    liftIO = undefined

main = do
    let x = return 5 :: Value Pure Int
    print $ addMeSingle' >>> (Value $ Pure 1)
    print $ addMeSingleS' >>> getX
    print $ addMeSingleS' >>> (Value $ Pure 1)

    --print $ addMeSingleS' >>> (getX `bindEnv_` askX)

    --print $ addMeSingleS' >>> askX
    --print $ addMeSingleS' $ (MonadCtx $ liftValue (Value $ Pure (1::Int)))
    print "end" 



--instance PolyApplicative Pure Pure Pure where
--    Pure f <<*>> Pure a = Pure $ f a

--instance PolyApplicative IO Pure IO where
--    f <<*>> Pure a = f <*> return a

--instance PolyApplicative Pure IO IO where
--    Pure f <<*>> a = a >>= (return . f)

--instance PolyApplicative IO IO IO where
--    f <<*>> a = f <*> a

-----

--instance PolyApplicative Pure (MonadCtx env set m) (MonadCtx env set m) <= Functor m where
--    (Pure f) <<*>> a = f <$> a

--instance PolyApplicative (MonadCtx env set m) Pure (MonadCtx env set m) <= (Functor m, Monad m) where
--    f <<*>> (Pure a) = f <*> pure a


--instance PolyApplicative IO (MonadCtx env set m) (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
--    mf <<*>> ma = MonadCtx $ do
--        f <- liftIO mf
--        a <- fromMonadCtx ma
--        return $ f a

--instance PolyApplicative (MonadCtx env set m) IO (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
--    mf <<*>> ma = MonadCtx $ do
--        f <- fromMonadCtx mf
--        a <- liftIO ma
--        return $ f a

--instance PolyApplicative (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (envout ~ EnvMerge env1 env2, setout ~ Union set1 set2, m1~m2, Monad m1) where
--    mf <<*>> ma = MonadCtx $ do
--        f <- fromMonadCtx mf
--        a <- fromMonadCtx ma
--        return $ f a
