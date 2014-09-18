---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.Bind where

import Control.Monad.Trans
import Control.PolyMonad
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.MonadCtx
import Luna.Target.HS.Control.Context.Value
import Data.TypeLevel
import Control.Monad.IO.Class

import Luna.Target.HS.Control.Error


class Monad3T m where
    return3T :: Functor s => s a -> m s a

class MonadSafety m s1 s2 where
    bindSafety :: m s1 a -> (a -> m s2 b) -> m (MatchSafety s1 s2) b




class PolyCtx m1 m2 where
    polyCtxBind :: m1 a -> (X1 m1 a -> m2 c) -> (XOut m1 m2) c

polyCtxBind_ a b = a `polyCtxBind` (\_ -> b)


type family XOut m1 m2 where
    XOut (Value base s1)                    (Value base' s2)                   = Value (EnvMerge base base') s2
    XOut (Value vbase vs)                   (MonadCtx2 env set m s)             = MonadCtx2 (XEnv env (Value vbase vs)) (XSet set (Value vbase vs)) m (XSafety s (Value vbase vs))
    XOut (Value vbase vs)                   (MonadCtx2Dummy' m2 s2 env set m s) = MonadCtx2Dummy' m2 s2 (XEnv env (Value vbase vs)) (XSet set (Value vbase vs)) m (XSafety s (Value vbase vs))
    XOut (MonadCtx2 env set m s)             a                                   = (XMonad a) (XEnv env a) (XSet set a) m (XSafety s a)
    XOut (MonadCtx2Dummy' m2 s2 env set m s) a                                   = XOut (MonadCtx2 env set m s) a


-- NOT USED - just a test
type family GetEnv2 m where
    GetEnv2 (Value base s)         = Nothing
    GetEnv2 (MonadCtx2 env set m s) = Just env


type family XMonad m where
    XMonad (Value base s)                     = MonadCtx2Dummy' Pure s
    XMonad (MonadCtx2 env set m s)             = MonadCtx2
    XMonad (MonadCtx2Dummy' m2 s2 env set m s) = MonadCtx2Dummy' m2 s2

type family XEnv a m where
    XEnv env (Value base s)                      = env
    XEnv env (MonadCtx2 env' set m s)             = EnvMerge3 env env'
    XEnv env (MonadCtx2Dummy' m2 s2 env' set m s) = EnvMerge3 env env'


type family XSet a m where
    XSet set (Value base s)                      = set
    XSet set (MonadCtx2 env set' m s)             = Union set set'
    XSet set (MonadCtx2Dummy' m2 s2 env set' m s) = Union set set'

type family XSafety a m where
    XSafety s (Value base s')                     = s
    XSafety s (MonadCtx2 env set m s')             = MatchSafety s s'
    XSafety s (MonadCtx2Dummy' m2 s2 env set m s') = MatchSafety s s'

class PolyApplicative5 m1 m2 m3 | m1 m2 -> m3 where
    appBind5 :: m1 a -> m2 (X1 m1 a -> c) -> m3 c
    
--y_tstme x = x `polyCtxBind` (\_ -> valS' 0)
--y_tstme2 = (valS' 1) `polyCtxBind` (\_ -> valS' (0::Int))
--y_tstme3 = (valS' (1::Int)) `polyCtxBind` (\_ -> valS' 0)
--y_tstme4 = (valS' 1) `polyCtxBind` (\_ -> valS' 0)
--y_tstme5 x y = x `polyCtxBind` (\_ -> y `polyCtxBind` (\_ -> valS' 0))

type family X1 m a where
    X1 (MonadCtx2 env1 set1 m1 s1)          a = a
    X1 (Value m s)                         a = (Value Pure s a)
    X1 (MonadCtx2Dummy' m' s' env set m s)  a = (Value m' s' a)







class Monad3R m s where
    return3 :: (forall x. x -> s x) -> a -> m s a

wrap3 = return3 Safe

--valS' = Value . Pure . Safe

--valS'io :: a -> Value IO Safe a
--valS'io = Value . return . Safe


--x_tstme x = x `polyCtxBind` (\_ -> valS' 0)
--x_tstme2 = (valS' 1) `polyCtxBind` (\_ -> valS' (0::Int))
--x_tstme3 = (valS' (1::Int)) `polyCtxBind` (\_ -> valS' 0)
--x_tstme4 = (valS' 1) `polyCtxBind` (\_ -> valS' 0)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PolyMonad Pure Pure Pure where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO Pure IO where
    ma >>=~ f = ma >>= return . fromPure . f

instance PolyMonad Pure IO IO where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO IO IO where
    a >>=~ f = a >>= f

---


class MonadIOS m where
    liftIOS :: IOS s a -> m Safe (s a)

----------------------------------------------

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2, m1~m2, MonadSafety m2 s1 s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (fromMonadCtx2 . f)

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env set m s1) (Value Pure s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety m s1 s2, MonadTrans m, Monad s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (lift . fromPure . fromValue . f)

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env set m s1) (Value IO s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety m s1 s2, MonadTrans m, Monad s2, LiftValue IO m, Functor s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (liftValue . f)

instance PolyMonad (Value Pure s1) (MonadCtx2 env set m s2) (MonadCtx2 env set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, MonadSafety m s1 s2, Monad s1) where
    a >>=~ f = MonadCtx2 $ (lift . fromPure $ fromValue a) `bindSafety` (fromMonadCtx2 . f)

instance PolyMonad (Value IO s1) (MonadCtx2 env set m s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, MonadSafety m s1 s2, Monad s1, LiftValue IO m, Functor s1) where
    a >>=~ f = MonadCtx2 $ (liftValue a) `bindSafety` (fromMonadCtx2 . f)

--instance PolyMonad IO (MonadCtx env set m) (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
--    a >>=~ f = MonadCtx $ liftIO a >>= (fromMonadCtx . f)


instance PolyMonad (Value Pure s1) (Value Pure s2) (Value Pure s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value Pure) s1 s2) where
    a >>=~ f = a `bindSafety` f

instance PolyMonad (Value IO s1) (Value Pure s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>=~ f = a `bindSafety` (Value . return . fromPure . fromValue . f)

instance PolyMonad (Value Pure s1) (Value IO s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>=~ f = (Value . return . fromPure . fromValue $ a) `bindSafety` f

instance PolyMonad (Value IO s1) (Value IO s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>=~ f = a `bindSafety` f

--instance PolyMonad Pure IO IO where
--    a >>=~ f = f $ fromPure a

--instance PolyMonad IO IO IO where
--    a >>=~ f = a >>= f




class LiftValue2 m t where
    liftValue2 :: Value m Safe a -> t Safe a



-----------------------------------------------------------

instance PolyCtx (Value Pure s1) (Value Pure s2) where
    a `polyCtxBind` f = f a

instance PolyCtx (Value Pure s1) (Value IO s2) where
    a `polyCtxBind` f = f a

instance PolyCtx (Value IO s1) (Value Pure s2) where
    a `polyCtxBind` f = Value $ do
        a' <- fromValue a
        return . fromPure . fromValue $ f (Value . Pure $ a')

instance PolyCtx (Value IO s1) (Value IO s2) where
    a `polyCtxBind` f = Value $ do
        a' <- fromValue a
        fromValue $ f (Value . Pure $ a')

---

instance PolyCtx (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a `polyCtxBind` f = MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (fromMonadCtx2 . f)

instance PolyCtx (Value Pure s1) (MonadCtx2 env2 set2 m2 s2) where
    a `polyCtxBind` f = f a

instance PolyCtx (Value IO s1) (MonadCtx2 env2 set2 m2 s2) <= (LiftValue2 IO m2, MonadSafety m2 Safe s2) where
    a `polyCtxBind` f = MonadCtx2 $ (liftValue2 . Value . fmap (Safe . Value . Pure) . fromValue $ a) `bindSafety` (fromMonadCtx2 . f)

instance PolyCtx (MonadCtx2 env1 set1 m1 s1) (Value IO s2) <= (MonadSafety m1 s1 Safe, LiftValue2 IO m1) where
    a `polyCtxBind` f = MonadCtx2Dummy' . MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (liftValue2 . Value . fmap (Safe . Value . Pure) . fromValue . f)

instance PolyCtx (MonadCtx2 env1 set1 m1 s1) (Value Pure s2) <= (MonadSafety m1 s1 Safe, LiftValue2 Pure m1) where
    a `polyCtxBind` f = MonadCtx2Dummy' . MonadCtx2 $ (fromMonadCtx2 a) `bindSafety` (liftValue2 . Value . fmap (Safe . Value . Pure) . fromValue . f)

-----

instance PolyCtx (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a `polyCtxBind` f = MonadCtx2Dummy' $ fromMonadCtx2Dummy' a `polyCtxBind` (fromMonadCtx2Dummy' . f)

instance PolyCtx (Value Pure s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) where
    a `polyCtxBind` f = MonadCtx2Dummy' $ a `polyCtxBind` (fromMonadCtx2Dummy' . f)

instance PolyCtx (Value IO s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (MonadSafety m2 Safe s2, LiftValue2 IO m2) where
    a `polyCtxBind` f = MonadCtx2Dummy' $ a `polyCtxBind` (fromMonadCtx2Dummy' . f)

instance PolyCtx (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (Value IO s2) <= (MonadSafety m1 s1 Safe, LiftValue2 IO m1) where
    a `polyCtxBind` f = fromMonadCtx2Dummy' a `polyCtxBind` f

instance PolyCtx (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (Value Pure s2) <= (MonadSafety m1 s1 Safe, LiftValue2 Pure m1) where
    a `polyCtxBind` f = fromMonadCtx2Dummy' a `polyCtxBind` f

instance PolyCtx (MonadCtx2 env1 set1 m1 s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a `polyCtxBind` f = MonadCtx2Dummy' $ a `polyCtxBind` (fromMonadCtx2Dummy' . f)

instance PolyCtx (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a `polyCtxBind` f = (fromMonadCtx2Dummy' a) `polyCtxBind` f


newtype MonadCtx2Dummy' m2 s2 env set m1 s1 a = MonadCtx2Dummy' (MonadCtx2 env set m1 s1 (Value m2 s2 a))

fromMonadCtx2Dummy' (MonadCtx2Dummy' a) = a



class UnpackMonadCtxDummy a b | a -> b where
    unpackMonadCtxDummy :: a -> b

instance UnpackMonadCtxDummy (MonadCtx2Dummy' m' s' env set m s a) (MonadCtx2 env set m s (Value m' s' a)) where
    unpackMonadCtxDummy = fromMonadCtx2Dummy'

instance UnpackMonadCtxDummy (MonadCtx2 env set m s a) (MonadCtx2 env set m s a) where
    unpackMonadCtxDummy = id

instance UnpackMonadCtxDummy (Value m s a) (Value m s a) where
    unpackMonadCtxDummy = id
