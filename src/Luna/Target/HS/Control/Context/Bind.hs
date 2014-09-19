---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
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


--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class MonadSafety m s1 s2 where
    bindSafety :: m s1 a -> (a -> m s2 b) -> m (MatchSafety s1 s2) b

class PolyApplicativeCtx m1 m2 m3 | m1 m2 -> m3 where
    appBindCtx :: m1 a -> m2 (XArg m1 a -> c) -> m3 c

class Monad3R m s where
    return3 :: (forall x. x -> s x) -> a -> m s a

wrap3 = return3 Safe

class LiftValue2 m t where
    liftValue2 :: Value m Safe a -> t Safe a


--------------------------------------------------------------------------------
-- CtxWrapper
--------------------------------------------------------------------------------
-- Context wrappers. They are used because of limitations of Haskell's type system.
-- eg: CtxWrapper (ValCtx m s) (MonadCtx ...) a <=> (MonadCtx ... (ValCtx m s a))
-- to read more about it, refer to: http://stackoverflow.com/questions/25854072/ambigous-instance-resolution-in-haskell/25881613#25881613

newtype CtxWrapper (w :: ((* -> *) -> * -> *)) m a  = CtxWrapper (w m a)
newtype AppCtx base a     = AppCtx (base a)
newtype ValCtx m s base a = ValCtx (base (Value m s a))

fromCtxWrapper (CtxWrapper a) = a
fromAppCtx     (AppCtx a)     = a
fromValCtx     (ValCtx a)     = a


class UnpackCtxWrapper a b | a -> b where
    unpackCtxWrapper :: a -> b

instance UnpackCtxWrapper (MonadCtx env set m s a) (MonadCtx env set m s a) where
    unpackCtxWrapper = id

instance UnpackCtxWrapper (Value m s a) (Value m s a) where
    unpackCtxWrapper = id

instance UnpackCtxWrapper (CtxWrapper w m a) (m v) <= UnpackW w a v where
    unpackCtxWrapper = unpackW . fromCtxWrapper

---

class UnpackW ctx a b | ctx a -> b where
    unpackW :: ctx base a -> base b

instance UnpackW AppCtx a a where
    unpackW = fromAppCtx

instance UnpackW (ValCtx m s) a (Value m s a) where
    unpackW = fromValCtx


--------------------------------------------------------------------------------
-- PolyMonadCtx
--------------------------------------------------------------------------------

class PolyMonadCtx m1 m2 where
    (>>>~) :: m1 a -> (XArg m1 a -> m2 c) -> (XOut m1 m2 c)

polyMonadCtxBind :: m1 a -> (XArg m1 a -> m2 c) -> XOut m1 m2 c <= PolyMonadCtx m1 m2
polyMonadCtxBind = (>>>~)

polyMonadCtxBind_ :: m1 a -> m2 c -> XOut m1 m2 c <= PolyMonadCtx m1 m2
polyMonadCtxBind_ a b = a >>>~ (\_ -> b)

type family XArg m a where
    XArg (MonadCtx env1 set1 m1 s1)    a = a
    XArg (Value m s)                   a = (Value Pure s a)
    XArg (CtxWrapper (ValCtx m' s') m) a = (Value m' s' a)
    XArg (CtxWrapper AppCtx m)         a = XArg m a

type family XOut m1 m2 where
    XOut (Value vbase s1)       (Value vbase' s2)      = Value (BottomEnvMerge vbase vbase') s2
    XOut (Value vbase vs)       (MonadCtx env set m s) = MonadCtx env set m s
    XOut (Value vbase vs)       (CtxWrapper w m)       = CtxWrapper w m
    XOut (MonadCtx env set m s) a                      = CtxWrapper (XWrapper a) (MonadCtx (XEnv env a) (XSet set a) m (XSafety s a))
    XOut (CtxWrapper w m)       a                      = XOut m a

type family XWrapper m where
    XWrapper (Value base s)         = ValCtx Pure s
    XWrapper (MonadCtx env set m s) = AppCtx
    XWrapper (CtxWrapper w m)       = w

type family XEnv a m where
    XEnv env (Value base s)          = env
    XEnv env (MonadCtx env' set m s) = EnvMerge3 env env'
    XEnv env (CtxWrapper w m)        = XEnv env m

type family XSet a m where
    XSet set (Value base s)          = set
    XSet set (MonadCtx env set' m s) = Union set set'
    XSet set (CtxWrapper w m)        = XSet set m

type family XSafety a m where
    XSafety s (Value base s')         = s
    XSafety s (MonadCtx env set m s') = MatchSafety s s'
    XSafety s (CtxWrapper w m)        = XSafety s m


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PolyMonad Pure Pure Pure where
    a >>>= f = f $ fromPure a

instance PolyMonad IO Pure IO where
    ma >>>= f = ma >>= return . fromPure . f

instance PolyMonad Pure IO IO where
    a >>>= f = f $ fromPure a

instance PolyMonad IO IO IO where
    a >>>= f = a >>= f

----------------------------------------------

instance PolyMonad (MonadCtx env1 set1 m1 s1) (MonadCtx env2 set2 m2 s2) (MonadCtx envout setout m1 s3) <= (envout~EnvMerge3 env1 env2, s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2, m1~m2, MonadSafety m2 s1 s2) where
    a >>>= f = MonadCtx $ (fromMonadCtx a) `bindSafety` (fromMonadCtx . f)

instance PolyMonad (MonadCtx env set m s1) (Value Pure s2) (MonadCtx env set m s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety m s1 s2, MonadTrans m, Monad s2) where
    a >>>= f = MonadCtx $ (fromMonadCtx a) `bindSafety` (lift . fromPure . fromValue . f)

instance PolyMonad (MonadCtx env set m s1) (Value IO s2) (MonadCtx envout set m s3) <= (envout~EnvMerge3 env (Value IO), s3 ~ MatchSafety s1 s2, MonadSafety m s1 s2, MonadTrans m, Monad s2, LiftValue IO m, Functor s2) where
    a >>>= f = MonadCtx $ (fromMonadCtx a) `bindSafety` (liftValue . f)

instance PolyMonad (Value Pure s1) (MonadCtx env set m s2) (MonadCtx env set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, MonadSafety m s1 s2, Monad s1) where
    a >>>= f = MonadCtx $ (lift . fromPure $ fromValue a) `bindSafety` (fromMonadCtx . f)

instance PolyMonad (Value IO s1) (MonadCtx env set m s2) (MonadCtx envout set m s3) <= (envout~EnvMerge3 (Value IO) env, s3 ~ MatchSafety s1 s2, MonadTrans m, MonadSafety m s1 s2, Monad s1, LiftValue IO m, Functor s1) where
    a >>>= f = MonadCtx $ (liftValue a) `bindSafety` (fromMonadCtx . f)

--instance PolyMonad IO (MonadCtx env set m) (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
--    a >>>= f = MonadCtx $ liftIO a >>= (fromMonadCtx . f)


instance PolyMonad (Value Pure s1) (Value Pure s2) (Value Pure s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value Pure) s1 s2) where
    a >>>= f = a `bindSafety` f

instance PolyMonad (Value IO s1) (Value Pure s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>>= f = a `bindSafety` (Value . return . fromPure . fromValue . f)

instance PolyMonad (Value Pure s1) (Value IO s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>>= f = (Value . return . fromPure . fromValue $ a) `bindSafety` f

instance PolyMonad (Value IO s1) (Value IO s2) (Value IO s3) <= (s3 ~ MatchSafety s1 s2, MonadSafety (Value IO) s1 s2) where
    a >>>= f = a `bindSafety` f

-----------------------------------------------------------

instance PolyMonadCtx (Value Pure s1) (Value base s2) where
    a >>>~ f = f a

instance PolyMonadCtx (Value IO s1) (Value Pure s2) where
    a >>>~ f = Value $ do
        a' <- fromValue a
        return . fromPure . fromValue $ f (Value . Pure $ a')

instance PolyMonadCtx (Value IO s1) (Value IO s2) where
    a >>>~ f = Value $ do
        a' <- fromValue a
        fromValue $ f (Value . Pure $ a')

-----

instance PolyMonadCtx (MonadCtx env1 set1 m1 s1) (MonadCtx env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a >>>~ f = CtxWrapper . AppCtx . MonadCtx $ (fromMonadCtx a) `bindSafety` (fromMonadCtx . f)

instance PolyMonadCtx (Value Pure s1) (MonadCtx env2 set2 m2 s2) where
    a >>>~ f = f a

instance PolyMonadCtx (Value IO s1) (MonadCtx env2 set2 m2 s2) <= (LiftValue2 IO m2, MonadSafety m2 Safe s2) where
    a >>>~ f = MonadCtx $ (liftValue2 . Value . fmap (Safe . Value . Pure) . fromValue $ a) `bindSafety` (fromMonadCtx . f)

instance PolyMonadCtx (MonadCtx env1 set1 m1 s1) (Value base s2) <= (MonadSafety m1 s1 Safe, LiftValue2 base m1, Functor base) where
    a >>>~ f = CtxWrapper . ValCtx . MonadCtx $ (fromMonadCtx a) `bindSafety` (liftValue2 . Value . fmap (Safe . Value . Pure) . fromValue . f)

-------

instance PolyMonadCtx (CtxWrapper (ValCtx m1' s1') (MonadCtx env1 set1 m1 s1)) (CtxWrapper (ValCtx m2' s2') (MonadCtx env2 set2 m2 s2)) <= (m1~m2, MonadSafety m2 s1 s2) where
    a >>>~ f = CtxWrapper . ValCtx . unpackCtxWrapper $ unpackCtxWrapper a >>>~ (unpackCtxWrapper . f)

instance PolyMonadCtx (Value base s1) (CtxWrapper (ValCtx m2' s2') (MonadCtx env2 set2 m2 s2)) <= PolyMonadCtx (Value base s1) (MonadCtx env2 set2 m2 s2) where
    a >>>~ f = CtxWrapper . ValCtx . unpackCtxWrapper $ a >>>~ (unpackCtxWrapper . f)

instance PolyMonadCtx (CtxWrapper (ValCtx m1' s1') (MonadCtx env1 set1 m1 s1)) (Value base s2) <= PolyMonadCtx (MonadCtx env1 set1 m1 s1) (Value base s2) where
    a >>>~ f = unpackCtxWrapper a >>>~ f

instance PolyMonadCtx (MonadCtx env1 set1 m1 s1) (CtxWrapper (ValCtx m2' s2') (MonadCtx env2 set2 m2 s2)) <= (m1~m2, MonadSafety m2 s1 s2) where
    a >>>~ f = CtxWrapper . ValCtx . unpackCtxWrapper $ a >>>~ (unpackCtxWrapper . f)

instance PolyMonadCtx (CtxWrapper (ValCtx m1' s1') (MonadCtx env1 set1 m1 s1)) (MonadCtx env2 set2 m2 s2) <= (m1~m2, MonadSafety m2 s1 s2) where
    a >>>~ f = (unpackCtxWrapper a) >>>~ f

