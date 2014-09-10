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

{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.Bind where

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

class Monad4 m s1 s2 where
    bind4 :: m s1 a -> (a -> m s2 b) -> m (MatchSafety s1 s2) b


class PolyMonad2 a b c | a b -> c where
    polyBind2 :: a -> b -> c


class PolyMonad3 a b c d | a -> b, a c -> d where
    polyBind3 :: a -> (b -> c) -> d


polyBind2Env :: PolyMonad2 a (s v -> c1) c => a -> (Value2 PureS s v -> c1) -> c
polyBind2Env a f = a `polyBind2` (f . Value2 . PureS . Pure)

--polyBind2Env :: PolyMonad2 a (s v -> c1) c => a -> (Value2 PureS s v -> c1) -> c
polyBind2Env_ (a :: Value2 PureS s a) b = a `polyBind2` (\(_ :: s a) -> b)


polyBind3Env a f = a `polyBind3` (f . Value2 . PureS . Pure)
polyBind3Env_ a b = a `polyBind3` (\_ -> b)

class Monad3R m s where
    return3 :: (forall x. x -> s x) -> a -> m s a

wrap3 = return3 Safe

--xxx :: PolyMonad2 a b c => a -> b -> c
--xxx t = polyBind2Env t

val2' = Value2 . PureS . Pure . Safe

--xxx x = x `polyBind2` (\_ -> val2' (0::Int))

xxx x = x `polyBind3` (\x' -> (Value2 . PureS . Pure) x')

----------------------------------------------------------------------------------
---- Utils
----------------------------------------------------------------------------------
bindEnv :: (PolyBindEnv m1 m2 m3 a1 a2, PolyMonad m1 m2 m3) =>
           m1 a1 -> (Value Pure a2 -> m2 b) -> m3 b
bindEnv = polyBindEnv

--bindEnv_ (a :: m1 a1) (b :: m2 b) = a `polyBindEnv` ((\_ -> b) :: (Value Pure a1 -> m2 b))
bindEnv_ a b = a `polyBindEnv` (\_ -> b)

--bindEnv_ (a :: m1 a1) (b :: m2 b) = a `polyBindEnv` ((\_ -> b) :: (Value Pure a1 -> m2 b))


--bindEnv :: m1 a -> (Value Pure a -> m2 b) -> m3 b <= PolyMonad m1 m2 m3
--bindEnv a f = a >>=~ (f . Value . Pure)

--bindEnv_ a b = bindEnv a (\_ -> b)



class PolyBindEnv m1 m2 m3 a1 a2 | m1 m2 -> m3, m1 a1 -> a2 where
    polyBindEnv :: PolyMonad m1 m2 m3 => m1 a1 -> (Value Pure a2 -> m2 b) -> m3 b


instance PolyBindEnv Pure m2 m3 a1 a2 <= (a1~a2) where
    a `polyBindEnv` f = a >>=~ (f . Value . Pure)

instance PolyBindEnv IO m2 m3 a1 a2 <= (a1~a2) where
    a `polyBindEnv` f = a >>=~ (f . Value . Pure)

instance PolyBindEnv (MonadCtx env set m) m2 m3 a1 a2 <= (a1 ~ Value Pure a2) where
    a `polyBindEnv` f = a >>=~ f

instance PolyBindEnv (Value m1) m2 m3 a1 a2 <= (a1~a2) where
    a `polyBindEnv` f = a >>=~ (f . Value . Pure)


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
instance PolyMonad (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, m1~m2, s3~MatchSafety s1 s2, Monad4 m1 s1 s2) where
    --(>>=~) = undefined
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad (MonadCtx2 env set m1 s1) (Value2 m2 s2) mout <= PolyMonad (MonadCtx2 env set m1 s1) (m2 s2) mout where
    a >>=~ f = a >>=~ (fromValue2 . f)

--instance PolyMonad (MonadCtx2 env set m s1) (PureS s2) (MonadCtx2 envout set m s3) <= (envout~EnvMerge2 env PureS, s3~MatchSafety s1 s2, Monad4 m s1 s2, Monad3T m, Functor s2) where
--    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (return3T . fromPure . fromPureS . f)

--- >>=~ ma zla sygnature! np.
--- :t putX (val (1::Int)) >>=~ (\_ -> (val (1::Int)))
--- zmienia sygnature! z Pure Safe Int na Int !

    --instance PolyMonad2 (MonadCtx2 env set m s1 a1) (a2 -> Value2 m2 s2 b) (MonadCtx2 env set m s1 out) <= (Functor (m s1), PolyMonad2 a1 (a2 -> Value2 m2 s2 b) out) where
    --    polyBind2 m f = fmap (flip polyBind2 f) m



--instance PolyMonad2 (Value2 PureS s1 a1) (a2 -> Value2 PureS s2 b) (Value2 PureS s3 b) <= (a1~a2, PolyMonad s1 s2 s3) where
--    polyBind2 m f = Value2 $ PureS $ Pure $ (fromPure $ fromPureS $ fromValue2 m) >>=~ (fromPure . fromPureS . fromValue2 . f)

--instance PolyMonad2 (Value2 IOS s1 a1) (a2 -> Value2 PureS s2 b) (Value2 IOS s3 b) <= (a1~a2, PolyMonad s1 s2 s3) where
--    polyBind2 m f = Value2 $ PureS $ Pure $ (fromIOS $ fromValue2 m) >>=~ (fromPure . fromPureS . fromValue2 . f)


---


instance PolyMonad (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (envout ~ EnvMerge env1 env2, setout ~ Union set1 set2, m1~m2, Monad m1) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (fromMonadCtx . f)

instance PolyMonad (MonadCtx env set m) Pure (MonadCtx envout set m) <= (envout ~ EnvMerge env Pure, Monad m) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (return . fromPure . f)

instance PolyMonad (MonadCtx env set m) IO (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (liftIO . f)

instance PolyMonad Pure (MonadCtx env set m) (MonadCtx env set m) where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO (MonadCtx env set m) (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
    a >>=~ f = MonadCtx $ liftIO a >>= (fromMonadCtx . f)

---

--instance PolyMonad (Value m1) m2 mout <= PolyMonad m1 m2 mout where
--    a >>=~ f = fromValue a >>=~ f

--instance PolyMonad m1 (Value m2) mout <= PolyMonad m1 m2 mout where
--    a >>=~ f = a >>=~ (fromValue . f)

instance PolyMonad (Value m1) (MonadCtx env set m2) mout <= PolyMonad m1 (MonadCtx env set m2) mout where
    a >>=~ f = fromValue a >>=~ f

instance PolyMonad (MonadCtx env set m1) (Value m2) mout <= PolyMonad (MonadCtx env set m1) m2 mout where
    a >>=~ f = a >>=~ (fromValue . f)

instance PolyMonad (Value m1) (Value m2) out <= (out~(Value mout), PolyMonad m1 m2 mout) where
    a >>=~ f = Value (fromValue a >>=~ (fromValue . f))




instance PolyMonad2 (MonadCtx2 env1 set1 m1 s1 a) (t -> MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout setout m1 s3 b) <= (m1~m2, t~a, envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
    a `polyBind2` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)


instance PolyMonad2 (PureS s1 a) (t -> MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 env2 set2 m2 s2 b) <= (t~s1 a) where
    a `polyBind2` f = f (fromPure $ fromPureS a)


instance PolyMonad2 (IOS s1 a) (t -> MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout set2 m2 s2 b) <= (t~s1 a, envout ~ EnvMerge2 env2 IOS, MonadIOS m2, Monad4 m2 Safe s2) where
    a `polyBind2` f = MonadCtx2 $ liftIOS a `bind4` (fromMonadCtx2 . f)

instance PolyMonad2 (MonadCtx2 env1 set1 m1 s1 a) (t -> IOS s2 b) (MonadCtx2 env1 set1 m1 s1 (Value2 IOS s2 b)) <= (t~a, Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    a `polyBind2` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . Value2 . f)

instance PolyMonad2 (MonadCtx2 env1 set1 m1 s1 a) (t -> PureS s2 b) (MonadCtx2 env1 set1 m1 s1 (Value2 PureS s2 b)) <= (t~a, Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    a `polyBind2` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . Value2 . f)

--poprawic to wyzej! bind4 ma zhardcodowane, ze t~a, co nie jest prawd powyzej!
--poprawka: niepoprawna jest implkementacja liftIOS! 
--liftIOS :: IOS! Safe! Int! -> MonadCtx2 IOS! (...) m Safe (Value2 PureS Safe! Int!)

--class MonadIOS m where
--    liftIOS :: IOS s a -> m s a


            --liftIOS powinno miec inna implementacje chyba! Powinna zwracac (m Safe (Value2 PureS s a)) !
--class MonadIOS m where
--    liftIOS :: IOS s a -> m Safe (Value2 PureS s a)

class MonadIOS m where
    liftIOS :: IOS s a -> m Safe (s a)



---

instance PolyMonad2 (MonadCtx2 env1 set1 m1 s1 a) (a2 -> Value2 m2 s2 b) out <= PolyMonad2 (MonadCtx2 env1 set1 m1 s1 a) (a2 -> m2 s2 b) out where
    a `polyBind2` f = a `polyBind2` (fromValue2 . f)


instance PolyMonad2 (Value2 m1 s1 a1) (a2 -> MonadCtx2 env set m2 s2 b) mout <= (PolyMonad2 (m1 s1 a1) (a2 -> MonadCtx2 env set m2 s2 b) mout) where
    a `polyBind2` f = fromValue2 a `polyBind2` f


instance PolyMonad2 (Value2 m1 s1 a1) (a2 -> Value2 m2 s2 b) (Value2 m s v) <= (PolyMonad2 (m1 s1 a1) (a2 -> m2 s2 b) (m s v)) where
    a `polyBind2` f = Value2 (fromValue2 a `polyBind2` (fromValue2 . f))

---

instance PolyMonad2 (PureS s1 a1) (t -> PureS s2 b) (PureS s2 b) <= (t~s1 a1) where
    a `polyBind2` f = f (fromPure $ fromPureS a)

instance PolyMonad2 (PureS s1 a1) (t -> IOS s2 b) (IOS s2 b) <= (t~s1 a1) where
    a `polyBind2` f = f (fromPure $ fromPureS a)


instance PolyMonad2 (IOS s1 a1) (t -> IOS s2 b) (IOS s2 b) <= (t~s1 a1) where
    a `polyBind2` f = IOS $ do
        t <- (fromIOS a)
        fromIOS $ f t

instance PolyMonad2 (IOS s1 a1) (t -> PureS s2 b) (IOS s2 b) <= (t~s1 a1) where
    a `polyBind2` f = IOS $ do
        t <- (fromIOS a)
        return $ fromPure $ fromPureS $ f t


----------------------------------------------

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout setout m1 s3 b) <= (m1~m2, envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)


instance PolyMonad3 (PureS s1 a) (s1 a) (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 env2 set2 m2 s2 b) where
    a `polyBind3` f = f (fromPure $ fromPureS a)


instance PolyMonad3 (IOS s1 a) (s1 a) (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout set2 m2 s2 b) <= (envout ~ EnvMerge2 env2 IOS, MonadIOS m2, Monad4 m2 Safe s2) where
    a `polyBind3` f = MonadCtx2 $ liftIOS a `bind4` (fromMonadCtx2 . f)

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (IOS s2 b) (MonadCtx2 env1 set1 m1 s1 (Value2 IOS s2 b)) <= (Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . Value2 . f)

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (PureS s2 b) (MonadCtx2 env1 set1 m1 s1 (Value2 PureS s2 b)) <= (Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . Value2 . f)

---

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a2 (Value2 m2 s2 b) out <= PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a2 (m2 s2 b) out where
    a `polyBind3` f = a `polyBind3` (fromValue2 . f)


instance PolyMonad3 (Value2 m1 s1 a1) a2 (MonadCtx2 env set m2 s2 b) mout <= (PolyMonad3 (m1 s1 a1) a2 (MonadCtx2 env set m2 s2 b) mout) where
    a `polyBind3` f = fromValue2 a `polyBind3` f


instance PolyMonad3 (Value2 m1 s1 a1) a2 (Value2 m2 s2 b) (Value2 m s v) <= (PolyMonad3 (m1 s1 a1) a2 (m2 s2 b) (m s v)) where
    a `polyBind3` f = Value2 (fromValue2 a `polyBind3` (fromValue2 . f))

---

--class PolyMonad3 a b c d | a -> b, a c -> d where
--    polyBind3 :: a -> (b -> c) -> d


instance PolyMonad3 (PureS s1 a1) (s1 a1) (PureS s2 b) (PureS s2 b) where
    a `polyBind3` f = f (fromPure $ fromPureS a)

instance PolyMonad3 (PureS s1 a1) (s1 a1) (IOS s2 b) (IOS s2 b) where
    a `polyBind3` f = f (fromPure $ fromPureS a)


instance PolyMonad3 (IOS s1 a1) (s1 a1) (IOS s2 b) (IOS s2 b) where
    a `polyBind3` f = IOS $ do
        t <- (fromIOS a)
        fromIOS $ f t

instance PolyMonad3 (IOS s1 a1) (s1 a1) (PureS s2 b) (IOS s2 b) where
    a `polyBind3` f = IOS $ do
        t <- (fromIOS a)
        return $ fromPure $ fromPureS $ f t