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

class Monad4 m s1 s2 where
    bind4 :: m s1 a -> (a -> m s2 b) -> m (MatchSafety s1 s2) b


class PolyMonad1 m1 m2 m3 | m1 m2 -> m3 where
    polyBind1 :: m1 a -> (a -> m2 b) -> m3 b

------

--type family Blah m1 m2 a where
--    Blah (ValueS Pure s1) (ValueS Pure s2) a = a
--    Blah x y a = (ValueS Pure Safe a)

--class PolyMonad1' m1 m2 m3 | m1 m2 -> m3 where
--    polyBind1' :: m1 a -> (a -> m2 b) -> m3 (Blah m1 m2 b)


--y_tstme x = x `polyBind1'` (\_ -> valS' 0)
--y_tstme2 = (valS' 1) `polyBind1'` (\_ -> valS' (0::Int))
--y_tstme3 = (valS' (1::Int)) `polyBind1'` (\_ -> valS' 0)
--y_tstme4 = (valS' 1) `polyBind1'` (\_ -> valS' 0)

------


class PolyMonad2 a b c | a b -> c where
    polyBind2 :: a -> b -> c


class PolyMonad3 a b c d | a -> b, a c -> d where
    polyBind3 :: a -> (b -> c) -> d


--class PolyMonad4 a b mc c d | a -> b, a mc -> d where
--    polyBind4 :: a -> (b -> mc c) -> d c


----

--class PolyMonad5 m1 m2 m3 |  m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c


class PolyMonad5' m1 m2 where
    polyBind5' :: m1 a -> (X1 m1 a -> m2 c) -> (XOut m1 m2) c


class PolyMonad5'' m1 m2 m3 where
    polyBind5'' :: m1 a -> (X1 m1 a -> m2 c) -> m3 c


type family XOut m1 m2 where
    XOut (ValueS base s1)                    (ValueS base' s2)                   = ValueS (EnvMerge base base') s2
    XOut (ValueS vbase vs)                   (MonadCtx2 env set m s)             = MonadCtx2 (XEnv env (ValueS vbase vs)) (XSet set (ValueS vbase vs)) m (XSafety s (ValueS vbase vs))
    XOut (ValueS vbase vs)                   (MonadCtx2Dummy' m2 s2 env set m s) = MonadCtx2Dummy' m2 s2 (XEnv env (ValueS vbase vs)) (XSet set (ValueS vbase vs)) m (XSafety s (ValueS vbase vs))
    XOut (MonadCtx2 env set m s)             a                                   = (XMonad a) (XEnv env a) (XSet set a) m (XSafety s a)
    XOut (MonadCtx2Dummy' m2 s2 env set m s) a                                   = XOut (MonadCtx2 env set m s) a
    --XOut a                                   
    --XOut (MonadCtx2Dummy' m2 s2 env set m s) a                                         = XOut (MonadCtx2 env set m s) a
    --XOut a                                   (MonadCtx2Dummy' m2 s2 env set m1 s1)     = XOut a (MonadCtx2 env set m1 s2)
    --XOut (MonadCtx2Dummy' m2 s2 env set m s) (MonadCtx2Dummy' m2' s2' env' set' m' s') = MonadCtx2Dummy' m2' s2' (XEnv env a) (XSet set a) m (XSafety s a)
    --XOut (MonadCtx2Dummy' m2 s2 env set m s) a = (XMonad a) (XEnv env a) (XSet set a) m (XSafety s a)


--instance PolyMonad5' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
--    (a :: MonadCtx2Dummy' m1' s1' env1 set1 m1 s1 a) `polyBind5'` (f :: (ValueS m1' s1' a) -> MonadCtx2 env2 set2 m2 s2 c) = 
--        (fromMonadCtx2Dummy' a :: MonadCtx2 env1 set1 m1 s1 (ValueS m1' s1' a)) `polyBind5'` f



--instance PolyMonad5' (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
--    a `polyBind5'` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)





    --instance PolyMonad5'' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) m3 <= (m3 ~ XOut (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2)) where
    --    a `polyBind5''` f = (fromMonadCtx2Dummy' a) `polyBind5''` f

-- - f :: X1 (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) a -> MonadCtx2 env2 set2 m2 s2 c
-- - 
-- - a :: MonadCtx2Dummy' m1' s1' env1 set1 m1 s1 a
-- - f :: (ValueS m1' s1' a) -> MonadCtx2 env2 set2 m2 s2 c
-- - -> MonadCtx2 (EnvMerge3 env env2) (Union set2) m (MatchSafety s s2)

--MonadCtx2 (EnvMerge3 env1 env2)

type family XMonad m where
    XMonad (ValueS base s)                     = MonadCtx2Dummy' Pure s
    XMonad (MonadCtx2 env set m s)             = MonadCtx2
    XMonad (MonadCtx2Dummy' m2 s2 env set m s) = MonadCtx2Dummy' m2 s2

type family XEnv a m where
    XEnv env (ValueS base s)                      = env
    XEnv env (MonadCtx2 env' set m s)             = EnvMerge3 env env'
    XEnv env (MonadCtx2Dummy' m2 s2 env' set m s) = EnvMerge3 env env'



type family XEnv' a m where
    XEnv' env (ValueS base s)                      = env
    XEnv' env (MonadCtx2 env' set m s)             = EnvMerge3 env' env
    XEnv' env (MonadCtx2Dummy' m2 s2 env' set m s) = EnvMerge3 env' env

--type family XEnvDummy a m where
--    XEnv env (ValueS base s)           = env
--    XEnv env (MonadCtx2 env' set m s)  = EnvMerge3 env env'

type family XSet a m where
    XSet set (ValueS base s)                      = set
    XSet set (MonadCtx2 env set' m s)             = Union set set'
    XSet set (MonadCtx2Dummy' m2 s2 env set' m s) = Union set set'

type family XSafety a m where
    XSafety s (ValueS base s')                     = s
    XSafety s (MonadCtx2 env set m s')             = MatchSafety s s'
    XSafety s (MonadCtx2Dummy' m2 s2 env set m s') = MatchSafety s s'

class PolyApplicative5 m1 m2 m3 | m1 m2 -> m3 where
    appBind5 :: m1 a -> m2 (X1 m1 a -> c) -> m3 c



class PolyMonad6 m1 m2 where
    polyBind6 :: InferOut m1 m2 c out => m1 a -> (X1 m1 a -> m2 c) -> out



class PolyMonad7 m1 m2 c out | m1 m2 c -> out, out -> c where
    polyBind7 :: m1 a -> (X1 m1 a -> m2 c) -> out



class PolyMonad8 m1 m2 m3 | m1 m2 -> m3 where
    polyBind8 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c


class InferOut (m1 :: * -> *) (m2 :: * -> *) a out | m1 m2 a -> out, out -> a

instance InferOut (ValueS m s) (ValueS m s) a (ValueS m s a)
instance InferOut (MonadCtx2 env set m1 s1) (ValueS m2 s2) a (MonadCtx2 env set m1 s1 (ValueS m2 s2 a))

instance PolyMonad6 (ValueS Pure Safe) (ValueS Pure Safe) where


y_tstme x = x `polyBind5'` (\_ -> valS' 0)
y_tstme2 = (valS' 1) `polyBind5'` (\_ -> valS' (0::Int))
y_tstme3 = (valS' (1::Int)) `polyBind5'` (\_ -> valS' 0)
y_tstme4 = (valS' 1) `polyBind5'` (\_ -> valS' 0)
y_tstme5 x y = x `polyBind5'` (\_ -> y `polyBind5'` (\_ -> valS' 0))

--instance InferOut  where
--    func = 

--polyBind5' a f = unpackMonadCtxDummy $ polyBind5 a f

type family X1 m a where
    X1 (MonadCtx2 env1 set1 m1 s1)          a = a
    X1 (ValueS m s)                         a = (ValueS Pure s a)
    X1 (MonadCtx2Dummy env set m2 s2 m1 s1) a = (ValueS m2 s2 a)
    X1 (MonadCtx2Dummy' m' s' env set m s)  a = (ValueS m' s' a)


type family X2 m a where
    X2 (MonadCtx2 env1 set1 m1 s1)          (ValueS ma sa a) = (ValueS ma sa a)
    X2 (ValueS m s)                         a = (ValueS Pure s a)
    X2 (MonadCtx2Dummy env set m2 s2 m1 s1) a = (ValueS m2 s2 a)


--type family X2 m1 m2 a where
--    X2 (ValueS m s) (ValueS m s) a = a

--class PolyMonad6 a b m1 m2 out | a -> b, b -> a, m1 m2 b -> out, out -> m1 m2 b where
--    polyBind6 :: m1 a -> (b -> m2 b) -> out

--type family XME a b

--polyBind2Env :: PolyMonad2 a (s v -> c1) c => a -> (Value2 PureS s v -> c1) -> c
--polyBind2Env a f = a `polyBind2` (f . Value2 . PureS . Pure)

----polyBind2Env :: PolyMonad2 a (s v -> c1) c => a -> (Value2 PureS s v -> c1) -> c
--polyBind2Env_ (a :: Value2 PureS s a) b = a `polyBind2` (\(_ :: s a) -> b)


--polyBind3Env a f = a `polyBind3` (f . Value2 . PureS . Pure)
--polyBind3Env_ a b = a `polyBind3` (\_ -> b)

polyBind1_ a b = a `polyBind1` (\_ -> b)
polyBind3_ a b = a `polyBind3` (\_ -> b)
polyBind5_ a b = a `polyBind5` (\_ -> b)
polyBind5'_ a b = a `polyBind5'` (\_ -> b)
polyBind7_ a b = a `polyBind7` (\_ -> b)
polyBind8_ a b = a `polyBind8` (\_ -> b)


--joinEnv

class Monad3R m s where
    return3 :: (forall x. x -> s x) -> a -> m s a

wrap3 = return3 Safe

--xxx :: PolyMonad2 a b c => a -> b -> c
--xxx t = polyBind2Env t

val2' = Value2 . PureS . Pure . Safe

--xxx x = x `polyBind2` (\_ -> val2' (0::Int))

xxx x = x `polyBind3` (\x' -> (Value2 . PureS . Pure) x')


--class JoinEnv m1 m2 m3 | m1 m2 -> m3 where
--    m1 (m2 a) -> m3 

valS' = ValueS . Pure . Safe

valS'io :: a -> ValueS IO Safe a
valS'io = ValueS . return . Safe

--tstme x = x `polyBind3` (\_ -> valS' 0)

--JEDNO Z TYCH NIE DZIALA !!!
--tstme x = x `polyBind4` (\_ -> valS' 0)
--tstme2 = (valS' 1) `polyBind4` (\_ -> valS' 0)

--tstme :: m1 a -> m3 c <= (PolyMonad5 m1 (ValueS Pure Safe) m3, Num c)
tstme x = x `polyBind5` (\_ -> valS' 0)

--tstme' :: m1 a -> out <= (PolyMonad5 m1 (ValueS Pure Safe) m3, Num c, UnpackMonadCtxDummy (m3 c) out)
        --tstme' x = unpackMonadCtxDummy (tstme x)

--tstme2 = (valS' 1) `polyBind5'` (\_ -> valS' (0::Int))
--tstme3 = (valS' (1::Int)) `polyBind5'` (\_ -> valS' 0)
--tstme4 = (valS' 1) `polyBind5'` (\_ -> valS' 0)


--class PolyMonad6 m1 m2 where
--    polyBind6 :: InferOut m1 m2 c out => m1 a -> (X1 m1 a -> m2 c) -> out
--                                         x    ->  _       -> .  ? 

x_tstme x = x `polyBind5` (\_ -> valS' 0)
x_tstme2 = (valS' 1) `polyBind5` (\_ -> valS' (0::Int))
x_tstme3 = (valS' (1::Int)) `polyBind5` (\_ -> valS' 0)
x_tstme4 = (valS' 1) `polyBind5` (\_ -> valS' 0)

--x_tstme5 x y = x `polyBind8` (\_ -> y `polyBind8` (\_ -> valS' 0))
    --x_tstme5' x y = flip polyBind8_ (flip polyBind8_ (valS' 0) y) x


    --flip polyBind8_             :: PolyMonad8 m1 m2 c1 c => m2 c1 -> m1 a -> c

    --(flip polyBind8_ (valS' 0)) :: (PolyMonad8 m1 (ValueS Pure Safe) c1 c, Num c1) => m1 a -> c
--class PolyMonad3 a b c d | a -> b, a c -> d where
--    polyBind3 :: a -> (b -> c) -> d
--                 x -> (_ -> 0) -> d

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


--instance PolyMonad (PureS s1) (PureS s2) (PureS s3) <= PolyMonad s1 s2 s3 where
--    a >>=~ f = PureS . Pure $ (fromPure . fromPureS) a >>=~ (fromPure . fromPureS . f)

--instance PolyMonad IO Pure IO where
--    ma >>=~ f = ma >>= return . fromPure . f

--instance PolyMonad Pure IO IO where
--    a >>=~ f = f $ fromPure a

--instance PolyMonad IO IO IO where
--    a >>=~ f = a >>= f







--instance PolyMonad (MonadCtx2 env set m1 s1) (Value2 m2 s2) mout <= PolyMonad (MonadCtx2 env set m1 s1) (m2 s2) mout where
--    a >>=~ f = a >>=~ (fromValue2 . f)

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

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2, m1~m2, Monad4 m2 s1 s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env set m s1) (ValueS Pure s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, Monad4 m s1 s2, MonadTrans m, Monad s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (lift . fromPure . fromValueS . f)

-- DODAC ENVOUT
instance PolyMonad (MonadCtx2 env set m s1) (ValueS IO s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, Monad4 m s1 s2, MonadTrans m, Monad s2, LiftValueS IO m, Functor s2) where
    a >>=~ f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS . f)

instance PolyMonad (ValueS Pure s1) (MonadCtx2 env set m s2) (MonadCtx2 env set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, Monad4 m s1 s2, Monad s1) where
    a >>=~ f = MonadCtx2 $ (lift . fromPure $ fromValueS a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad (ValueS IO s1) (MonadCtx2 env set m s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, Monad4 m s1 s2, Monad s1, LiftValueS IO m, Functor s1) where
    a >>=~ f = MonadCtx2 $ (liftValueS a) `bind4` (fromMonadCtx2 . f)

--instance PolyMonad IO (MonadCtx env set m) (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
--    a >>=~ f = MonadCtx $ liftIO a >>= (fromMonadCtx . f)


instance PolyMonad (ValueS Pure s1) (ValueS Pure s2) (ValueS Pure s3) <= (s3 ~ MatchSafety s1 s2, Monad4 (ValueS Pure) s1 s2) where
    a >>=~ f = a `bind4` f

instance PolyMonad (ValueS IO s1) (ValueS Pure s2) (ValueS IO s3) <= (s3 ~ MatchSafety s1 s2, Monad4 (ValueS IO) s1 s2) where
    a >>=~ f = a `bind4` (ValueS . return . fromPure . fromValueS . f)

instance PolyMonad (ValueS Pure s1) (ValueS IO s2) (ValueS IO s3) <= (s3 ~ MatchSafety s1 s2, Monad4 (ValueS IO) s1 s2) where
    a >>=~ f = (ValueS . return . fromPure . fromValueS $ a) `bind4` f

instance PolyMonad (ValueS IO s1) (ValueS IO s2) (ValueS IO s3) <= (s3 ~ MatchSafety s1 s2, Monad4 (ValueS IO) s1 s2) where
    a >>=~ f = a `bind4` f

--instance PolyMonad Pure IO IO where
--    a >>=~ f = f $ fromPure a

--instance PolyMonad IO IO IO where
--    a >>=~ f = a >>= f




class LiftValueS2 m t where
    liftValueS2 :: ValueS m Safe a -> t Safe a

----------------------------------------------

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout setout m1 s3 b) <= (m1~m2, envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)


instance PolyMonad3 (Pure (s1 a)) (ValueS Pure s1 a) (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 env2 set2 m2 s2 b) where
    a `polyBind3` f = f $ ValueS a


--instance PolyMonad3 (IO (s1 a)) (ValueS Pure s1 a) (MonadCtx2 env2 set2 m2 s2 b) (MonadCtx2 envout set2 m2 s2 b) <= (envout ~ EnvMerge2 env2 IOS, MonadIOS m2, Monad4 m2 Safe s2) where
--    a `polyBind3` f = MonadCtx2 $ liftIO a `bind4` (fromMonadCtx2 . f . ValueS . Pure)

    --instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (IO (s2 b)) (MonadCtx2 env1 set1 m1 s1 (ValueS IO s2 b)) <= (Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    --    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . ValueS . f)


-- DODAC envout
instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (IO (s2 b)) (MonadCtx2 envout set1 m1 s1 (ValueS Pure s2 b)) <= (Monad4 m1 s1 Safe, LiftValueS IO m1) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS . ValueS . fmap (Safe . ValueS . Pure) . f)

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (Pure (s2 b)) (MonadCtx2 env1 set1 m1 s1 (ValueS Pure s2 b)) <= (Monad4 m1 s1 Safe, Monad3R m1 Safe) where
    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . ValueS . f)

-----

instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a2 (ValueS m2 s2 b) out <= PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a2 (m2 (s2 b)) out where
    a `polyBind3` f = a `polyBind3` (fromValueS . f)


instance PolyMonad3 (ValueS m1 s1 a1) a2 (MonadCtx2 env set m2 s2 b) mout <= (PolyMonad3 (m1 (s1 a1)) a2 (MonadCtx2 env set m2 s2 b) mout) where
    a `polyBind3` f = fromValueS a `polyBind3` f


instance PolyMonad3 (ValueS m1 s1 a1) a2 (ValueS m2 s2 b) (ValueS m s v) <= (PolyMonad3 (m1 (s1 a1)) a2 (m2 (s2 b)) (m (s v))) where
    a `polyBind3` f = ValueS (fromValueS a `polyBind3` (fromValueS . f))

---

--class PolyMonad3 a b c d | a -> b, a c -> d where
--    polyBind3 :: a -> (b -> c) -> d


instance PolyMonad3 (Pure (s1 a1)) (ValueS Pure s1 a1) (Pure (s2 b)) (Pure (s2 b)) where
    a `polyBind3` f = f $ ValueS a

instance PolyMonad3 (Pure (s1 a1)) (ValueS Pure s1 a1) (IO (s2 b)) (IO (s2 b)) where
    a `polyBind3` f = f $ ValueS a


instance PolyMonad3 (IO (s1 a1)) (ValueS Pure s1 a1) (IO (s2 b)) (IO (s2 b)) where
    a `polyBind3` f = do
        t <- a
        f (ValueS . Pure $ t)

instance PolyMonad3 (IO (s1 a1)) (ValueS Pure s1 a1) (Pure (s2 b)) (IO (s2 b)) where
    a `polyBind3` f = do
        t <- a
        return . fromPure $ f (ValueS . Pure $ t)


--instance PolyMonad5 a a (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (m1~m2, envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
--    a `polyBind5` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)


--instance PolyMonad5 (s1 a) (ValueS Pure s1 a) Pure (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 env2 set2 m2 s2) where
--    a `polyBind5` f = f $ ValueS a

--class PolyMonad5 a b m1 m2 m3 | a -> b, b -> a, m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (b -> m2 b) -> m3 b

--class PolyMonad6 a b m1 m2 out | a -> b, b -> a, m1 m2 b -> out, out -> m1 m2 b where
--    polyBind6 :: m1 a -> (b -> m2 b) -> out


--newtype Just a = Just a

instance PolyMonad5 (ValueS Pure s1) (ValueS Pure s2) (ValueS Pure s2) where
    a `polyBind5` f = f a

instance PolyMonad5 (ValueS Pure s1) (ValueS IO s2) (ValueS IO s2) where
    a `polyBind5` f = f a

instance PolyMonad5 (ValueS IO s1) (ValueS Pure s2) (ValueS IO s2) where
    a `polyBind5` f = ValueS $ do
        a' <- fromValueS a
        return . fromPure . fromValueS $ f (ValueS . Pure $ a')

instance PolyMonad5 (ValueS IO s1) (ValueS IO s2) (ValueS IO s2) where
    a `polyBind5` f = ValueS $ do
        a' <- fromValueS a
        fromValueS $ f (ValueS . Pure $ a')



--class PolyApplicative5 m1 m2 m3 | m1 m2 -> m3 where
--    appBind5 :: m1 a -> m2 (X1 m1 a -> c) -> m3 c


--instance PolyApplicative5 (ValueS Pure s1) (ValueS Pure s2) (ValueS Pure s2) where
--    f `appBind5` a = (fromPure . fromValueS $ f) a

--instance PolyApplicative5 (ValueS Pure s1) (ValueS IO s2) (ValueS IO s2) where
--    a `appBind5` f = f a

--instance PolyApplicative5 (ValueS IO s1) (ValueS Pure s2) (ValueS IO s2) where
--    a `appBind5` f = ValueS $ do
--        a' <- fromValueS a
--        return . fromPure . fromValueS $ f (ValueS . Pure $ a')

--instance PolyApplicative5 (ValueS IO s1) (ValueS IO s2) (ValueS IO s2) where
--    a `appBind5` f = ValueS $ do
--        a' <- fromValueS a
--        fromValueS $ f (ValueS . Pure $ a')

---

instance PolyMonad5 (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (m1~m2, envout ~ EnvMerge3 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
    a `polyBind5` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad5 (ValueS Pure s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 env2 set2 m2 s2) where
    a `polyBind5` f = f a

instance PolyMonad5 (ValueS IO s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout set2 m2 s2) <= (envout~(EnvMerge3 env2 (ValueS IO)), LiftValueS2 IO m2, Monad4 m2 Safe s2) where
    a `polyBind5` f = MonadCtx2 $ (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS $ a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad5 (MonadCtx2 env1 set1 m1 s1) (ValueS IO s2) (MonadCtx2Dummy envout set1 Pure s2 m1 s1) <= (envout~(EnvMerge3 env1 (ValueS IO)), Monad4 m1 s1 Safe, LiftValueS2 IO m1) where
    a `polyBind5` f = MonadCtx2Dummy . MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)

instance PolyMonad5 (MonadCtx2 env1 set1 m1 s1) (ValueS Pure s2) (MonadCtx2Dummy envout set1 Pure s2 m1 s1) <= (envout~(EnvMerge3 env1 (ValueS Pure)), Monad4 m1 s1 Safe, LiftValueS2 Pure m1) where
    a `polyBind5` f = MonadCtx2Dummy . MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)

---

instance PolyMonad5 (MonadCtx2Dummy env1 set1 m1' s1' m1 s1) (MonadCtx2Dummy env2 set2 m2' s2' m2 s2) (MonadCtx2Dummy envout setout m2' s2' m1 s3) <= (m1~m2, Monad4 m2 s1 s2, envout ~ EnvMerge3 env1 env2, s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2) where
    a `polyBind5` f = MonadCtx2Dummy $ fromMonadCtx2Dummy a `polyBind5` (fromMonadCtx2Dummy . f)

instance PolyMonad5 (ValueS Pure s1) (MonadCtx2Dummy env2 set2 m2' s2' m2 s2) (MonadCtx2Dummy env2 set2 m2' s2' m2 s2) where
    a `polyBind5` f = MonadCtx2Dummy $ a `polyBind5` (fromMonadCtx2Dummy . f)

instance PolyMonad5 (ValueS IO s1) (MonadCtx2Dummy env2 set2 m2' s2' m2 s2) (MonadCtx2Dummy envout set2 m2' s2' m2 s2) <= (envout~(EnvMerge3 env2 (ValueS IO)), Monad4 m2 Safe s2, LiftValueS2 IO m2) where
    a `polyBind5` f = MonadCtx2Dummy $ a `polyBind5` (fromMonadCtx2Dummy . f)

instance PolyMonad5 (MonadCtx2Dummy env1 set1 m1' s1' m1 s1) (ValueS IO s2) (MonadCtx2Dummy envout set1 Pure s2 m1 s1) <= (Monad4 m1 s1 Safe, LiftValueS2 IO m1, envout~(EnvMerge3 env1 (ValueS IO))) where
    a `polyBind5` f = fromMonadCtx2Dummy a `polyBind5` f

instance PolyMonad5 (MonadCtx2Dummy env1 set1 m1' s1' m1 s1) (ValueS Pure s2) (MonadCtx2Dummy envout set1 Pure s2 m1 s1) <= (Monad4 m1 s1 Safe, LiftValueS2 Pure m1, envout~(EnvMerge3 env1 (ValueS Pure))) where
    a `polyBind5` f = fromMonadCtx2Dummy a `polyBind5` f

instance PolyMonad5 (MonadCtx2 env1 set1 m1 s1) (MonadCtx2Dummy env2 set2 m2' s2' m2 s2) (MonadCtx2Dummy envout setout m2' s2' m1 s3) <= (m1~m2, Monad4 m2 s1 s2, envout ~ EnvMerge3 env1 env2, s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2) where
    a `polyBind5` f = MonadCtx2Dummy $ a `polyBind5` (fromMonadCtx2Dummy . f)

instance PolyMonad5 (MonadCtx2Dummy env1 set1 m1' s1' m1 s1) (MonadCtx2 env2 set2 m2 s2) (MonadCtx2 envout setout m1 s3) <= (m1~m2, Monad4 m2 s1 s2, envout ~ EnvMerge3 env1 env2, s3 ~ MatchSafety s1 s2, setout ~ Union set1 set2) where
    a `polyBind5` f = (fromMonadCtx2Dummy a) `polyBind5` f


-----------------------------------------------------------

instance PolyMonad5' (ValueS Pure s1) (ValueS Pure s2) where
    a `polyBind5'` f = f a

instance PolyMonad5' (ValueS Pure s1) (ValueS IO s2) where
    a `polyBind5'` f = f a

instance PolyMonad5' (ValueS IO s1) (ValueS Pure s2) where
    a `polyBind5'` f = ValueS $ do
        a' <- fromValueS a
        return . fromPure . fromValueS $ f (ValueS . Pure $ a')

instance PolyMonad5' (ValueS IO s1) (ValueS IO s2) where
    a `polyBind5'` f = ValueS $ do
        a' <- fromValueS a
        fromValueS $ f (ValueS . Pure $ a')

---

instance PolyMonad5' (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
    a `polyBind5'` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad5' (ValueS Pure s1) (MonadCtx2 env2 set2 m2 s2) where
    a `polyBind5'` f = f a

instance PolyMonad5' (ValueS IO s1) (MonadCtx2 env2 set2 m2 s2) <= (LiftValueS2 IO m2, Monad4 m2 Safe s2) where
    a `polyBind5'` f = MonadCtx2 $ (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS $ a) `bind4` (fromMonadCtx2 . f)

instance PolyMonad5' (MonadCtx2 env1 set1 m1 s1) (ValueS IO s2) <= (Monad4 m1 s1 Safe, LiftValueS2 IO m1) where
    a `polyBind5'` f = MonadCtx2Dummy' . MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)

instance PolyMonad5' (MonadCtx2 env1 set1 m1 s1) (ValueS Pure s2) <= (Monad4 m1 s1 Safe, LiftValueS2 Pure m1) where
    a `polyBind5'` f = MonadCtx2Dummy' . MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)

-----

instance PolyMonad5' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
    a `polyBind5'` f = MonadCtx2Dummy' $ fromMonadCtx2Dummy' a `polyBind5'` (fromMonadCtx2Dummy' . f)

instance PolyMonad5' (ValueS Pure s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) where
    a `polyBind5'` f = MonadCtx2Dummy' $ a `polyBind5'` (fromMonadCtx2Dummy' . f)

instance PolyMonad5' (ValueS IO s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (Monad4 m2 Safe s2, LiftValueS2 IO m2) where
    a `polyBind5'` f = MonadCtx2Dummy' $ a `polyBind5'` (fromMonadCtx2Dummy' . f)

instance PolyMonad5' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (ValueS IO s2) <= (Monad4 m1 s1 Safe, LiftValueS2 IO m1) where
    a `polyBind5'` f = fromMonadCtx2Dummy' a `polyBind5'` f

instance PolyMonad5' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (ValueS Pure s2) <= (Monad4 m1 s1 Safe, LiftValueS2 Pure m1) where
    a `polyBind5'` f = fromMonadCtx2Dummy' a `polyBind5'` f

instance PolyMonad5' (MonadCtx2 env1 set1 m1 s1) (MonadCtx2Dummy' m2' s2' env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
    a `polyBind5'` f = MonadCtx2Dummy' $ a `polyBind5'` (fromMonadCtx2Dummy' . f)

instance PolyMonad5' (MonadCtx2Dummy' m1' s1' env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) <= (m1~m2, Monad4 m2 s1 s2) where
    a `polyBind5'` f = (fromMonadCtx2Dummy' a) `polyBind5'` f

--dokonczyc ^^^^^^^^^^^^^^^^^
-----------------------------------------------------------


--instance PolyMonad3 (MonadCtx2 env1 set1 m1 s1 a) a (Pure (s2 b)) (MonadCtx2 env1 set1 m1 s1 (ValueS Pure s2 b)) <= (Monad4 m1 s1 Safe, Monad3R m1 Safe) where
--    a `polyBind3` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (wrap3 . ValueS . f)


--class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

newtype MonadCtx2Dummy env set m2 s2 m1 s1 a = MonadCtx2Dummy (MonadCtx2 env set m1 s1 (ValueS m2 s2 a))
newtype MonadCtx2Dummy' m2 s2 env set m1 s1 a = MonadCtx2Dummy' (MonadCtx2 env set m1 s1 (ValueS m2 s2 a))

fromMonadCtx2Dummy (MonadCtx2Dummy a) = a
fromMonadCtx2Dummy' (MonadCtx2Dummy' a) = a

instance PolyMonad7 (ValueS Pure s1) (ValueS Pure s2) a (ValueS Pure s2 a) where
    a `polyBind7` f = f a


instance PolyMonad7 (ValueS Pure s1) (ValueS IO s2) a (ValueS IO s2 a) where
    a `polyBind7` f = f a


instance PolyMonad7 (ValueS IO s1) (ValueS Pure s2) a (ValueS IO s2 a) where
    a `polyBind7` f = ValueS $ do
        a' <- fromValueS a
        return . fromPure . fromValueS $ f (ValueS . Pure $ a')


instance PolyMonad7 (ValueS IO s1) (ValueS IO s2) a (ValueS IO s2 a) where
    a `polyBind7` f = ValueS $ do
        a' <- fromValueS a
        fromValueS $ f (ValueS . Pure $ a')


---

instance PolyMonad7 (MonadCtx2 env1 set1 m1 s1) (MonadCtx2 env2 set2 m2 s2) a (MonadCtx2 envout setout m1 s3 out) <= (out~a, m1~m2, envout ~ EnvMerge2 env1 env2, setout ~ Union set1 set2, s3~MatchSafety s1 s2, Monad4 m2 s1 s2) where
    a `polyBind7` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (fromMonadCtx2 . f)


instance PolyMonad7 (ValueS Pure s1) (MonadCtx2 env2 set2 m2 s2) a (MonadCtx2 env2 set2 m2 s2 out) <= (out~a) where
    a `polyBind7` f = f a


instance PolyMonad7 (ValueS IO s1) (MonadCtx2 env2 set2 m2 s2) a (MonadCtx2 envout set2 m2 s2 out) <= (envout~(EnvMerge3 env2 (ValueS IO)), out~a, LiftValueS2 IO m2, Monad4 m2 Safe s2) where
    a `polyBind7` f = MonadCtx2 $ (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS $ a) `bind4` (fromMonadCtx2 . f)


instance PolyMonad7 (MonadCtx2 env1 set1 m1 s1) (ValueS IO s2) a (MonadCtx2 envout set1 m1 s1 (ValueS Pure s2 a)) <= (envout~(EnvMerge3 env1 (ValueS IO)), Monad4 m1 s1 Safe, LiftValueS2 IO m1) where
    a `polyBind7` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)


instance PolyMonad7 (MonadCtx2 env1 set1 m1 s1) (ValueS Pure s2) a (MonadCtx2 envout set1 m1 s1 out) <= (envout~(EnvMerge3 env1 (ValueS Pure)), out~(ValueS Pure s2 a), Monad4 m1 s1 Safe, LiftValueS2 Pure m1) where
    a `polyBind7` f = MonadCtx2 $ (fromMonadCtx2 a) `bind4` (liftValueS2 . ValueS . fmap (Safe . ValueS . Pure) . fromValueS . f)



--instance PolyMonad (ValueS IO s1) (MonadCtx2 env set m s2) (MonadCtx2 envout set m s3) <= (s3 ~ MatchSafety s1 s2, MonadTrans m, Monad4 m s1 s2, Monad s1, LiftValueS IO m) where
--    a >>=~ f = MonadCtx2 $ (liftValueS a) `bind4` (fromMonadCtx2 . f)

--class PolyMonad7 m1 m2 c out | m1 m2 c -> out, out -> c where
--    polyBind7 :: m1 a -> (X1 m1 a -> m2 c) -> out


class UnpackMonadCtxDummy a b | a -> b where
    unpackMonadCtxDummy :: a -> b

instance UnpackMonadCtxDummy (MonadCtx2Dummy env set m2 s2 m1 s1 a) (MonadCtx2 env set m1 s1 (ValueS m2 s2 a)) where
    unpackMonadCtxDummy = fromMonadCtx2Dummy

instance UnpackMonadCtxDummy (MonadCtx2 env set m s a) (MonadCtx2 env set m s a) where
    unpackMonadCtxDummy = id

instance UnpackMonadCtxDummy (ValueS m s a) (ValueS m s a) where
    unpackMonadCtxDummy = id

--class UnpackMonadCtxDummy m a b | m a -> b where
--    unpackMonadCtxDummy :: m a -> b (ConvX m a)


--type family ConvX (m :: * -> *) a where
--    --ConvX (MonadCtx2Dummy env set m s1 s2) a = MonadCtx2 env set m s1 (ValueS Pure s2 a)
--    ConvX (MonadCtx2 env set m s) a = MonadCtx2 env set m s a 
--    --ConvX m a = m a


--class ConvX2 m a out | m a -> out


