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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.MonadCtx where

import Control.Monad.Trans (MonadTrans, lift)
import Control.PolyMonad
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Control.Monad.IO.Class
import Data.Typeable (Typeable)
import Flowbox.Utils
import Data.TypeLevel
import Control.Applicative

import Luna.Target.HS.Control.Error.Data as DELME

--------------------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------------------

newtype Req req m a = Req (m a) deriving (Show, Typeable)

fromReq (Req a) = a

newtype MonadCtx (base :: * -> *) set m val = MonadCtx (m val) deriving (Show, Typeable)
--newtype MonadCtx2 (base :: * -> *) set (m :: (* -> *) -> * -> *) s val = MonadCtx2 (m s val) deriving (Show, Typeable)
newtype MonadCtx2 (base :: (* -> *) -> * -> *) set (m :: (* -> *) -> * -> *) s val = MonadCtx2 (m s val) deriving (Show, Typeable, Functor)


class SwapCtx ctx1 ctx2 where
    swapCtx :: Value2 ctx1 s a -> Value2 ctx2 s a


instance SwapCtx a a where
    swapCtx = id

instance SwapCtx PureS IOS where
    swapCtx = Value2 . IOS . return . fromPure . fromPureS . fromValue2 


--newtype MonadCtx2 (base :: * -> *) safety set m val = MonadCtx2 (m val) deriving (Show, Typeable)

--operacje safety okazuja sie PO wykonaniu monadu, np IO(Safe ...)


newtype MonadWrapper m = MonadWrapper m

fromMonadCtx :: MonadCtx base set m val -> m val
fromMonadCtx (MonadCtx a) = a

fromMonadCtx2 (MonadCtx2 a) = a

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class MatchMonadCloseProto flag m t | flag m -> t where
    matchMonadCloseProto :: flag -> m a -> t a

class MatchMonadClose m t | m -> t where
    matchMonadClose :: m a -> t a

class AppMonadCtx a b | a -> b where
    appMonadCtx :: a -> b


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

closeMonadCtx :: MonadCtx base set base a -> Value base a
closeMonadCtx (MonadCtx a) = Value a

runMonadProto :: mptr -> (ma a -> mb b) -> (MonadCtx env set ma a) -> (MonadCtx env (Remove mptr set) mb b)
runMonadProto _ f ms = MonadCtx $ f (fromMonadCtx ms)

runMonadProtoReq :: mptr -> (ma a -> mb b) -> Req mptr (MonadCtx env set ma) a -> MonadCtx env (Remove mptr set) mb b
runMonadProtoReq mptr f ms = runMonadProto mptr f (fromReq ms)

runMonad :: mptr -> (ma a -> mb b) -> Req mptr (MonadCtx env set ma) a -> t b <= MatchMonadClose (MonadCtx env (Remove mptr set) mb) t
runMonad = matchMonadClose `dot3` runMonadProtoReq


--Dokonczyc - liftMonadRunner dla MonadCtx2 !
--runMonadProto :: mptr -> (ma a -> mb b) -> (MonadCtx2 env set ma s a) -> (MonadCtx env (Remove mptr set) mb b)
--runMonadProto _ f ms = MonadCtx $ f (fromMonadCtx ms)

closeMonadCtx2 :: MonadCtx2 base set base s a -> base s a
closeMonadCtx2 (MonadCtx2 a) = a

runMonadProto2 :: mptr -> (ma sa a -> mb sb b) -> (MonadCtx2 env set ma sa a) -> (MonadCtx2 env (Remove mptr set) mb sb b)
runMonadProto2 _ f ms = MonadCtx2 $ f (fromMonadCtx2 ms)

runMonadProtoReq2 :: mptr -> (ma sa a -> mb sb b) -> Req mptr (MonadCtx2 env set ma sa) a -> MonadCtx2 env (Remove mptr set) mb sb b
runMonadProtoReq2 mptr f ms = runMonadProto2 mptr f (fromReq ms)


runMonad2 :: mptr -> (ma sa a -> mb sb b) -> Req mptr (MonadCtx2 env set ma sa) a -> t b <= MatchMonadClose (MonadCtx2 env (Remove mptr set) mb sb) t
runMonad2 = matchMonadClose `dot3` runMonadProtoReq2


--runMonad'' :: mptr -> (ma a -> mb b) -> MonadCtx env (ConstrainSet (Insert mptr Empty) set) ma a -> t b <= MatchMonadClose (MonadCtx env (ConstrainSet (Insert mptr Empty) (Remove mptr set)) mb) t 
    --runMonad'' = (removeReqConstrains . matchMonadClose) `dot3` runMonadProto'

--liftMonadRunner1' :: MatchMonadCloseProto (IsEmpty (Remove mptr set)) (MonadCtx env (Remove mptr set) mb) t => mptr -> (ma a1 -> a -> mb b) -> MonadCtx env (Insert (Include mptr) Empty) ma a1 -> a -> t b
--liftMonadRunner1' (mptr :: mprt) (f :: ma a1 -> a -> mb b) (m :: MonadCtx env (Insert (Include mptr) Empty) ma a1) = flip (runMonad mptr) m . (appLastArg1 f)

liftMonadRunner1 mptr f m = flip (runMonad mptr) m . (appLastArg1 f)
liftMonadRunner2 mptr f m = flip (runMonad mptr) m . (appLastArg2 f)
liftMonadRunner3 mptr f m = flip (runMonad mptr) m . (appLastArg3 f)


--removeReqConstrains :: MonadCtx env (ConstrainSet req set) m a -> MonadCtx env (ConstrainSet newreq set) m a
--removeReqConstrains = MonadCtx . fromMonadCtx

--liftMonadRunner1 (Proxy :: Proxy StateT)  runStateT


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Monad (MonadCtx base set m) <= Monad m where
    return = MonadCtx . return
    (MonadCtx ma) >>= f = MonadCtx $ do
        a <- ma
        fromMonadCtx $ f a

instance Functor (MonadCtx base set m) <= Functor m where
    fmap f (MonadCtx mval) = MonadCtx $ fmap f mval

instance Applicative (MonadCtx base set m) <= (Monad m, Functor m) where
    pure = MonadCtx . return
    MonadCtx mf <*> MonadCtx ma = MonadCtx $ do
        f <- mf
        a <- ma
        return $ f a
    
        
instance MatchMonadCloseProto False m m where
    matchMonadCloseProto _ = id

instance MatchMonadCloseProto True (MonadCtx env set m) (Value env) <= (m~env) where
    matchMonadCloseProto _ = closeMonadCtx

instance MatchMonadClose (MonadCtx env set ma) out <= (MatchMonadCloseProto emptySet (MonadCtx env set ma) out, emptySet ~ IsEmpty set) where
    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)


instance MatchMonadClose (MonadCtx2 env set ma sa) out <= (MatchMonadCloseProto emptySet (MonadCtx2 env set ma sa) out, emptySet ~ IsEmpty set) where
    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)

instance MatchMonadCloseProto True (MonadCtx2 env set m s) (env s) <= env~m where
    matchMonadCloseProto _ = closeMonadCtx2

---

--instance MatchMonadClose (MonadCtx env set ma) out <= out ~ MonadCtx env set ma where
--    matchMonadClose = id

--instance MatchMonadClose (MonadCtx env () m) env <= (m~env) where
--    matchMonadClose = closeMonadCtx

---

--instance MatchMonadCloseProto False m m where
--    matchMonadCloseProto _ = id

--instance MatchMonadCloseProto True (MonadCtx env set m) (Value env) <= (m~env) where
--    matchMonadCloseProto _ = closeMonadCtx

--instance MatchMonadClose (MonadCtx env (ConstrainSet req set) ma) out <= (MatchMonadCloseProto emptySet (MonadCtx env (ConstrainSet req set) ma) out, emptySet ~ IsEmpty set) where
--    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)

---

instance AppMonadCtx (MonadCtx2 env set m s a) (Req req (MonadCtx2 env set m s) a) where
    appMonadCtx = Req

instance AppMonadCtx (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
    appMonadCtx = Req

instance AppMonadCtx (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
    appMonadCtx = id

instance AppMonadCtx (Value m2 a2) (Req req (MonadCtx env set m1) a1) <= (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) where
    appMonadCtx = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue


--instance AppMonadCtx (ValueS m2 s2 a2) (Req req (MonadCtx2 env set m1 s1) a1) <= (s2~s1, a1~ValueS Pure Safe a2, Functor s1, Functor m2, LiftValueS m2 m1) where
--    appMonadCtx = Req . MonadCtx2 . liftValueS . (fmap (ValueS . Pure . Safe))

--wyzej dodana jest informacja o env - tu nie, a i tak dziala. Czy jest to potrzbne ? 
--- do czego potrzebne jest req?
-- nie powinnismy dodac tu informacji o env?
instance AppMonadCtx (ValueS m2 s2 a2) (Req req (MonadCtx2 env set m1 s1) a1) <= (set~Insert req Empty, s1~s2, m1~t (ValueS m2), a1 ~ ValueS Pure Safe a2, LiftValueS' (ValueS m2) s2 t, Functor s2, Functor m2) where
    appMonadCtx = Req . MonadCtx2 . liftValueS' . (fmap (ValueS . Pure . Safe))



--class LiftValueS' m s t where
--    liftValueS' :: m (s :: * -> *) a -> t m s a

--liftValueS :: m s a -> t m s a
--liftValueS :: ValueS m s a -> t m s a <- tak powinna wygladac ta sygnatura!!!
--bo inaczej AppMonadCtx nie wnioskuje ze m jest pod monadem t!


---

instance IOEnv (Value m) <= IOEnv m where
    toIOEnv = toIOEnv . fromValue

instance IOEnv (MonadCtx base set m) <= (base~m, IOEnv m) where
    toIOEnv = toIOEnv . closeMonadCtx