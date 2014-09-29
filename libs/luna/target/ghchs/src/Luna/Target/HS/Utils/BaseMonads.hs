---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}


{-# LANGUAGE GADTs #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}


!{-# LANGUAGE RightSideContexts #-}

{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Utils.BaseMonads where

import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow
import Control.Monad.Trans
import Control.Monad.Morph
import Data.Typeable (Typeable)
import Luna.Target.HS.Control.Context
import Data.TypeLevel
import Data.Typeable
import Control.Applicative
import Control.Monad (ap)

import Control.PolyMonad


import Flowbox.Utils

--print' = liftIO . print


instance MonadIO (t m) <= (MonadIO m, MonadTrans t, Monad (t m)) where
    liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- ReaderT monad
--------------------------------------------------------------------------------


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving (Typeable)

class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    ask   :: m r
    ask = reader id

    local :: (r -> r) -> m a -> m a

    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

withReaderT f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (const m)

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = lift (fail msg)

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local = withReaderT
    reader f = ReaderT (return . f)

instance MFunctor (ReaderT r) where
    hoist nat m = ReaderT (\i -> nat (runReaderT m i))

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)


--------------------------------------------------------------------------------
-- StateT monad
--------------------------------------------------------------------------------

-- data Vector a = Vector { x :: a 
--                        , y :: a
--                        , z :: a
--                        , t :: a -> a
--                        } deriving Show
 
-- class Vector a:
--     x,y,z :: a
--     t     :: a -> a
 
 
-- ###
 
-- monad State s a:
--     val :: s -> ctx (a, s)
 
--     def wrap val:
--         State s: ctx.wrap (val, s)
 
--     def bind f:
--         State s:
--             (val2, subs) = unwrap (val s)
--             (f val2).val subs
 
-- def get     : State s: (s, s)
-- def put val : State _: ((), val)
-- def run st ::(a in State s) init::s : 
--     s.val(init).map(@.fst)
 
-- ### <=>
 
-- context State a:
--     val :: a
 
--     def get: val
 
--     def put n:
--         self.val = n
 
 
-- konwersja w obie strony: monad <-> wartosc
-- moze gadtsy?
-- dowolne metody - zachowanie jak w klasach
 
-- >>= :: m a -> (a -> m b) -> m b

 --class Maybe a:
 --   Just a
 --   Nothing

 --   def wrap a: Just (ctx a)

 --   def bind f:
 --       case self:
 --           Just a : f a
 --           Nothing: Nothing




 
---  :: Value Pure (Safe (Maybe (Value Pure (Safe Int))))
---  -> Value Pure (Safe (MonadCtx Pure (Maybe,()) m (Value Pure (Safe Int))))
---
---  :: Value Pure (Safe (Maybe (Value Pure (Safe Int))))
---  -> MonadCtx Pure (Maybe,()) m (Safe (Value Pure (Safe Int)))
 

---  -> MonadCtx Pure (Maybe,()) m (Safe Int)

 
-- Value Pure (Safe (Maybe Int)) -> Value Pure (Safe (MonadCtx Pure (Maybe,()) m Int))
 
-- Value Pure (Safe (Maybe Int)) -> (MonadCtx Pure (Maybe,()) m (Safe Int))


--MonadCtx Pure (Maybe,()) m (Safe Int)

--put (x :: Value IO (Safe Int)) :: MonadCtx IO (Maybe,()) m (Safe Int)

--runStateT ... :: Value Pure (Safe    (Value IO(Safe Int) , Value IO(Safe Int))   ) 


--put (x :: Value IO (Safe Int)) :: MonadCtx IO (Maybe,()) m (Value IO (Safe Int))


--Pure (Safe val)

--MonadCtx Safe 

--runMonadCtx -> Pure Safe val

newtype StateT2 v m s a = StateT2 { runStateT2 :: v -> m (s (a,v)) } deriving (Typeable)
--newtype StateT3 v m s a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)

data E1 = E1 deriving Show

data Safe2 a = Safe2 a deriving Show
data Unsafe2 a = Unsafe2 a deriving Show


--liftIO :: IO a -> m a

--instance MonadIO Safe2 where
--    liftIO a 

class Monad2 m where
    return2 :: Monad safety => a -> m safety a
    bind2   :: m safety1 a -> (a -> m safety2 b) -> m safety2 b

class IsError a where
    isError :: a -> Prelude.Bool

--instance Monad m => Monad2 (StateT2 s m) where
--    return2 a = StateT2 $ \s -> return $ return (a,s)
--    m `bind2` k  = StateT2 $ \s -> do
--        s1 <- runStateT2 m s
--        if isError s1 then return s1
--                      else return s1
        --do
        --    ~(a, s') <- s1
        --    runStateT2 (k a) s'
    --fail str = failErr


------------------------

--newtype MonadCtx (base :: * -> *) set m s val = MonadCtx (m s val) deriving (Show, Typeable)




class Monad3 m s1 s2 s3 | s1 s2 -> s3 where
    bind3 :: m s1 a -> (a -> m s2 b) -> m s3 b






instance Monad3R (Value m) s <= Monad m where
    return3 fs a = Value . return $ fs a




--- ===

instance MonadSafety (Value Pure) s1 s2 <= PolyMonad s1 s2 (MatchSafety s1 s2) where
    bindSafety m f = tst where
        f' = fromPure . fromValue . f -- :: a -> (s2 b)
        m' = fromPure $ fromValue m   -- :: (s1 a)
        tst = Value $ Pure $ m' >>>= f'
        -- PureS $ fromPureS m >>>= (fromPureS . f)

instance MonadSafety (Value IO) Safe s where
    bindSafety m f = Value $ do
        Safe a <- fromValue m
        fromValue $ f a

--instance MonadSafety PureS Safe s where
--    bindSafety m f = PureS $ do
--        Safe a <- fromPureS m
--        fromPureS $ f a


--instance MonadSafety PureS (UnsafeBase base err) Safe where
--    bindSafety m f = fromPureS m



--s1 a -> (a -> s2 b) -> s3 b

--s1 a -> (a -> m (s2 b)) -> m (s3 b)

--instance Monad3 (StateT3 s m) Safe s2 s3 <= Monad3 m Safe s2 s3 where
--    bind3 m f = StateT3 $ \ s ->
--        runStateT3 m s `bind3` (\(a, s')->
--        runStateT3 (f a) s')

newtype StateT3 v m (s :: * -> *) a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)

--put4Raw :: (Monad3R m Safe) => v -> StateT3 v m Safe (Value2 PureS Safe ()) -- modified
--put4Raw s = StateT3 $ \_ -> return3 return (val2 (), s)


--class NewBind m1 m2 m3 s1 s2 s3 | m1 m2 -> m3, s1 s2 -> s3 where
--    newBind :: m1 (base :: (* -> *) -> * -> *) s1 a -> (a -> m2 (base :: (* -> *) -> * -> *) s2 b) -> m3 (base :: (* -> *) -> * -> *) s3 b


--newTst1 = put4Raw (val2 (0::Int)) `newBind` (\_ -> put4Raw (val2 (0::Int)))
--moze cos z tym wyzej zrobic?



instance MonadSafety (StateT3 s m) s1 s2 <= MonadSafety m s1 s2 where
    bindSafety m f = StateT3 $ \ s ->
        runStateT3 m s `bindSafety` (\(a, s')->
        runStateT3 (f a) s')




instance Monad3R (StateT3 s m) safety <= (Monad3R m safety) where
    return3 fs a = StateT3 $ \s -> return3 fs (a, s)





--instance Monad3T (StateT3 s m) <= Monad3T m where
--    return3T sa = StateT3 $ \s -> return3T $ fmap (\a -> (a, s)) sa


class MonadState2 v m | m -> v where
    put3' :: v -> m Safe v


instance MonadState2 v (StateT3 v m) <= Monad3R m Safe where
    put3' s = StateT3 $ \_ -> wrap3 (s,s)


--instance MonadState2 (Value2 PureS Safe Int) (StateT3 Int PureS) where
--    put3' v = StateT3 $ \_ s -> do
--        a <- fromValue2 v
--        return (a, s)



class MonadState4 v (m :: (* -> *) -> * -> *) s | m -> v where
    get4 :: m s v
    put4 :: v -> m s (Value Pure Safe ())

instance MonadState4 v (StateT3 v m) s <= (Monad s, Monad3R m s) where
    get4   = StateT3 $ \s -> return3 return (s, s)
    put4 s = StateT3 $ \_ -> return3 return (val (), s)







--put4sim :: m e s v ->  StateT3 (Value2 PureS Safe ) m s (Value2 PureS Safe ())
--put4sim = undefined -- s = StateT3 $ \_ -> return3 return (val2 (), s)


--instance MonadState4 (Value2 PureS Safe Int) out Safe <= out~(StateT3 Int PureS) where
--    put4 = undefined

--instance MonadTrans (StateT s) where
--    lift m = StateT $ \s -> do
--        a <- m
--        return (a, s)

--instance MonadState s (t s m) <= (MonadTrans (t s), MonadState s m, Monad (t s m)) where
--    put3' s = lift $ put3' s
    --get   = lift $ get
    --put s = lift $ put s


put3 :: Monad3R m Safe => v -> StateT3 v m Safe v
put3 s = StateT3 $ \_ -> wrap3 (s,s)

--raiseMe :: e -> m (UnsafeBase Safe e) () <= Monad3R m (UnsafeBase Safe e)
--raiseMe e = return3 ()

raiseMe :: e -> m (UnsafeBase Safe e) () <= Monad3R m (UnsafeBase Safe e)
raiseMe e = return3 (\_ -> Error e) ()

tst1 = put3 (0::Int) `bindSafety` (\_ -> put3 (0::Int))
tst1' = put3' (0::Int) `bindSafety` (\_ -> put3' (0::Int))

tst2 = put3 (0::Int) `bindSafety` (\_ -> raiseMe E1)
tst2' = put3' (0::Int) `bindSafety` (\_ -> raiseMe E1)

tst3 = raiseMe E1 `bindSafety` (\_ -> put3 (0::Int))
tst3' = raiseMe E1 `bindSafety` (\_ -> put3' (0::Int))




get5X :: MonadState4 val m Safe => MonadCtx (Value Pure) (Insert (Proxy StateT3) Empty) m Safe val
get5X = MonadCtx $ get4

put5X :: MonadState4 val m Safe => val -> MonadCtx (Value Pure) (Insert (Proxy StateT3) Empty) m Safe (Value Pure Safe ())
put5X = MonadCtx . put4

--put4X :: v -> MonadCtx PureS (Insert (Proxy StateT3) Empty) m Safe v <= MonadState4 v m Safe
--put4X = MonadCtx . put4


--tstB1 = val 0 `bindEnv_` val 1



-- !!!!!!!!! zrobic by runStateT3 dzialal na MonadCtx

--bindEnv_ :: (PolyBindEnv m1 m2 m3 a1 a2, PolyMonad m1 m2 m3) => m1 a1 -> m2 b -> m3 b

--m1 :: MonadCtx base set m Safe
--m2 :: MonadCtx base set m Safe

--put3X (val 0) :: MonadCtx base set m Safe (Value Pure (Safe a))

--tstB2 = put3X (val 0) `bindEnv_` put3X (val 0)

--tstB2 = put3X (val 0) `bindEnv_` val 1

--lift :: m a -> t m a

--lift2 :: m s a -> t (m s) ts a
--lift2 :: m a -> t m s a
--class Lift2 t s where
--    lift2 :: m a -> t m s a

--instance Lift2 Data where
--    func = 

--instance Lift2 (StateT3 s) where
--    lift m = StateT $ \s -> do
--        a <- m
--        return (a, s)

----zrobic liftowanie i instancje nizej
----instance MonadState2 v (t v m) where
----    put3' s = lift $ put3' s


--instance Monad3 (StateT2 s m) (UnsafeBase base err) s2 s3 where
--    bind3

--tst = do
--    liftIO $ print "hello" :: MonadIO m => Val m Safe2
--    raise E1               :: MonadIO m => Val m (Unsafe2 E1)
--    liftIO $ print "world" :: MonadIO m => Val m Safe2


--class Monad3 m safety1 safety2 safety3 | safety1 safety2 -> safety3 where
--    bind3   :: m safety1 a -> (a -> m safety2 b) -> m safety3 b

------------------------

instance Monad Safe2 where
    return = Safe2
    Safe2 a >>= f = f a

class Error a m where
    error' :: a -> m x

--instance Error E1 Safe2 where
--    error' _ = 

instance (Monad m, Monad safety) => Monad (StateT2 s m safety) where
    return a = StateT2 $ \s -> return $ return (a, s)
    --m >>= k  = StateT2 $ \s -> do
    --    ~(a, s') <- runStateT2 m s
    --    runStateT2 (k a) s'
    --fail str = failErr

failErr = StateT2 $ \_ -> return $ error' E1

    --get2 = StateT2 $ \s -> return (s, s)
foo2 :: (Monad m) => s -> StateT2 s m Safe2 s
foo2 s = StateT2 $ \_ -> return $ return (s, s)

data Val t m a = Val (t (m a)) deriving Show

instance x~Safe2 => MonadIO (Val IO x) where
    liftIO ioa = Val $ (fmap Safe2) ioa

instance Monad (Val t m) where

    
    --instance (Monad t, Monad m) => Monad (Val t m) where
    --    return = Val . return . return

--tst = do
--    foo2 (0::Int)
    --failErr

    --fail str = StateT $ \_ -> fail str

--toIO :: m~IO => m a -> m a; toIO = id

matchT :: a -> a -> a
matchT = const

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) } deriving (Typeable)


instance (Monad m) => Monad (StateT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s


instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask       = lift ask
    local f m = StateT $ \s -> local f (runStateT m s)


--class (Monad m) => MonadState s m | m -> s where
--    get :: m s
--    put :: s -> m (Safe (Value Pure (Safe ())))

--instance MonadState s (StateT s m) <= Monad m where
--    get   = StateT $ \s -> return (s, s)
--    put s = StateT $ \_ -> return (Safe $ val (), s)

--instance MonadState s (ReaderT s m) <= MonadState s m where
--    get   = lift $ get
--    put s = lift $ put s

--instance MonadState s (t s m) <= (MonadTrans (t s), MonadState s m, Monad (t s m)) where
--    get   = lift $ get
--    put s = lift $ put s

instance MFunctor (StateT s) where
    hoist nat m = StateT (\s -> nat (runStateT m s))

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

state f = StateT (return . f)


----------------------------------------------------------------------

--getX2 :: MonadCtx Pure (Insert (Proxy StateT) Empty) m s <= MonadState s m
--getX2 = MonadCtx get

--getX :: MonadCtx Pure (Insert (Proxy StateT) Empty) m s <= MonadState s m
--getX = MonadCtx get

--putX :: s -> MonadCtx Pure (Insert (Proxy StateT) Empty) m (Safe (Value Pure (Safe ()))) <= MonadState s m
--putX = MonadCtx . put

--askX :: MonadCtx Pure (Insert (Proxy ReaderT) Empty) m s <= MonadReader s m
--askX = MonadCtx ask

--getX' :: MonadCtx Pure (ConstrainSet () (Insert (Proxy StateT) Empty)) m s <= MonadState s m
--getX' = MonadCtx get

runStateT' a s = fmap Safe $ runStateT a s

-- FIXME[wd]: update
--runStateTX  = liftMonadRunner1 (Proxy :: Proxy StateT)  runStateT  . appMonadCtx
--runReaderTX = liftMonadRunner1 (Proxy :: Proxy ReaderT) runReaderT . appMonadCtx


runStateT3X a s = runMonad (Proxy::Proxy StateT3) (flip runStateT3 s) $ appMonadCtx $ unpackCtxWrapper $ a

--runStateTX''  = liftMonadRunner1'' (Proxy :: Proxy StateT)  runStateT
--runReaderTX'' = liftMonadRunner1'' (Proxy :: Proxy ReaderT) runReaderT


--liftMonadRunner1' :: MatchMonadCloseProto (IsEmpty (Remove mptr set)) (MonadCtx env (Remove mptr set) mb) t => mptr -> (ma a1 -> a -> mb b) -> MonadCtx env set ma a1 -> a -> t b
--liftMonadRunner1' (mf :: (t x (ma :: * -> *) a1 -> a -> mb b)) = liftMonadRunner1 (Proxy :: Proxy t) mf

--putX' = putX . Safe
--getX' = fmap fromSafe getX



--main = do
--    printType (1::Int)
    --NIE DZIALA:
    --runMonad (Proxy :: Proxy StateT3) (flip runStateT3 (swapCtx $ val2 (0::Int))) $ appMonadCtx $  put4X (val2 (1::Int)) `polyBind2` (\_ -> put4X (val2io (2::Int)))

    --print $ runMonad (Proxy :: Proxy StateT3) (flip runStateT3 (swapCtx $ val2 (0::Int))) $ appMonadCtx $  put4X (val2io (0::Int)) `polyBind2` (\_ -> (val2io (2::Int)))
    --czemu trzeba robic swapCtx tam ? Czy nie powinno to dzialac bez tego?

        --printType $ (val2 (0::Int)) `polyBind2` (\_ -> put4X $ val2 (2::Int))
    --printType (undefined :: Remove (Proxy StateT) (Proxy StateT, ()))
    --printType (undefined :: Remove (Proxy StateT3) (Proxy StateT3, ()))
    --printType (undefined :: Remove (Proxy StateT3) (Proxy StateT3, ()))
    --printType (undefined :: Remove (Proxy (StateT3 Int PureS Safe Int)) (Proxy (StateT3 Int PureS Safe Int),()))
    --printType $ runMonadProtoReq2 (Proxy :: Proxy StateT3) (flip runStateT3 (0::Int)) $ appMonadCtx $ put3X (0::Int)

--xxx = do
--    x <- getX
--    putX x




--newtype StateT3 v m (s :: * -> *) a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)


instance LiftValue2 Pure (StateT3 v m) <= Monad3R m Safe where
    liftValue2 a = StateT3 $ \v -> return3 Safe (fromSafe . fromPure . fromValue $ a,v)


instance LiftValue2 IO (StateT3 v m) <= (Monad3R m Safe, LiftValue2 IO m) where
    liftValue2 a = StateT3 $ \v -> liftValue2 $ fmap (\x -> (x,v)) a
    --liftValue2 a = StateT3 $ \v -> (lift . liftIO . fmap fromSafe . fromValue $ a,v)


instance LiftValue2 IO (Value IO) where
    liftValue2 = id

instance LiftValue2 Pure (Value Pure) where
    liftValue2 = id



instance LiftValue base (StateT3 v m) <= (Monad3R m Safe, LiftValue base m, Functor base) where
    liftValue a = StateT3 $ \v -> liftValue $ fmap (\x -> (x,v)) a


instance LiftValue a (Value a) where
    liftValue = id


--class LiftValue' m t where
--    liftValue' :: Functor s => m s a -> t m s a




instance LiftValue' base s (StateT3 v) <= Functor (base s) where
    liftValue' a = StateT3 $ \v -> fmap (\x -> (x,v)) a


--class LiftValue m t where
--    liftValue :: Value m s a -> t s a

--class LiftValue2 m t where
--    liftValue2 :: Value m Safe a -> t Safe a



--newtype StateT3 v m (s :: * -> *) a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)
