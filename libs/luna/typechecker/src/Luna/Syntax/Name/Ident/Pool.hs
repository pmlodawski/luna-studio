{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Name.Ident.Pool where

import Prelude.Luna

import Data.Pool
import Luna.Syntax.Name.Ident.Class
import qualified Control.Monad.State            as State
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)


-----------------------
-- === IdentPool === --
-----------------------

-- === Definitions === --

data IdentPoolState = IdentPoolState { _varNames  :: Pool VarIdent
                                     , _typeNames :: Pool TypeIdent
                                     }

makeLenses ''IdentPoolState


-- === utils === --

--requestVarName :: MonadNamePool m => m Name
--requestVarName = modify $ mapOver varNames $ fmap swap request

--requestTypeName :: MonadNamePool m => m Name
--requestTypeName = modify $ mapOver typeNames $ fmap swap request

newNamePool :: IsString a => String -> String -> [Char] -> [Char] -> Pool a
newNamePool preffix suffix base chars = Pool
                                      $ drop 1
                                      $ fmap fromString
                                      $ fmap (preffix <>)
                                      $ concat
                                      $ iterate permute [suffix] where
    permute a = fmap (:) chars <*> a

varNamePool :: IsString a => Pool a
varNamePool  = newNamePool "" "#" ['a' .. 'z'] ['a' .. 'z']

typeNamePool :: IsString a => Pool a
typeNamePool = newNamePool "" "#" ['A' .. 'Z'] ['a' .. 'z']



-- === Isntances === --
--instance Default NamePoolState where
--    def = NamePoolState varNamePool typeNamePool



--------------------------------------------
-- === IdentPool state implementation === --
--------------------------------------------


-- === State implementation === --

---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    IdentPool  n e     = IdentPoolT n e Identity
newtype IdentPoolT n e m a = IdentPoolT (State.StateT IdentPoolState m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''IdentPoolT


-- === Utils === --

runT  ::            IdentPoolT n e m a -> IdentPoolState -> m (a, IdentPoolState)
evalT :: Monad m => IdentPoolT n e m a -> IdentPoolState -> m a
execT :: Monad m => IdentPoolT n e m a -> IdentPoolState -> m IdentPoolState

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: IdentPool n e a -> IdentPoolState -> (a, IdentPoolState)
eval :: IdentPool n e a -> IdentPoolState -> a
exec :: IdentPool n e a -> IdentPoolState -> IdentPoolState

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadIdentPool n e m => (IdentPoolState -> IdentPoolState) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadIdentPool n e m => (IdentPoolState -> (a, IdentPoolState)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadIdentPool n e m => (IdentPoolState -> m (a, IdentPoolState)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadIdentPool n e m => (IdentPoolState -> IdentPoolState) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadIdentPool n e m | m -> n e where
    get :: m IdentPoolState
    put :: IdentPoolState -> m ()

instance Monad m => MonadIdentPool n e (IdentPoolT n e m) where
    get = IdentPoolT   State.get ; {-# INLINE get #-}
    put = IdentPoolT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (IdentPoolT n e m) where
    get = IdentPoolT $ lift   State.get ; {-# INLINE get #-}
    put = IdentPoolT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadIdentPool n e m, MonadTrans t, Monad (t m)) => MonadIdentPool n e (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<





---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

--newtype NamePoolT m a = NamePoolT { fromNamePoolT :: State.StateT NamePoolState m a }
--                      deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative)

--type NamePool = NamePoolT Identity

--class Monad m => MonadNamePool m where
--    get :: m NamePoolState
--    put :: NamePoolState -> m ()

--instance Monad m => MonadNamePool (NamePoolT m) where
--    get = NamePoolT State.get
--    put = NamePoolT . State.put

--instance State.MonadState s m => State.MonadState s (NamePoolT m) where
--    get = NamePoolT (lift State.get)
--    put = NamePoolT . lift . State.put

--instance {-# OVERLAPPABLE #-} (MonadNamePool m, MonadTrans t, Monad (t m)) => MonadNamePool (t m) where
--    get = lift get
--    put = lift . put

--runT  ::            NamePoolT m a -> NamePoolState -> m (a, NamePoolState)
--evalT :: Monad m => NamePoolT m a -> NamePoolState -> m a
--execT :: Monad m => NamePoolT m a -> NamePoolState -> m NamePoolState

--runT  = State.runStateT  . fromNamePoolT
--evalT = State.evalStateT . fromNamePoolT
--execT = State.execStateT . fromNamePoolT

--run  :: NamePool a -> NamePoolState -> (a, NamePoolState)
--eval :: NamePool a -> NamePoolState -> a
--exec :: NamePool a -> NamePoolState -> NamePoolState

--run   = runIdentity .: runT
--eval  = runIdentity .: evalT
--exec  = runIdentity .: execT

--with :: MonadNamePool m => (NamePoolState -> NamePoolState) -> m b -> m b
--with f m = do
--    s <- get
--    put $ f s
--    out <- m
--    put s
--    return out

--modify :: MonadNamePool m => (NamePoolState -> (NamePoolState, a)) -> m a
--modify f = do
--    s <- get
--    let (s', a) = f s
--    put $ s'
--    return a

--modify_ :: MonadNamePool m => (NamePoolState -> NamePoolState) -> m ()
--modify_ = modify . fmap (,())

---- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
