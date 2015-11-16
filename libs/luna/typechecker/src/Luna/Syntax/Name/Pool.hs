{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Luna.Syntax.Name.Pool where

-- FIXME[wd]: fix Data.Pool implementation and update the code

--import Flowbox.Prelude

--import qualified Control.Monad.State as State
--import Data.Pool
--import Luna.Syntax.Name
--import Data.Tuple (swap)

--data NamePoolState = NamePoolState { _varNames  :: Pool Name
--                                   , _typeNames :: Pool Name
--                                   }

--makeLenses ''NamePoolState


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

--requestVarName :: MonadNamePool m => m Name
--requestVarName = modify $ mapOver varNames $ fmap swap request

--requestTypeName :: MonadNamePool m => m Name
--requestTypeName = modify $ mapOver typeNames $ fmap swap request

--newNamePool :: IsString a => String -> String -> [Char] -> [Char] -> Pool a
--newNamePool preffix suffix base chars = Pool
--                                      $ drop 1
--                                      $ fmap fromString
--                                      $ fmap (preffix <>)
--                                      $ concat
--                                      $ iterate permute [suffix] where
--    bases     = return <$> base
--    permute a = fmap (:) chars <*> a

--varNamePool :: IsString a => Pool a
--varNamePool  = newNamePool "" "#" ['a' .. 'z'] ['a' .. 'z']

--typeNamePool :: IsString a => Pool a
--typeNamePool = newNamePool "" "#" ['A' .. 'Z'] ['a' .. 'z']

--instance Default NamePoolState where
--    def = NamePoolState varNamePool typeNamePool
