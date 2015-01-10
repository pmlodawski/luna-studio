{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Pragma.Store
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
----------------------------------------------------------------------------

module Luna.System.Pragma.Store where

import           Flowbox.Prelude     as P hiding (noneOf, lookup)

import qualified Luna.System.Pragma  as Pragma
import           Luna.System.Pragma  (PragmaMap, PragmaDef, PragmaVal, PragmaInst, RegisterError, AccessError, LookupError, IsPragma, PragmaCons, SwitchPragma)
import           Control.Monad.State (MonadState, StateT, runStateT)
import qualified Control.Monad.State as State

----------------------------------------------------------------------
-- PragmaMap
----------------------------------------------------------------------

type    PragmaStore      = PragmaStoreT Identity
newtype PragmaStoreT m a = PragmaStoreT { unPragmaStoreT :: StateT PragmaMap m a }
                         deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

class (Monad m, Applicative m) => MonadPragmaStore m where
    get :: m PragmaMap
    put :: PragmaMap -> m ()


-- == Instances ==

instance MonadState s m => MonadState s (PragmaStoreT m) where
    get = PragmaStoreT . lift $ State.get
    put = PragmaStoreT . lift . State.put

instance (Monad m, Functor m) => MonadPragmaStore (PragmaStoreT m) where
    get = PragmaStoreT $ State.get
    put = PragmaStoreT . State.put

instance (MonadTrans t, MonadPragmaStore m, Monad (t m), Applicative (t m)) => MonadPragmaStore (t m) where
    get = lift get
    put = lift . put
__overlapping__ = run get mempty


-- == State utils ==

runT :: PragmaStoreT m a -> PragmaMap -> m (a, PragmaMap)
runT = runStateT . unPragmaStoreT

run :: PragmaStore a -> PragmaMap -> (a,PragmaMap)
run = runIdentity .: runT

defrunT :: PragmaStoreT m a -> m (a, PragmaMap)
defrunT = flip runT def

defrun :: PragmaStore a -> (a, PragmaMap)
defrun = flip run def

-- == Pragma utils ==

type Ctx t m a = (MonadPragmaStore m, IsPragma a, PragmaCons t)

withSession :: (MonadPragmaStore m, Traversable t) => (PragmaMap -> t PragmaMap) -> m (t PragmaMap)
withSession f = do
    out <- f <$> get
    traverse put out
    return out

registerPragma :: Ctx t m a => PragmaDef t a -> m (Either RegisterError PragmaMap)
registerPragma = withSession . Pragma.register

pushPragma :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError PragmaMap)
pushPragma = withSession .: Pragma.push

setPragma :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError PragmaMap)
setPragma = withSession .: Pragma.set

lookupPragma :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
lookupPragma p = Pragma.lookup p <$> get

popPragma :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
popPragma p = do 
    lup <- Pragma.pop p <$> get
    traverse_ (put.snd) lup
    return $ fmap fst lup

enablePragma :: (IsPragma a, MonadPragmaStore m) => SwitchPragma a -> m (Either AccessError PragmaMap)
enablePragma = withSession . Pragma.enable

disablePragma :: (IsPragma a, MonadPragmaStore m) => SwitchPragma a -> m (Either AccessError PragmaMap)
disablePragma = withSession . Pragma.disable
