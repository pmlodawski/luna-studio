{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Config
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module Luna.System.Session where

import Flowbox.Prelude

import           Control.Monad.State    (StateT, runStateT, MonadState)
import qualified Control.Monad.State    as State
import           Control.Monad          (MonadPlus)
import           Control.Monad.Identity (Identity, runIdentity)
import           Luna.System.Pragma     (HasPragmaSet(pragmaSet), PragmaSet)
import qualified Luna.System.Pragma     as Pragma
import           Luna.System.Pragma     (Pragma, SwitchPragma, PragmaCons, PragmaVal, RegisterError, AccessError, LookupError, IsPragma, PragmaDef, PragmaInst)
import           Control.Monad.Trans    (MonadTrans)

----------------------------------------------------------------------
-- Session
----------------------------------------------------------------------

-- == Types ==

type    Session a    = SessionT Identity a
newtype SessionT m a = SessionT { unSessionT :: StateT Config m a } 
                     deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

type SessionCtx m     = (SessionMonad m, Functor m, Applicative m)
type Ctx        t m a = (SessionCtx m, IsPragma a, PragmaCons t)

-- == Instances ==

class Monad m => SessionMonad m where
    get :: m Config
    put :: Config -> m ()

instance MonadState s m => MonadState s (SessionT m) where
    get = SessionT . lift $ State.get
    put = SessionT . lift . State.put

instance Monad m => SessionMonad (SessionT m) where
    get = SessionT $ State.get
    put = SessionT . State.put

instance (MonadTrans t, SessionMonad m, Monad (t m)) => SessionMonad (t m) where
    get = lift get
    put = lift . put


-- == Utils ==


runT :: SessionT m a -> Config -> m (a, Config)
runT = runStateT . unSessionT

run :: Session a -> Config -> (a,Config)
run = runIdentity .: runT

defrunT :: SessionT m a -> m (a, Config)
defrunT = flip runT def

defrun :: Session a -> (a, Config)
defrun = flip run def

withSession :: (SessionCtx m, Traversable t) => (Config -> t Config) -> m (t Config)
withSession f = do
    out <- f <$> get
    traverse put out
    return out

registerPragma :: Ctx t m a => PragmaDef t a -> m (Either RegisterError Config)
registerPragma = withSession . Pragma.register

pushPragma :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError Config)
pushPragma = withSession .: Pragma.push

setPragma :: Ctx t m a => PragmaDef t a -> PragmaVal t a -> m (Either AccessError Config)
setPragma = withSession .: Pragma.set

lookupPragma :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
lookupPragma p = Pragma.lookup p <$> get

popPragma :: Ctx t m a => PragmaDef t a -> m (Either LookupError (PragmaInst t a))
popPragma p = do 
    lup <- Pragma.pop p <$> get
    traverse_ (put.snd) lup
    return $ fmap fst lup

enablePragma :: (SessionCtx m, Typeable a, IsPragma a) => SwitchPragma a -> m (Either AccessError Config)
enablePragma = withSession . Pragma.enable

disablePragma :: (SessionCtx m, Typeable a, IsPragma a) => SwitchPragma a -> m (Either AccessError Config)
disablePragma = withSession . Pragma.disable

----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data Config = Config { __pragmaSet :: PragmaSet } deriving (Show, Generic, Typeable)
makeLenses ''Config

instance HasPragmaSet Config where
    pragmaSet = _pragmaSet

instance Default Config where
    def = Config def 

