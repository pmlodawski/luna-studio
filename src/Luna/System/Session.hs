{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Luna.System.Pragma     (Pragma)

----------------------------------------------------------------------
-- Session
----------------------------------------------------------------------

-- == Types ==

type    Session a    = SessionT Identity a
newtype SessionT m a = SessionT { unSessionT :: StateT Config m a } 
                     deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

type SessionCtx m   = (SessionMonad m, Functor m, Applicative m)
type PragmaCtx  m a = (SessionCtx m, Typeable a)

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

-- == Utils ==

withSession :: SessionCtx m => (Config -> Config) -> m Config
withSession f = do
    out <- f <$> get
    put out
    return out

withSession_ :: SessionCtx m => (Config -> Config) -> m ()
withSession_ f = withSession_ f *> pure ()

runSessionT :: SessionT m a -> Config -> m (a, Config)
runSessionT = runStateT . unSessionT

runSession :: Session a -> Config -> (a,Config)
runSession = runIdentity .: runSessionT

-- Pragmas

pushPragma :: PragmaCtx m a => Pragma a -> a -> m Config
pushPragma = withSession .: Pragma.pushPragma

setPragma :: PragmaCtx m a => Pragma a -> a -> m Config
setPragma = withSession .: Pragma.setPragma

lookupPragma :: PragmaCtx m a => Pragma a -> m (Pragma.Lookup a)
lookupPragma p = Pragma.lookupPragma p <$> get

popPragma :: PragmaCtx m a => Pragma a -> m (Pragma.Lookup a)
popPragma p = do 
    lup <- Pragma.popPragma p <$> get
    traverse_ (put.snd) lup
    return $ fmap fst lup

----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data Config = Config { __pragmaSet :: PragmaSet } deriving (Show, Generic, Typeable)
makeLenses ''Config

instance HasPragmaSet Config where
    pragmaSet = _pragmaSet

instance Default Config where
    def = Config def 

