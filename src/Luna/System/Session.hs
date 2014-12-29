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
import           Luna.System.Pragma     (Pragma, SwitchPragma)
import qualified Data.HTSet.HTSet       as HTSet

----------------------------------------------------------------------
-- Session
----------------------------------------------------------------------

-- == Types ==

type    Session a    = SessionT Identity a
newtype SessionT m a = SessionT { unSessionT :: StateT Config m a } 
                     deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

type SessionCtx m         = (SessionMonad m, Functor m, Applicative m)
type PragmaCtx  m a t rep = (SessionCtx m, Pragma.PragmaCons t a rep, HTSet.IsKey (Pragma t a) (Pragma.PragmaStack rep), Typeable rep) 

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

runT :: SessionT m a -> Config -> m (a, Config)
runT = runStateT . unSessionT

run :: Session a -> Config -> (a,Config)
run = runIdentity .: runT

defrunT :: SessionT m a -> m (a, Config)
defrunT = flip runT def

defrun :: Session a -> (a, Config)
defrun = flip run def

-- Pragmas

registerPragma :: PragmaCtx  m a t rep => Pragma t a -> m Config
registerPragma = withSession . Pragma.registerPragma

pushPragma :: PragmaCtx  m a t rep => Pragma t a -> rep -> m Config
pushPragma = withSession .: Pragma.pushPragma

setPragma :: PragmaCtx  m a t rep => Pragma t a -> rep -> m Config
setPragma = withSession .: Pragma.setPragma

lookupPragma :: PragmaCtx  m a t rep => Pragma t a -> m (Pragma.Lookup rep)
lookupPragma p = Pragma.lookupPragma p <$> get

popPragma :: PragmaCtx  m a t rep => Pragma t a -> m (Pragma.Lookup rep)
popPragma p = do 
    lup <- Pragma.popPragma p <$> get
    traverse_ (put.snd) lup
    return $ fmap fst lup

enablePragma :: (PragmaCtx m a Pragma.Switch rep, Typeable a) => SwitchPragma a -> m Config
enablePragma = withSession . Pragma.enablePragma

disablePragma :: (PragmaCtx m a Pragma.Switch rep, Typeable a) => SwitchPragma a -> m Config
disablePragma = withSession . Pragma.disablePragma

----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data Config = Config { __pragmaSet :: PragmaSet } deriving (Show, Generic, Typeable)
makeLenses ''Config

instance HasPragmaSet Config where
    pragmaSet = _pragmaSet

instance Default Config where
    def = Config def 

