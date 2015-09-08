{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

import qualified Luna.System.Pragma.Store as PragmaStore
import           Luna.System.Pragma.Store (PragmaStoreT, MonadPragmaStore)
import           Control.Monad.State      (StateT, evalStateT)
import           Luna.System.Config       (Config)
import qualified Luna.System.Config       as Config
import qualified Luna.System.Env          as Env

----------------------------------------------------------------------
-- Session
----------------------------------------------------------------------

type SessionMonad m = MonadPragmaStore m
type SessionT m a = PragmaStoreT m a

--runT s = flip PragmaStore.runT mempty
--       $ flip runStateT (0::Int) s

--runT :: PragmaStoreT m a -> m a
run cfg session = flip PragmaStore.evalT mempty
                . flip Config.evalT cfg
                $ Env.evalT session =<< defM
      

--runT2 s = flip runStateT (0::Int) 
--        $ flip PragmaStore.runT mempty s


