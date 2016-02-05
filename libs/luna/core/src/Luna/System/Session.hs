{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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

import           Flowbox.Prelude

import           Control.Monad.State      (StateT, evalStateT)
import           Luna.System.Config       (Config)
import qualified Luna.System.Config       as Config
import qualified Luna.System.Env          as Env
import           Luna.System.Pragma.Store (MonadPragmaStore, PragmaStoreT)
import qualified Luna.System.Pragma.Store as PragmaStore

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


