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

----------------------------------------------------------------------
-- Session
----------------------------------------------------------------------

type SessionMonad m = MonadPragmaStore m
type SessionT m a = PragmaStoreT m a

runT s = PragmaStore.runT s mempty

