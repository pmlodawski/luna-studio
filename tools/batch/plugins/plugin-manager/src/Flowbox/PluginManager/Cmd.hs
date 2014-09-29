---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.PluginManager.Cmd where

import Flowbox.Bus.Data.Prefix (Prefix)
import Flowbox.Prelude



data Cmd = Run { _initConfig :: FilePath

               , _prefix     :: Prefix

               , _verbose    :: Int
               , _noColor    :: Bool
               }
         | Version
         deriving Show

makeLenses(''Cmd)
