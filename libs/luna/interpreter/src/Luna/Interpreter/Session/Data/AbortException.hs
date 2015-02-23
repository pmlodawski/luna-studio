---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Interpreter.Session.Data.AbortException where

import Control.Exception.Base (Exception)

import Flowbox.Prelude hiding (Context, error)



data AbortException = AbortException
    deriving (Show, Typeable)

instance Exception AbortException

