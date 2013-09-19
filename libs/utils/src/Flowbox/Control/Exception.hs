---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

module Flowbox.Control.Exception where

import qualified Control.Exception as Exc

import           Flowbox.Prelude     



try a = do
    r <- Exc.try a
    return $ case r of
        Right _                     -> r
        Left (e::Exc.SomeException) -> r
