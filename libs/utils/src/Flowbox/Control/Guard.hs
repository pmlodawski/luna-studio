---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Control.Guard where

import qualified Control.Exception.Base as Exception

import Flowbox.Prelude



protect :: IO a -> (Exception.SomeException -> IO ()) ->  IO a
protect action onError =
    Exception.catch action (\e -> onError e >> protect action onError)
