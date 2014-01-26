---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Debug.Debug where

import qualified Debug.Trace as Debug

import Prelude

dtrace :: Show a => a -> b -> b
dtrace  = Debug.trace . show

dtraceM :: (Monad m, Show a) => a -> m ()
dtraceM s = Debug.trace (show s) (return ())
