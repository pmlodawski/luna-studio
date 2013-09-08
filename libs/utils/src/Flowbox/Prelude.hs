---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Prelude(
	module Flowbox.Prelude,
	module Prelude
) where

import           Prelude                hiding (print, putStr, putStrLn)
import qualified Prelude                as Prelude
import           Control.Monad.IO.Class   (liftIO)


print    = liftIO . Prelude.print
putStr   = liftIO . Prelude.putStr
putStrLn = liftIO . Prelude.putStrLn
