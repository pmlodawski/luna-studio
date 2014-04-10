---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AccountManager.Context where

--import           Control.Applicative
--import           Data.IORef          (IORef)
--import qualified Data.IORef          as IORef

import Flowbox.Prelude hiding (Context)



data Context = Context


mk :: IO Context
mk = return Context
