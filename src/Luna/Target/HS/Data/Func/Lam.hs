---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------


{-# LANGUAGE DeriveDataTypeable #-}
!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Data.Func.Lam where

import Luna.Target.HS.Data.Func.App
import Data.Typeable
import Luna.Target.HS.Control.Flow.Env

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

data Lam lam = Lam lam deriving (Typeable)


----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

mkLam lam args = val $ AppH (Lam lam, args)