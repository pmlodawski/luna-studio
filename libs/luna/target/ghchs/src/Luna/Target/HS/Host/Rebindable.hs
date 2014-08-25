---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Host.Rebindable (
    module Luna.Target.HS.Host.Rebindable,
    module Prelude,
    ifThenElse
) where

import           Prelude hiding ((>>=),(>>), return, fail)
import qualified Prelude
import           Luna.Target.HS.Host.Wrapper (ifThenElse)

import Luna.Target.HS.Control.Context.Bind

(>>=)  = bindEnv
(>>)   = bindEnv_
fail _ = undefined
return = Prelude.return
