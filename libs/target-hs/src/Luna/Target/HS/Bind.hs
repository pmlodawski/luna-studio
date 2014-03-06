---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Bind where

import           Prelude hiding ((>>=), (>>), fail, return)
import qualified Prelude 

import Luna.Target.HS.Base
import Luna.Target.HS.Data 

(>>=) = bind
(>>)  = bind_
fail _ = undefined
return = id

--returnIO :: a -> IO a
--returnIO = return