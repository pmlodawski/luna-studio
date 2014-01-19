---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Bind where

import Prelude hiding ((>>=), (>>), fail, return)

import Luna.Target.HS.Base

(>>=) = bind
(>>)  = bind_
fail = ()
return a = a