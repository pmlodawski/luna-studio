---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Status where

import           Flowbox.Prelude



data CacheStatus = Ready
                 | Modified
                 | Affected
                 | NonCacheable
                 deriving (Show, Eq)

merge :: CacheStatus -> CacheStatus -> CacheStatus
merge NonCacheable _            = NonCacheable
merge _            NonCacheable = NonCacheable
merge Modified     _            = Modified
merge _            Modified     = Modified
merge Affected     _            = Affected
merge _            Affected     = Affected
merge Ready        Ready        = Ready
