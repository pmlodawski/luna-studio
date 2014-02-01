{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Target.HS.Std (
	module Luna.Target.HS.Std,
	module Data.List
)where

import Luna.Target.HS.Base
import Luna.Target.HS.Data
import Luna.Target.HS.Utils

import Data.List

--instance Call (Pure a) () (Pure a) where
--    call val _ = val

--instance Call (m(s([a]))) () (m(s([a]))) where
--    call val _ = val

--instance Call (m(s(Int))) () (m(s(Int))) where
--    call val _ = val


--instance Call (IO a) () (IO a) where
--    call val _ = val

type List a = [a]


concatPure a                  = map val $ concat a
rangeFromTo (Pure(Safe a)) (Pure(Safe b)) = if a < b then [a..b] else [a,a-1..b]
rangeFrom   (Pure(Safe a))                = [a..]