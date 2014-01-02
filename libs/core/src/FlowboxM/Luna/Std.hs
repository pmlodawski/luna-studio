{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FlowboxM.Luna.Std where

import FlowboxM.Luna.Base
import FlowboxM.Luna.Data
import FlowboxM.Luna.Utils

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
