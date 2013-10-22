{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FlowboxM.Luna.Std where

import FlowboxM.Luna.Data

type List a = [a]
instance Get0 (Pure [a]) (Pure [a]) where get0 = id

con_True  = Pure True
con_False = Pure False
instance Get0 (Pure Bool) (Pure Bool) where get0 = id
