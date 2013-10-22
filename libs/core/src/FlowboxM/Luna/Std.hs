{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FlowboxM.Luna.Std where

import FlowboxM.Luna.Data
import FlowboxM.Luna.Utils

type List a = [a]
instance Get0 (Pure [a]) (Pure [a]) where get0 = id

con_True  = Pure True
con_False = Pure False
instance Get0 (Pure Bool) (Pure Bool) where get0 = id

instance Get0 (Pure Char) (Pure Char) where get0 = id



-- operators
(~^) = defFunction2 (\a b -> get1 (member (Proxy :: Proxy "pow") (get0 a)) b)
(~*) = defFunction2 (\a b -> get1 (member (Proxy :: Proxy "mul") (get0 a)) b)
(~/) = defFunction2 (\a b -> get1 (member (Proxy :: Proxy "div") (get0 a)) b)
(~+) = defFunction2 (\a b -> get1 (member (Proxy :: Proxy "add") (get0 a)) b)
(~-) = defFunction2 (\a b -> get1 (member (Proxy :: Proxy "sub") (get0 a)) b)