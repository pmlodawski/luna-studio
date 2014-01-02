---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module FlowboxM.Luna.Utils where

import           Data.Typeable (Typeable, typeOf)

import FlowboxM.Luna.Base
import FlowboxM.Luna.Data
import FlowboxM.Luna.Imports

------------------------------------------------------------------------
-- Display utils
------------------------------------------------------------------------

instance (Typeable a) => Show (IO a) where
    show e = '<' : (show . typeOf) e ++ ">"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '<' : (show . typeOf) e ++ ">"

------------------------------------------------------------------------
-- Func utils
------------------------------------------------------------------------

val = Pure . Safe

call0 a = call a ()
call1 a v1 = call a (OneTuple v1)
call2 a v1 v2 = call a (v1,v2)
call3 a v1 v2 v3 = call a (v1,v2,v3)
call4 a v1 v2 v3 v4 = call a (v1,v2,v3,v4)
call5 a v1 v2 v3 v4 v5 = call a (v1,v2,v3,v4,v5)
call6 a v1 v2 v3 v4 v5 v6 = call a (v1,v2,v3,v4,v5,v6)
call7 a v1 v2 v3 v4 v5 v6 v7 = call a (v1,v2,v3,v4,v5,v6,v7)
call8 a v1 v2 v3 v4 v5 v6 v7 v8 = call a (v1,v2,v3,v4,v5,v6,v7,v8)
call9 a v1 v2 v3 v4 v5 v6 v7 v8 v9 = call a (v1,v2,v3,v4,v5,v6,v7,v8,v9)

flattenCtx a = fmap flattenErr $ (flattenEnv $ fmap flipCtx a)

throw :: Pure (Safe a) -> Pure b -> Pure(Either b a)
throw (Pure (Safe a)) (Pure b) = Pure $ Left b

dot0  = ($)
dot1  = (.)
dot2  = dot1 . (.)
dot3  = dot2 . (.)
dot4  = dot3 . (.)
dot5  = dot4 . (.)
dot6  = dot5 . (.)
dot7  = dot6 . (.)
dot8  = dot7 . (.)
dot9  = dot8 . (.)
dot10 = dot9 . (.)

call v args = flattenCtx $ liftf2 callProto v (val args)

print' s = print s >> return (Safe ())

print'' :: (EvalEnvProto a IO (Pure b), Show b) => a -> IO (Safe ())
print'' s = eval s >>= print >> return (Safe ())

------------------------------------------------------------------------
-- Lifted functions
------------------------------------------------------------------------

(~+) = liftf2 (+)
(~-) = liftf2 (-)
(~*) = liftf2 (*)
(~<) = liftf2 (<)
(~>) = liftf2 (>)
(~:) = liftf2 (:)
(~!!) = flattenCtx `dot2` liftf2 (!!)
tuple2 = liftf2 (,)

map' = liftf2 map

each' a f = do
    map' f a

ifthenelse = liftf3 (\cond tval fval -> if cond then tval else fval)


exIO_1 :: IO (Safe Int)
exIO_1 = return (Safe 1)



