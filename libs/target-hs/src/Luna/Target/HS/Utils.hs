---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Luna.Target.HS.Utils where

import Control.Applicative
import Data.Typeable (Typeable, typeOf)

import Luna.Target.HS.Base
import Luna.Target.HS.Data
import Luna.Target.HS.Imports

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



call0 a = call a ()
call1 a v1 = call a (v1,())
--call1 a v1 = call a (OneTuple v1)
--call2 a v1 v2 = call a (v1,v2)
--call3 a v1 v2 v3 = call a (v1,v2,v3)
--call4 a v1 v2 v3 v4 = call a (v1,v2,v3,v4)
--call5 a v1 v2 v3 v4 v5 = call a (v1,v2,v3,v4,v5)
--call6 a v1 v2 v3 v4 v5 v6 = call a (v1,v2,v3,v4,v5,v6)
--call7 a v1 v2 v3 v4 v5 v6 v7 = call a (v1,v2,v3,v4,v5,v6,v7)
--call8 a v1 v2 v3 v4 v5 v6 v7 v8 = call a (v1,v2,v3,v4,v5,v6,v7,v8)
--call9 a v1 v2 v3 v4 v5 v6 v7 v8 v9 = call a (v1,v2,v3,v4,v5,v6,v7,v8,v9)

flattenCtx :: (Functor m1, Functor m3, FlipCtx s1 m2, FlattenEnv m1 m2 m3, FlattenErr s1 s2 s3) 
           => m1(s1(m2(s2 a))) -> m3(s3 a)
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


rTuple1 a = (a,())
rTuple2 a = (a,) `dot1` rTuple1
rTuple3 a = (a,) `dot2` rTuple2
rTuple4 a = (a,) `dot3` rTuple3
rTuple5 a = (a,) `dot4` rTuple4
rTuple6 a = (a,) `dot5` rTuple5
rTuple7 a = (a,) `dot6` rTuple6
rTuple8 a = (a,) `dot7` rTuple7
rTuple9 a = (a,) `dot8` rTuple8

type RTuple1 v1 = (v1,())
type RTuple2 v1 v2 = (v1, RTuple1 v2)
type RTuple3 v1 v2 v3 = (v1, RTuple2 v2 v3)
type RTuple4 v1 v2 v3 v4 = (v1, RTuple3 v2 v3 v4)
type RTuple5 v1 v2 v3 v4 v5 = (v1, RTuple4 v2 v3 v4 v5)

call v args = flattenCtx $ liftf2 callProto v (val args)
call' v args = liftf2 callProto v (val args)

member :: (MemberProto name base (Pure(Safe handler))) => proxy name -> base -> Pure (Safe (AppH handler base))
member proxy base = fcurry base $ memberProto proxy base

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
--(~:) = liftf2 (:)

--(~:) :: (Pure(Safe Int)) -> (Pure(Safe[(Pure(Safe Int))])) -> (Pure(Safe[(Pure(Safe Int))]))
x ~: xs = (fmap.fmap) (x:) xs


(~!!) = flattenCtx `dot2` liftf2 (!!)
tuple2 = val `dot2` (,)

map' = liftf2 map

each' a f = do
    map' f a

--ifThenElse = liftf3 (\cond tval fval -> if cond then tval else fval)

ifThenElse cond tval fval = flattenCtx $ (fmap.fmap) (\c -> if c then tval else fval) cond

exIO_1 :: IO (Safe Int)
exIO_1 = return (Safe 1)

nop = (pure.pure) ()

------------------------------------------------------------------------
-- Errors catch
------------------------------------------------------------------------

--catchProto :: Either err val -> (Pure(Safe err) -> Pure(Safe val)) -> Pure(Safe val)
catchProto f el = case el of
    Left err -> call1 f $ val err
    Right el -> val el

catch el f = flattenEnv $ fmap (catchProto f) el


