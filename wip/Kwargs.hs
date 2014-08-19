{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}


{-# LANGUAGE TupleSections #-}



class Call ptr args result | ptr args -> result where
    callProto :: ptr -> args -> result


class TestC el baseFunc func | el baseFunc -> func where
    testC :: el -> baseFunc -> func


data X = X deriving Show

instance TestC X (a->Int->c) (a->c) where
    testC _ = applyOffset1 (5::Int)




applyOffset0 x f = f x
applyOffset1 x f = (\a1 -> f a1 x)
applyOffset2 x f = (\a1 a2 -> f a1 a2 x)
applyOffset3 x f = (\a1 a2 a3 -> f a1 a2 a3 x)
applyOffset4 x f = (\a1 a2 a3 a4 -> f a1 a2 a3 a4 x)
applyOffset5 x f = (\a1 a2 a3 a4 a5 -> f a1 a2 a3 a4 a5 x)
applyOffset6 x f = (\a1 a2 a3 a4 a5 a6 -> f a1 a2 a3 a4 a5 a6 x)
applyOffset7 x f = (\a1 a2 a3 a4 a5 a6 a7 -> f a1 a2 a3 a4 a5 a6 a7 x)
applyOffset8 x f = (\a1 a2 a3 a4 a5 a6 a7 a8 -> f a1 a2 a3 a4 a5 a6 a7 a8 x)
applyOffset9 x f = (\a1 a2 a3 a4 a5 a6 a7 a8 a9 -> f a1 a2 a3 a4 a5 a6 a7 a8 a9 x)


applyOverride1 :: t1 -> (t1 -> t2) -> (t1 -> t2)
applyOverride1 x f = (\_ -> f x)

applyOverride2 :: t2 -> (t1 -> t2 -> t3) -> (t1 -> t2 -> t3)
applyOverride2 x f = (\a1 _ -> f a1 x)

applyOverride3 :: t3 -> (t1 -> t2 -> t3 -> t4) -> (t1 -> t2 -> t3 -> t4)
applyOverride3 x f = (\t1 t2 _ -> f t1 t2 x)

applyOverride4 :: t4 -> (t1 -> t2 -> t3 -> t4 -> t5) -> (t1 -> t2 -> t3 -> t4 -> t5)
applyOverride4 x f = (\t1 t2 t3 _ -> f t1 t2 t3 x)

applyOverride5 :: t5 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6) -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6)
applyOverride5 x f = (\t1 t2 t3 t4 _ -> f t1 t2 t3 t4 x)

applyOverride6 :: t6 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7) -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7)
applyOverride6 x f = (\t1 t2 t3 t4 t5 _ -> f t1 t2 t3 t4 t5 x)

applyOverride7 :: t7 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8) -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8)
applyOverride7 x f = (\t1 t2 t3 t4 t5 t6 _ -> f t1 t2 t3 t4 t5 t6 x)

applyOverride8 :: t8 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9) -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9)
applyOverride8 x f = (\t1 t2 t3 t4 t5 t6 t7 _ -> f t1 t2 t3 t4 t5 t6 t7 x)

applyOverride9 :: t9 -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10) -> (t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10)
applyOverride9 x f = (\t1 t2 t3 t4 t5 t6 t7 t8 _ -> f t1 t2 t3 t4 t5 t6 t7 t8 x)

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

type RTuple0 = ()
type RTuple1 v1 = (v1,())
type RTuple2 v1 v2 = (v1, RTuple1 v2)
type RTuple3 v1 v2 v3 = (v1, RTuple2 v2 v3)
type RTuple4 v1 v2 v3 v4 = (v1, RTuple3 v2 v3 v4)
type RTuple5 v1 v2 v3 v4 v5 = (v1, RTuple4 v2 v3 v4 v5)

type LTuple0 = ()
type LTuple1 v1 = ((),v1)
type LTuple2 v1 v2 = (LTuple1 v2, v1)
type LTuple3 v1 v2 v3 = (LTuple2 v2 v3, v1)
type LTuple4 v1 v2 v3 v4 = (LTuple3 v2 v3 v4, v1)
type LTuple5 v1 v2 v3 v4 v5 = (LTuple4 v2 v3 v4 v5, v1)

rTuple0   = ()
rTuple1 v = (v,) `dot0` rTuple0
rTuple2 v = (v,) `dot1` rTuple1
rTuple3 v = (v,) `dot2` rTuple2
rTuple4 v = (v,) `dot3` rTuple3
rTuple5 v = (v,) `dot4` rTuple4
rTuple6 v = (v,) `dot5` rTuple5
rTuple7 v = (v,) `dot6` rTuple6
rTuple8 v = (v,) `dot7` rTuple7
rTuple9 v = (v,) `dot8` rTuple8

lTuple0   = ()
lTuple1 v = (,v) `dot0` lTuple0
lTuple2 v = (,v) `dot1` lTuple1
lTuple3 v = (,v) `dot2` lTuple2
lTuple4 v = (,v) `dot3` lTuple3
lTuple5 v = (,v) `dot4` lTuple4
lTuple6 v = (,v) `dot5` lTuple5
lTuple7 v = (,v) `dot6` lTuple6
lTuple8 v = (,v) `dot7` lTuple7
lTuple9 v = (,v) `dot8` lTuple8

class R2LTuple f g | f -> g where
    r2lTuple :: f -> g

instance R2LTuple b c => R2LTuple (a,b) (c,a) where
    r2lTuple (a,b) = (r2lTuple b,a)

instance R2LTuple () () where
    r2lTuple = id


class H2RTuple f g | f -> g where
    l2rTuple :: f -> g

instance H2RTuple a c => H2RTuple (a,b) (b,c) where
    l2rTuple (a,b) = (b, l2rTuple a)

instance H2RTuple () () where
    l2rTuple = id


ltupleInsert a t = r2lTuple (a, l2rTuple t)

main = do
    let x = lTuple2 1 2 :: LTuple2 Int Int
        x' = l2rTuple x :: RTuple2 Int Int
        --y  = r2lTuple x'
    print $ x
    print x'
    --print y
    --print $ reverseMe 
    print "hello"