{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}


--{-# LANGUAGE DysfunctionalDependencies #-}

import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

class Foo a b c | a b -> c where
    foo :: a -> (a -> b) -> c


--instance 

--tst :: (Num n, Foo n a b) => a -> b
--tst :: (a -> b) -> c
--tst = foo 5


newtype Rec1 t1 = Rec1 t1

data A a = A (AA a)
         | B (AB a)


newtype AA a = AA (a,a)
newtype AB a = AB a


data AnyType

bar :: Num a => a -> a 
--bar :: Int -> Int 
bar = (+1)

--main = do
--  let a = unsafeCoerce (5::Int) :: AnyType
--      b = bar $ unsafeCoerce a

--  print b
--  return ()


--class Func a b c where
--  func :: a -> b -> c



--class AppArg a f g | a f -> g where
--  appArg :: a -> f -> g


emptyArgs = (\() f -> f ())

app :: a -> (a -> b) -> b
app a f = f a

newtype Args f = Args { runArgs :: f }

--appArg :: a -> Args (f -> a -> b) -> Args (f -> b)
--appArg a (Args f) = Args (app a . f)

--app2 :: AppRTuple as f (a -> g) => a -> (as, f) -> ((a,as), f)
--app2 a (as, f) = ((a,as), f)
    --where x = appRTuple (a,as) f

class AppRTuple t f g where
    appRTuple :: t -> f -> g

instance (f~g) => AppRTuple () f g where
    appRTuple _ = id

instance (f~(a->b), AppRTuple as b g) => AppRTuple (a,as) f g where
    appRTuple (a,as) f = appRTuple as $ f a


class AppRTuple2 t f g where
    appRTuple2 :: t -> f -> g

instance (f~g) => AppRTuple2 () f g where
    appRTuple2 _ = id

instance (f~(a->b), AppRTuple2 as b g, pa~Provided a) => AppRTuple2 (pa,as) f g where
    appRTuple2 (a,as) f = appRTuple2 as $ f (fromProvided a)


test a b = (a,b)

tst2 = app []

--newtype Func kind f a = Func (f -> a)
--data F0
--data F1
--data F2
--data F3


newtype Func      (names :: [Maybe Symbol]) defs f   = Func { fromFunc :: (f,defs) } deriving (Functor)
newtype FuncTrans (names :: [Maybe Symbol]) defs f a = FuncTrans { runFuncTrans :: f -> Func names defs a }

remapFunc :: Func n d f -> Func n' d f
remapFunc = Func . fromFunc

runFunc2 (Func (f, defs)) = appRTuple2 defs f
runFunc = fst . fromFunc


remapFuncTrans :: FuncTrans n d f a -> FuncTrans n' d f a
remapFuncTrans = FuncTrans . fmap remapFunc . runFuncTrans

func :: d -> FuncTrans n d f f
func d = FuncTrans (\f -> Func (f,d))

--func = FuncTrans Func
--func :: Func k (a -> b) (a -> b)
--func = Func ($)
-- finalnie Func kind names f a !
data K (n :: Nat) = K deriving (Show)

type family Inc a where
    Inc (K n) = K (n+1)


--class NamedApp n ns ns' a f g | n ns -> ns' where
--    appNamedArg :: Proxy n -> a -> Func ns f -> Func ns' g





----------------------------------------------------------------------
-- ElemIndex
----------------------------------------------------------------------

type family ElemIndex (a :: k) (as :: [k]) where
    ElemIndex a (a ': as) = 0
    ElemIndex a (b ': as) = ElemIndex a as + 1

type family ElemIndex2 (a :: k) (as :: [Maybe k]) :: Nat where
    ElemIndex2 a (Just a ': as) = 0
    ElemIndex2 a (b ': as)      = ElemIndex2 a as + 1

type family FromJust a where
    FromJust (Just a) = a

type family Delete (a :: k) (as :: [k]) where
    Delete a (a ': as) = as
    Delete a (b ': as) = (b ': Delete a as)

----------------------------------------------------------------------
-- AppNth
----------------------------------------------------------------------

class AppNth (k::Nat) a f g where
    appNth :: Proxy k -> a -> f -> g

instance (f~(a -> b), g~b) => AppNth 0 a f g where
    appNth _ a = ($a)

instance (CmpNat n 0 ~ ok, AppNth' ok n a f g)
      => AppNth n a f g where
    appNth = appNth' (Proxy :: Proxy ok)

class AppNth' ok (k::Nat) a f g where
    appNth' :: Proxy ok -> Proxy k -> a -> f -> g

instance (f~(t -> s), g~(t -> u), AppNth (n-1) a s u) 
      => AppNth' GT n a f g where
    appNth' _ _ a = (\f t -> appNth (Proxy :: Proxy (n-1)) a (f t))

---

class DeleteNth (num::Nat) t t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNth 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNth (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNth num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)

--appByName :: forall idx a f g n ns d. (AppNth idx a f g, idx~ElemIndex n ns)
--          => Proxy n -> a -> Func ns d f -> Func (Delete n ns) d g
appByName (_::Proxy n) a (Func (f,d) :: Func ns d f) = 
    Func $ ( appNth (Proxy :: Proxy (ElemIndex2 n ns)) a f
           , deleteNth (Proxy :: Proxy (ElemIndex2 n ns)) d
           )

--appByNameW :: AppNth (ElemIndex n ns) a g g' 
--           => Proxy n -> a -> FuncTrans ns d f g -> FuncTrans (Delete n ns) d f g'
appByNameW n a (FuncTrans f) = FuncTrans $ fmap (appByName n a) f

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

data    Unprovided   = Unprovided  deriving (Show, Eq, Typeable)
newtype Provided   a = Provided { fromProvided :: a }  deriving (Show, Eq, Typeable, Functor)

newtype Arg (name :: Maybe Symbol) a = Arg a

defArg = Arg . Provided
arg    = Arg Unprovided



appArg :: a -> Func ns (d,ds) (a -> b) -> Func ns ds b
appArg a (Func (f,(d,ds))) = Func (f a,ds)

appArgW :: a -> FuncTrans (n ': ns) (d,ds) f (a -> b) -> FuncTrans ns ds f b
appArgW a (FuncTrans f) = FuncTrans $ (\f' -> remapFunc . appArg a $ f f')

tst a b c = (a,b,c)

tst' f = appNth (Proxy :: Proxy 1) [] f



type family SigNames s where
    SigNames () = '[]
    SigNames (Arg n p, as) = n ': SigNames as

--mkFunc :: sig -> FuncTrans (SigNames sig) d f f
--mkFunc = const func

sig = (arg::Arg Nothing Unprovided,(arg::Arg (Just "y") Unprovided,(defArg 5::Num a=>Arg (Just "z") (Provided a),())))
sig2 = (arg::Arg Nothing Unprovided,(arg::Arg (Just "y") Unprovided,(arg :: Arg (Just "z") Unprovided,())))
sig3 = (arg :: Arg Nothing Unprovided,())

extr :: sig -> Proxy (ExtractNames sig)
extr _ = Proxy

class ExtractDefaults t d | t -> d where
    extractDefaults :: t -> d

instance ExtractDefaults () () where
    extractDefaults = id

instance ExtractDefaults as ds => ExtractDefaults (Arg n d, as) (d, ds) where
    extractDefaults (Arg d, as) = (d, extractDefaults as)

type family ExtractNames t :: [Maybe Symbol] where
    ExtractNames (Arg n d, as) = n ': ExtractNames as
    ExtractNames () = '[]

mkFunc :: ExtractDefaults sig d => sig -> FuncTrans (ExtractNames sig) d f f
mkFunc sig = func (extractDefaults sig)

main = do
    let f = Func (tst, (Unprovided,(Unprovided,(Unprovided,())))) :: Func [Just "x", Just "y", Just "z"] (Unprovided,(Unprovided,(Unprovided,()))) (a->b->c->(a,b,c))
        --g = appByName (Proxy::Proxy "y") [] f
        g = appArg 7 $ appArg 5 $ appByName (Proxy::Proxy "y") [] f

    print $ runFunc2 g

    let 
        f = func (Unprovided,(Unprovided,(Provided 5,()))) :: Num a => FuncTrans [Just "x", Just "y", Just "z"] (Unprovided,(Unprovided,(Provided a,()))) f f
        --f = mkFunc sig -- (Unprovided,(Unprovided,(Provided 5,()))) :: Num a => FuncTrans ["x","y","z"] (Unprovided,(Unprovided,(Provided a,()))) f f
        g  = appArgW [] $ appArgW 5 $ appArgW "a" f
        --g' = appByNameW (Proxy :: Proxy "x") "a" f
        g' = appArgW [] $ appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f
        h  = appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f

    print $ runFunc2 $ runFuncTrans g tst
    print $ runFunc2 $ runFuncTrans g' tst
    print $ runFunc2 $ runFuncTrans h tst



    --print $ fromFunc $ (runFuncTrans f tst)
    --let f = ()
    --let f = appArg [] func
    --let args = Args id
        --a2 = appArg 5 $ appArg 6 $ args
    --let x = app 5
    --print $ appRTuple (1,(2,())) test
    --print $ runArgs a2 test
    return ()


-- @f x=1 y=2 z=3 foo=4