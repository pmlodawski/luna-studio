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

class AppRTuple t f g | t f -> g where
    appRTuple :: t -> f -> g

instance AppRTuple () f f where
    appRTuple _ = id

instance AppRTuple as f (a -> g) => AppRTuple (a,as) f g where
    appRTuple (a,as) f = (appRTuple as f) a

test a b = (a,b)

tst2 = app []

--newtype Func kind f a = Func (f -> a)
--data F0
--data F1
--data F2
--data F3


newtype Func      (names :: [Symbol]) f   = Func      { runFunc :: f } deriving (Functor)
newtype FuncTrans (names :: [Symbol]) f a = FuncTrans { runFuncTrans :: f -> Func names a }

remapFunc :: Func n f -> Func n' f
remapFunc = Func . runFunc

remapFuncTrans :: FuncTrans n f a -> FuncTrans n' f a
remapFuncTrans = FuncTrans . fmap remapFunc . runFuncTrans

func :: FuncTrans n f f
func = FuncTrans Func

--func = FuncTrans Func
--func :: Func k (a -> b) (a -> b)
--func = Func ($)
-- finalnie Func kind names f a !
data K (n :: Nat) = K deriving (Show)

type family Inc a where
    Inc (K n) = K (n+1)


--class NamedApp n ns ns' a f g | n ns -> ns' where
--    appNamedArg :: Proxy n -> a -> Func ns f -> Func ns' g

--data Just a  deriving (Typeable)
--data Nothing deriving (Typeable)





----------------------------------------------------------------------
-- ElemIndex
----------------------------------------------------------------------

type family ElemIndex (a :: k) (as :: [k]) where
    ElemIndex a (a ': as) = 0
    ElemIndex a (b ': as) = ElemIndex a as + 1

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

appByName :: forall idx a f g n ns. (AppNth idx a f g, idx~ElemIndex n ns)
          => Proxy n -> a -> Func ns f -> Func (Delete n ns) g
appByName _ a f = remapFunc $ fmap (appNth (Proxy :: Proxy idx) a) f

appByNameW :: AppNth (ElemIndex n ns) a g g' 
           => Proxy n -> a -> FuncTrans ns f g -> FuncTrans (Delete n ns) f g'
appByNameW n a (FuncTrans f) = FuncTrans $ fmap (appByName n a) f

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

data    Unprovided   = Unprovided  deriving (Show, Eq, Typeable)
newtype Provided   a = Provided a  deriving (Show, Eq, Typeable, Functor)

newtype Arg (name :: Maybe Symbol) a = Arg a

defArg = Arg . Provided
arg    = Arg Unprovided



appArg :: a -> Func ns (a -> b) -> Func ns b
appArg a (Func f) = Func $ f a

appArgW :: a -> FuncTrans (n ': ns) f (a -> b) -> FuncTrans ns f b
appArgW a (FuncTrans f) = FuncTrans $ (\f' -> remapFunc . appArg a $ f f')

tst a b c = (a,b,c)

tst' f = appNth (Proxy :: Proxy 1) [] f



type family SigNames s where
    SigNames () = '[]
    SigNames (Arg n p, as) = n ': SigNames as

--mkFunc :: sig -> FuncTrans (SigNames sig) f f
--mkFunc = const func

sig = (arg::Arg (Just "x") Unprovided,(arg::Arg (Just "y") Unprovided,(arg::Arg (Just "z") Unprovided,())))

main = do
    --print $ tst' tst 5 6

    let f = Func tst :: Func ["x","y","z"] (a->b->c->(a,b,c))
        g = appArg 5 $ appByName (Proxy::Proxy "y") [] f

    print $ runFunc g 7

    --let f = mkFunc sig
    --    g  = appArgW [] $ appArgW 5 $ appArgW "a" f
    --    g' = appArgW [] $ appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f

    --print $ runFunc $ runFuncTrans g tst
    --print $ runFunc $ runFuncTrans g' tst



    --print $ runFunc $ (runFuncTrans f tst)
    --let f = ()
    --let f = appArg [] func
    --let args = Args id
        --a2 = appArg 5 $ appArg 6 $ args
    --let x = app 5
    --print $ appRTuple (1,(2,())) test
    --print $ runArgs a2 test
    return ()


-- @f x=1 y=2 z=3 foo=4