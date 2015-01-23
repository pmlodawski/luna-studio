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


{-# LANGUAGE DysfunctionalDependencies #-}

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


newtype Func args f = Func { fromFunc :: (args, f) } deriving (Functor)
newtype Func' f = Func' { fromFunc' :: f } deriving (Functor)
newtype FuncTrans args f g = FuncTrans { runFuncTrans :: f -> Func args g }

runFunc = snd . fromFunc

--remapFunc :: Func n f -> Func n' f
--remapFunc = Func . fromFunc

--remapFuncTrans :: FuncTrans n f a -> FuncTrans n' f a
--remapFuncTrans = FuncTrans . fmap remapFunc . runFuncTrans

func :: args -> FuncTrans args f f
func sig = FuncTrans (\f -> Func (sig,f))

----func = FuncTrans Func
----func :: Func k (a -> b) (a -> b)
----func = Func ($)
---- finalnie Func kind names f a !
--data K (n :: Nat) = K deriving (Show)

--type family Inc a where
--    Inc (K n) = K (n+1)


----class NamedApp n ns ns' a f g | n ns -> ns' where
----    appNamedArg :: Proxy n -> a -> Func ns f -> Func ns' g

----------------------------------------------------------------------
-- ElemIndex
----------------------------------------------------------------------

type family ElemIndex (a :: k) (as :: [k]) where
    ElemIndex a (a ': as) = 0
    ElemIndex a (b ': as) = ElemIndex a as + 1

type family NamedIndex (a :: Symbol) as where
    NamedIndex n (Arg (Just n) a, as) = 0
    NamedIndex n (a,as)               = (NamedIndex n as) + 1


type family FromJust a where
    FromJust (Just a) = a

type family Delete (a :: k) (as :: [k]) where
    Delete a (a ': as) = as
    Delete a (b ': as) = (b ': Delete a as)


type family DeleteIdx (idx :: Nat) (lst :: [a]) where
    DeleteIdx 0 (a ': as) = as
    DeleteIdx n (a ': as) = (a ': DeleteIdx (n-1) as)

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

class ExtractArg (name::Symbol) args arg | name args -> arg where
    extractArg :: Proxy name -> args -> arg

instance ExtractArg n (Arg (Just n) a, as) (Arg (Just n) a, as) where
    extractArg _ = id

--instance ExtractArg n as (a', as') => ExtractArg n (Arg a f, as) (a', (Arg a f, as')) where
--    extractArg n (a,as) = (a',(a,as')) where
--        (a',as') = extractArg n as

instance ExtractArg n as (a', as') => ExtractArg n (Arg (Just x) f, as) (a', (Arg (Just x) f, as')) where
    extractArg n (a,as) = (a',(a,as')) where
        (a',as') = extractArg n as

--appByName :: forall name a args args1 b t idx n. (ExtractArg name args (t, args1), idx~NamedIndex n args)
--          => Proxy name -> a -> Func args (a -> b) -> Func args1 b

--appByName :: (AppNth (NamedIndex n args) a1 (a -> b) f, ExtractArg n args (t, args1)) 
--          => Proxy n -> a1 -> Func args (a -> b) -> Func args1 f
appByName (n :: Proxy n) v (Func (args,f) :: Func args (a->b)) = 
    Func (as, appNth (Proxy :: Proxy (NamedIndex n args)) v f)
    where (_,as) = extractArg n args

--appByName :: forall idx a f g n ns. (AppNth idx a f g, idx~NamedIndex n ns)
--          => Proxy n -> a -> Func ns f -> Func (DeleteIdx idx ns) g
--appByName _ a f = remapFunc $ fmap (appNth (Proxy :: Proxy idx) a) f

--appByName :: forall idx a f g n ns. (AppNth idx a f g, idx~ElemIndex n ns)
--          => Proxy n -> a -> Func ns f -> Func (Delete n ns) g
--appByName _ a f = remapFunc $ fmap (appNth (Proxy :: Proxy idx) a) f

--appByNameW :: AppNth (ElemIndex n ns) a g g' 
--           => Proxy n -> a -> FuncTrans ns f g -> FuncTrans (Delete n ns) f g'

appByNameW n a (FuncTrans f) = FuncTrans $ fmap (appByName n a) f

------------------------------------------------------------------------
---- Tests
------------------------------------------------------------------------

data    Unprovided   = Unprovided  deriving (Show, Eq, Typeable)
newtype Provided   a = Provided a  deriving (Show, Eq, Typeable, Functor)
newtype Arg (name :: Maybe Symbol) a = Arg a
defArg = Arg . Provided
arg    = Arg Unprovided



--appArg :: v -> Func (a, as) (v -> o) -> Func as o
appArg v (Func ((a,as),f)) = Func (as, f v)
appArg' v (Func' f) = Func' (f v)

appArgW :: v -> FuncTrans (a, as) f (v -> o) -> FuncTrans as f o
appArgW v (FuncTrans f) = FuncTrans $ fmap (appArg v) f

sig = (arg::Arg (Just "x") Unprovided,(arg::Arg (Just "y") Unprovided,(arg::Arg (Just "z") Unprovided,())))

tst a b c = (a,b,c) 

f = func sig
g  = appArgW [] $ appArgW 5 $ appArgW "a" f
--g' :: Num v => FuncTrans () ([Char] -> v -> [t] -> o) o
--g' = appArgW [] $ appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f

main = do
    --print $ tst' tst 5 6

    let f_ = Func (sig, tst)
        g_ = appArg 0 $ appArg 5 $ appArg 7 f_ 
        h_ = appArg (0::Int) $ appArg (5::Int) $ appByName (Proxy :: Proxy "x") 7 f_ 
    --    g = appByName (Proxy::Proxy "y") [] f

    print $ (snd $ fromFunc g_)



    --print $ runFunc $ runFuncTrans g tst
    --print $ runFunc $ runFuncTrans g' tst



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