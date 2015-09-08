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

module Luna.Target.HS.Data.Func.Args2 where

--{-# LANGUAGE DysfunctionalDependencies #-}

import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

import Data.PolyTypeable




class AppDefaults t f g  | t f -> g where
    appDefaults :: t -> f -> g

instance (f~g) => AppDefaults () f g where
    appDefaults _ = id

instance (f~(a->b), AppDefaults as b g, pa~Provided a) => AppDefaults (pa,as) f g where
    appDefaults (a,as) f = appDefaults as $ f (fromProvided a)



newtype Func      (names :: [Maybe Symbol]) defs f   = Func      { fromFunc      :: (f,defs) } deriving (Functor, Typeable)
newtype FuncTrans (names :: [Maybe Symbol]) defs f a = FuncTrans { fromFuncTrans :: f -> Func names defs a } deriving (Typeable)

instance Show (FuncTrans names defs f a) where
    show _ = "FuncTrans"

remapFunc :: Func n d f -> Func n' d f
remapFunc = Func . fromFunc

runFunc (Func (f, defs)) = appDefaults defs f


remapFuncTrans :: FuncTrans n d f a -> FuncTrans n' d f a
remapFuncTrans = FuncTrans . fmap remapFunc . fromFuncTrans

func :: d -> FuncTrans n d f f
func d = FuncTrans (\f -> Func (f,d))


----------------------------------------------------------------------
-- NameIndex
----------------------------------------------------------------------

type family NameIndex (a :: Symbol) (as :: [Maybe Symbol]) :: Nat where
    NameIndex a (Just a ': as) = 0
    NameIndex a (b ': as)      = NameIndex a as + 1

type family Delete (a :: k) (as :: [k]) where
    Delete a (a ': as) = as
    Delete a (b ': as) = (b ': Delete a as)

class Delete2 (a::Symbol) (as :: [Symbol]) (r :: [Symbol]) | a as -> r

instance Delete2 a (a ': as) as
instance Delete2 a as bs => Delete2 a (b ': as) (b ': bs)

class Delete3 a as r | a as -> r
instance Delete3 a (a, as) as
instance Delete3 a as bs => Delete3 a (b, as) (b, bs)

--uxx :: Delete2 ("x") '["x", "y", "z"] ns => Proxy ns
--uxx = undefined

--uxx3 :: Delete3 (Proxy "x") (Proxy "x",(Proxy "y",(Proxy "z",()))) ns => ns
--uxx3 = undefined

----------------------------------------------------------------------
-- AppNth
----------------------------------------------------------------------

class AppNth (k::Nat) a f g | k a f -> g where
    appNth :: Proxy k -> a -> f -> g

instance (f~(a -> b)) => AppNth 0 a f b where
    appNth _ a = ($a)

instance (CmpNat n 0 ~ ok, AppNth' ok n a f g)
      => AppNth n a f g where
    appNth = appNth' (Proxy :: Proxy ok)

class AppNth' (ok :: Ordering) (k::Nat) a f g | k a f -> g where
    appNth' :: Proxy ok -> Proxy k -> a -> f -> g

instance (f~(t -> s), g~(t -> u), AppNth (n-1) a s u) 
      => AppNth' GT n a f g where
    appNth' _ _ a = (\f t -> appNth (Proxy :: Proxy (n-1)) a (f t))

--class AppFoo a b | a -> b
class AppNth2 (k::Nat) a f g | k a f -> g where
    appNth2 :: Proxy k -> a -> f -> g

instance (f~(a -> b)) => AppNth2 0 a f b where
    appNth2 _ a = ($a)

tstme a = appNth (Proxy :: Proxy 5) []

---

class DeleteNth (num::Nat) t t' | num t -> t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNth 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNth (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNth num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)

--appByName :: forall idx a f g n ns d. (AppNth idx a f g, idx~NameIndex n ns)
--          => Proxy n -> a -> Func ns d f -> Func (Delete n ns) d g
appByName :: (AppNth (NameIndex n ns) a f f1, DeleteNth (NameIndex n ns) d defs)
          => Proxy n -> a -> Func ns d f -> Func names defs f1
appByName (_::Proxy n) a (Func (f,d) :: Func ns d f) = Func $ ( appNth idx a f, deleteNth idx d) where
    idx = (Proxy :: Proxy (NameIndex n ns))

--appByNameW :: AppNth (NameIndex n ns) a g g' 
--           => Proxy n -> a -> FuncTrans ns d f g -> FuncTrans (Delete n ns) d f g'
--appByNameW :: Proxy n0 -> a0 -> FuncTrans ns0 d0 f0 f1 -> FuncTrans names0 defs0 f0 f2
appByNameW :: (AppNth (NameIndex n ns) a f1 f2, DeleteNth (NameIndex n ns) d defs) 
           => Proxy n -> a -> FuncTrans ns d f f1 -> FuncTrans (Delete (Just n) ns) defs f f2
appByNameW n a (FuncTrans f) = FuncTrans $ fmap (appByName n a) f

--tstme2 v = appByName (Proxy::Proxy "x") [] v

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

data    Unprovided   = Unprovided  deriving (Show, Eq, Typeable)
newtype Provided   a = Provided { fromProvided :: a }  deriving (Show, Eq, Typeable, Functor)

newtype Arg (name :: Maybe Symbol) a = Arg a

defArg :: a -> Proxy n -> Arg n (Provided a) 
defArg a _ = Arg $ Provided a



arg :: Proxy (n :: Maybe Symbol) -> Arg n Unprovided
arg _ = Arg Unprovided

uarg :: Arg Nothing Unprovided
uarg = Arg Unprovided :: Arg Nothing Unprovided

--fdc1 = (arg (Proxy :: (Proxy (Nothing :: Maybe Symbol))))

--fdc2 = (Arg Unprovided :: Arg Nothing Unprovided)


appArg :: a -> Func ns (d,ds) (a -> b) -> Func ns ds b
appArg a (Func (f,(d,ds))) = Func (f a,ds)

appArgW :: a -> FuncTrans (n ': ns) (d,ds) f (a -> b) -> FuncTrans ns ds f b
appArgW a (FuncTrans f) = FuncTrans $ (\f' -> remapFunc . appArg a $ f f')



type family SigNames s where
    SigNames () = '[]
    SigNames (Arg n p, as) = n ': SigNames as



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

infixr 5 //
(//) = (,)

sig =  arg (Proxy :: Proxy (Just "x")) 
    // arg (Proxy :: Proxy (Just "x")) 
    // defArg 5 (Proxy :: Proxy (Just "x"))
    // ()

tst a b c = (a,b,c)



foox a = appArgW 5 $ appArgW [] $ appByNameW (Proxy :: Proxy "x") "a" $ a
foox2 a = appByNameW (Proxy :: Proxy "x") "a" $ a


main = do
    let 
        f = mkFunc sig
        g  = appArgW [] $ appArgW 5 $ appArgW "a" f
        g' = appArgW [] $ appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f
        h  = appArgW 5 $ appByNameW (Proxy :: Proxy "x") "a" f



    print $ runFuncTrans g tst
    print $ runFuncTrans g' tst
    print $ runFuncTrans h tst

    --print $ fromFunc $ (fromFuncTrans f tst)
    --let f = ()
    --let f = appArg [] func
    --let args = Args id
        --a2 = appArg 5 $ appArg 6 $ args
    --let x = app 5
    --print $ appRTuple (1,(2,())) test
    --print $ runArgs a2 test
    return ()

    --print $ polyTypeOf 

runFuncTrans t f = runFunc $ fromFuncTrans t f



-- @f x=1 y=2 z=3 foo=4