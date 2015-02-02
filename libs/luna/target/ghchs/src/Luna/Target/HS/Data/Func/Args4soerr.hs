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
{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE PolyKinds #-}


{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Data.Func.Args4soerr where


--import Prelude hiding (reverse)
--import Unsafe.Coerce
--import GHC.TypeLits
--import Data.Typeable

--import Data.PolyTypeable

--import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)


--import Unsafe.Coerce (unsafeCoerce)




--class MemberProvider cls (name :: Symbol) argRep f | cls name argRep -> f where
--    getMember :: cls -> Proxy name -> argRep -> f


--data V = V deriving (Show, Typeable)

--instance Analyze a b => MemberProvider V "foo" args (a -> b) where
--    getMember _ _ _ = (\a -> monoType a)

--instance MemberProvider V "bar" args (a -> [TVar]) where
--    getMember _ _ _ _ = monoType []

--newtype Arg (name::Maybe Symbol) a = Arg a deriving (Show)


--uArg :: a -> Arg Nothing a
--uArg = Arg

--nArg :: a -> Proxy n -> Arg (Just n) a
--nArg a _ = Arg a


--empty = ()
--appArg = (,)

----class AppArg a t t' | a t -> t' where
----    appArg :: a -> t -> t'

----instance AppArg a () (a,()) where
----    appArg a = (a,)

----instance AppArg a xs ys => AppArg a (x,xs) (x,ys) where
----    appArg a = fmap (appArg a)

------------------------------------------------------------------------
---- Signature args
------------------------------------------------------------------------

--newtype Provided a = Provided { fromProvided :: a }
--data Unprovided = Unprovided

--newtype SigArg (name :: Maybe Symbol) def = SigArg def

--sArg :: SigArg Nothing Unprovided
--sArg = SigArg Unprovided

--sArgP :: a -> SigArg Nothing (Provided a)
--sArgP = SigArg . Provided

--sArgN :: Proxy n -> SigArg (Just n) Unprovided
--sArgN _ = SigArg Unprovided

--sArgNP :: a -> Proxy n -> SigArg (Just n) (Provided a)
--sArgNP a _ = SigArg $ Provided a



------------------------------------------------------------------------
---- Func
------------------------------------------------------------------------

--newtype Func sig f = Func { fromFunc :: (sig,f) }

--func sig f = Func (sig, f)

--class AppArgs2 args f f' | args f -> f' where
--    appArgs2 :: args -> f -> f'

--instance AppArgs2 () f f where
--    appArgs2 _ = id

----instance (f~Func (s,ss) (a->b), AppArgs2 as (Func ss b) f') => AppArgs2 (Arg 'Nothing a,as) f f' where
----    appArgs2 (Arg a,as) (Func ((_,ss),f)) = appArgs2 as $ Func (ss, f a)

--instance ( AppArgs2 as (Func sig1 f) f', AppNth (ArgIndex n sig) a f1 f, DeleteNthC (ArgIndex n sig) sig sig1, f~Func sig f1)
--      => AppArgs2 (Arg ('Just n) a,as) f f' where
--    appArgs2 = appArgs2xx


--type family ArgIndex (n :: Symbol) args where
--    ArgIndex n (SigArg (Just n) def, as) = 0
--    ArgIndex n (a, as)                   = 1 + ArgIndex n as

------------------------------------------------------------------------
----- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
------------------------------------------------------------------------

----sig = (nSArg )

--class Reverse t r t' | t r -> t' where
--    rev :: t -> r -> t'

--instance Reverse () a a where
--    rev _ = id

--instance Reverse xs (x,a) b => Reverse (x,xs) a b where
--    rev (x,xs) a = rev xs (x,a)

--reverse a = rev a ()

--member cls name args = getMember cls name (monoType args)
--call cls name args = extractType $ member cls name args args

--tst q = (call q (Proxy :: Proxy "foo") ([],()), call q (Proxy :: Proxy "foo") ('a',()))
--tst2 q = (call q (Proxy :: Proxy "bar") () ++ ['a'], call q (Proxy :: Proxy "bar") () ++ [4])

--newtype FuncSig (names :: [Maybe Symbol]) defs = FuncSig defs

----shiftSig :: FuncSig (n ': ns) (d,ds) -> FuncSig ns ds
----shiftSig (FuncSig d) = FuncSig $ snd d

----shiftNSig :: (idx~NameIndex (Just n) sig, sig'~DeleteNth idx sig, DeleteNthC idx def def')
----          => Proxy (n :: Symbol) -> FuncSig sig def -> FuncSig sig' def'
----shiftNSig (_ :: Proxy n) (FuncSig d :: FuncSig sig def) = FuncSig $ deleteNth (Proxy::Proxy (NameIndex (Just n) sig)) d



--funcSig :: Proxy names -> defs -> FuncSig names defs
--funcSig _ = FuncSig

----appArg 

--tstX q v = call q (Proxy :: Proxy "foo") ([],v)

--test a b = (a,b)

--sig = sArgN (Proxy::Proxy "x") // sArgN (Proxy::Proxy "y") // ()

--main = do
--    let 
--        --args = appArg (uArg 5) $ appArg (uArg []) empty
--        --args = appArg (uArg 5) $ appArg (uArg []) empty
--        args = appArg (uArg 5) $ appArg (nArg [] (Proxy :: Proxy "x")) empty
--        args2 = appArg (nArg [] (Proxy :: Proxy "x")) empty

--    --let args = appArg (uArg []) $ appArg (uArg 5 (Proxy::Proxy "y")) empty
--        fs   = funcSig (Proxy::Proxy '[Just "x", Just "y"]) ()

--    print $ member V (Proxy :: Proxy "foo") () (5::Int)
--    print $ member V (Proxy :: Proxy "foo") () "a"

--    print $ tst V
--    print $ tst2 V
--    --print $ typeOf $ analyze args

--    --print $ appArgsR args (funcApp (Proxy::Proxy '[Just "x", Just "y"]) test)
--    let yy = appArgs2 args2 (Func (sig, test))
--    let xx = appArgs2xx (nArg [] (Proxy :: Proxy "y"),()) (func sig test)
--    return ()

--appArgs2xx (Arg a :: Arg (Just n) a,as) (Func (sig::sig, f)) = appArgs2 as $ Func (deleteNth idx sig, appNth idx a f) where
--    idx = Proxy :: Proxy (ArgIndex n sig)

----callFunc (Func (f, sig :: FuncSig names defs)) args = appArgsR args (funcApp (Proxy::Proxy names) f)

--fooa x = appArg (uArg 5) $ appArg (uArg []) x

--class ExtractType a b where
--    extractType :: a -> b

--instance ExtractType TVar a where
--    extractType = unsafeCoerce

--instance (a~b) => ExtractType a b where
--    extractType = id

--instance (ExtractType t1 u1, r~c u1) => ExtractType (c t1) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, r~c u1 u2) => ExtractType (c t1 t2) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, r~c u1 u2 u3) => ExtractType (c t1 t2 t3) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, ExtractType t4 u4, r~c u1 u2 u3 u4) => ExtractType (c t1 t2 t3 t4) r where
--    extractType = unsafeCoerce

--instance (ExtractType t1 u1, ExtractType t2 u2, ExtractType t3 u3, ExtractType t4 u4, ExtractType t5 u5, r~c u1 u2 u3 u4 u5) => ExtractType (c t1 t2 t3 t4 t5) r where
--    extractType = unsafeCoerce

----main = do
----    let v = monoType []
----    print $ typeOf $ v
----    print $ extractType v
----    return ()

--funcApp :: Proxy sig -> f -> FuncApp sig f
--funcApp _ = FuncApp

--newtype FuncApp sig f = FuncApp { fromFuncApp :: f } deriving (Functor)

----appFunc :: a -> FuncApp (s ': ss) (a -> b) -> FuncApp ss b
----appFunc a = appFuncWith ($ a)

----appFuncByName :: (sig' ~ Delete n sig, idx~NameIndex (Just n) sig) => Proxy n -> a -> FuncApp sig (a -> b) -> FuncApp sig' b
----appFuncByName a = appFuncWith ($ a)

----appFuncWith :: (f -> g) -> FuncApp (s ': ss) f -> FuncApp ss g
----appFuncWith t (FuncApp f) = FuncApp $ t f

----remapFuncApp :: FuncApp sig f -> FuncApp sig' f
----remapFuncApp = FuncApp . fromFuncApp

----class AppArgs args (sig :: [Maybe Symbol]) f r | args sig f -> r where
----    appArgs :: args -> FuncApp sig f -> r

----instance AppArgs () sig f f where
----    appArgs _ = fromFuncApp

----instance (f~ (a->b), sig~(s ': ss), AppArgs as ss b g) => AppArgs (Arg Nothing a,as) sig f g where
----    appArgs (Arg a,as) f = appArgs as $ appFunc a f

----instance (idx~NameIndex (Just n) sig, sig'~DeleteNth idx sig, AppNth idx a f f', AppArgs as sig' f' g)
----      => AppArgs (Arg (Just n) a,as) sig f g where
----    appArgs (Arg a,as) f = appArgs as $ (remapFuncApp $ fmap (appNth (Proxy::Proxy idx) a) f :: FuncApp sig' f')




----class AppArgs2 args (sig :: [Maybe Symbol]) defs f r | args sig defs f -> r where
----    appArgs2 :: args -> Func sig defs f -> r

----instance AppArgs2 () sig f f where
----    appArgs2 _ = fromFuncApp

-----

--class AppRTuple t f f' | t f -> f' where
--    appRTuple :: t -> f -> f'

--instance AppRTuple () f f where
--    appRTuple _ = id

--instance (f~(a->b), AppRTuple as b f') => AppRTuple (a,as) f f' where
--    appRTuple (a,as) f = appRTuple as $ f a

----class 

----appArgsR = appArgs . reverse

----foo f a = appArgs (uArg [],a) f


------------------------------------------------------------------------
---- NameIndex
------------------------------------------------------------------------

----type family NameIndex (a :: k) (as :: [k]) :: Nat where
----    NameIndex a (a ': as) = 0
----    NameIndex a (b ': as) = NameIndex a as + 1

----type family Delete (a :: k) (as :: [k]) where
----    Delete a (a ': as) = as
----    Delete a (b ': as) = (b ': Delete a as)

----type family DeleteNth (num::Nat) (as :: [k]) :: [k] where
----    DeleteNth 0 (a ': as) = as
----    DeleteNth n (a ': as) = a ': DeleteNth (n-1) as

------------------------------------------------------------------------
---- AppNth
------------------------------------------------------------------------

--class AppNth (k::Nat) a f g | k a f -> g where
--    appNth :: Proxy k -> a -> f -> g

--instance (f~(a -> b)) => AppNth 0 a f b where
--    appNth _ a = ($a)

--instance (CmpNat n 0 ~ ok, AppNth' ok n a f g)
--      => AppNth n a f g where
--    appNth = appNth' (Proxy :: Proxy ok)

--class AppNth' (ok :: Ordering) (k::Nat) a f g | k a f -> g where
--    appNth' :: Proxy ok -> Proxy k -> a -> f -> g

--instance (f~(t -> s), g~(t -> u), AppNth (n-1) a s u) 
--      => AppNth' GT n a f g where
--    appNth' _ _ a = (\f t -> appNth (Proxy :: Proxy (n-1)) a (f t))

-----

--class DeleteNthC (num::Nat) t t' | num t -> t' where
--    deleteNth :: Proxy num -> t -> t'

--instance (t~(a,b)) => DeleteNthC 0 t b where
--    deleteNth _ (_,b) = b

--instance (DeleteNthC (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNthC num a b where
--    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)



--infixr 5 //
--(//) = (,)

