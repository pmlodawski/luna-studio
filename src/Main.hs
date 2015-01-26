-------- HSC --------
-- extensions --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}

{-# LANGUAGE DysfunctionalDependencies #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS
import qualified Luna.Target.HS.Data.Func.Args2 as Args2
import qualified Luna.Target.HS.Data.Func.Args3 as Args3
import qualified Luna.Target.HS.Data.Func.Args7 as Args7
import qualified Prelude as P
import qualified Type.BaseType as BaseType
import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)

-- body --
#include "pragmas.cpp"

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)

-- ------ Main.Main constructor ------ --
cons_Main = member("Main") (val Cls_Main)
memSig_Cls_Main_Main = rtup1(nparam("self"))
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memDef_Main_print rtup2(self, s) = do 
    polyJoin . liftF1 (Value . fmap Safe . print) $ s

--memDef_Main_print2 self s = s
memDef_Main_print2 self s = polyJoin . liftF1 (Value . fmap Safe . print) $ s

memDef_Main_print3 (Args7.noDefault -> self) (Args7.appDefault2 (val "") -> s) = polyJoin . liftF1 (Value . fmap Safe . print) $ s


memSig_Main_print = rtup2(nparam("self"), param)
memSig_Main_print2 = rtup2(Args2.arg (Proxy :: Proxy (Just "self")), Args2.uarg)

type instance Args7.SigOf Main "print3" = '[Args7.Named "self", Args7.Named "y"]
$(registerMethod3 ''Main "print3")


$(registerMethod ''Main "print")
$(registerMethod2 ''Main "print2")

--ftst a v = call3 $ appNextW (val v) $ member2 (Proxy::Proxy "print2") $ a

--main = fromValue $ tst (call cons_Main)

memSig_Main_tst = rtup2(Args2.arg (Proxy :: Proxy (Just "self")), Args2.uarg)

--x = addArg (Args6.uArg "a") $ member3 (Proxy::Proxy "print2") $ call cons_Main;
args v = Args7.addArg (Args7.uArg (val "self"))
     $ Args7.addArg (Args7.nArg v (Proxy::Proxy "self"))
     $ Args7.empty

memDef_Main_tst self = do
    call3 $ appNextW (val 5) $ member2 (Proxy::Proxy "print2") $ (call cons_Main)
    call3 $ appNextW (val "a") $ member2 (Proxy::Proxy "print2") $ (call cons_Main)

    Args7.runFunc $ Args7.appArgs (args (val "aaa")) $ Args7.mkFunc Main (Proxy::Proxy "print3") memDef_Main_print3
    --Args7.runFunc $ Args7.appArgs args $ Args7.mkFunc Main (Proxy::Proxy "print3") 
    --              $ flip getMember (monoType args) 
    --              $ (fst . fromAppH . fromSafe . fromPure . fromValue $ getMem4 (Proxy::Proxy "print3") (val Main))

    call5 $ addArg (Args7.uArg $ val "!!!") $ member3 (Proxy::Proxy "print3") $ (call cons_Main)

main = fromValue $ memDef_Main_tst (call cons_Main)




-- $(registerMethod2 ''Main "tst")

--(Call2
--                        (AppH
--                           (Mem Main "print2")
--                           (Args2.FuncTrans '[] () (Value Pure Safe Main -> [t0] -> b0) b0))
--                        (m20 a))

--instance (func~(a->b->b)) => FuncProvider Main "print2" func where
--  getFunc2 _ _ = memDef_Main_print2


--class FuncProvider (base :: k) name func where
--    getFunc2 :: Mem base name -> (func -> out) -> func

-- 
-- instance HasMem2 "print" Main (Args2.Arg ('Just "self") Args2.Unprovided,(Args2.Arg 'Nothing Args2.Unprovided, ())) where
--     memSig2 _ = memSig_Main_print2
-- 
-- -- ====== Method: Int._plus ====== --
-- memDef_Int__plus rtup2(self, a) = do 
--     liftF2 (+) self a
--      
-- 
-- memSig_Int__plus = rtup2(nparam("self"), param)
-- $(registerMethod ''Int "_plus")
-- 
-- -- ====== Method: Main.foo ====== --
-- memDef_Main_foo rtup2(_self, (extractTuple2 -> (_a, _b))) = do 
--      call (appNext (val (1 :: Int)) (appNext _a (member("_plus") _self)))
--      
-- 
-- memSig_Main_foo = rtup2(nparam("self"), param)
-- $(registerMethod ''Main "foo")
-- 
-- -- ====== Method: Main._plus ====== --
-- memDef_Main__plus rtup3(_self, _a, _b) = do 
--      call (appNext _b (member("_plus") _a))
--      
-- 
-- memSig_Main__plus = rtup3(nparam("self"), param, param)
-- $(registerMethod ''Main "_plus")
-- 
-- -- ====== Method: Main.main ====== --
-- memDef_Main_main rtup1(_self) = do 
--     --Args2.runFunc $ Args2.runFuncTrans (Args2.appArgW [] $ Args2.mkFunc memSig_Main_print2) memDef_Main_print2
--      --x <- appNextW [] $ member2 (Proxy::Proxy "print") _self;
--      --Args2.runFunc $ Args2.runFuncTrans (Args2.appArgW (val []) $ Args2.appArgW (val "self") $ Args2.mkFunc memSig_Main_print2) memDef_Main_print2
--      --Args2.runFuncTrans (Args2.appArgW (val []) $ Args2.appArgW (val "self") $ Args2.mkFunc memSig_Main_print2) memDef_Main_print2
--      --Args2.runFuncTrans (Args2.appArgW (val []) $ Args2.appArgW (val "self") $ Args2.mkFunc memSig_Main_print2) memDef_Main_print2
--      call (appNext typed(val [], [Int]) (member("print") _self))
--      call (appNext (call (appNext (val (val (5 :: Int), val ("b" :: String))) (member("foo") _self))) (member("print") _self))
--      
-- 
-- memSig_Main_main = rtup1(nparam("self"))
-- $(registerMethod ''Main "main")
-- 
-- 
-- -- ===================================================================
-- -- Main module wrappers
-- -- ===================================================================
-- main = mainMaker cons_Main
