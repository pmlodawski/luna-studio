-------- HSC --------
-- extensions --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import qualified Prelude as P
import qualified Type.BaseType as BaseType

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


memSig_Main_print = rtup2(nparam("self"), param)
memSig_Main_print2 = rtup2(Args2.arg (Proxy :: Proxy (Just "self")), Args2.uarg)
$(registerMethod ''Main "print")
$(registerMethod2 ''Main "print2")

ftst a v = call3 $ appNextW (val v) $ member2 (Proxy::Proxy "print2") $ a

--main = fromValue $ tst (call cons_Main)

memSig_Main_tst = rtup2(Args2.arg (Proxy :: Proxy (Just "self")), Args2.uarg)

--memDef_Main_tst
--  :: (FuncProvider obj "print2" f, HasMem2 "print2" obj sig,
--      Env base, Args2.AppDefaults t1 f1 (m2 a),
--      Args2.AppNth
--        (Args2.NameIndex "self" (Args2.ExtractNames sig))
--        (m base s a1)
--        f
--        (Value Pure Safe [Char] -> f1),
--      Args2.DeleteNth
--        (Args2.NameIndex "self" (Args2.ExtractNames sig)) d1 (d, t1),
--      Args2.ExtractDefaults sig d1, Safety s,
--      PolyMonad (Value Pure Safe) m2 m3,
--      BaseType.BaseType (Proxy a1) (Proxy obj),
--      Args2.Delete ('Just "self") (Args2.ExtractNames sig) ~ (n : t)) =>
--     m base s a1 -> m3 a
memDef_Main_tst self = do
    call3 $ appNextW (val []) $ member2 (Proxy::Proxy "print2") $ self
    call3 $ appNextW (val "a") $ member2 (Proxy::Proxy "print2") $ self

main = memDef_Main_tst
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
