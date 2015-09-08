-------- HSC --------
-- extensions --
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# LANGUAGE DysfunctionalDependencies #-}

-- module --
module Main where

-- imports --
--import Luna.Target.HS

---- body --
-- #include "pragmas.h"

---- ====== Main type ====== --
--data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
--data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)

---- ------ Main.Main constructor ------ --
--cons_Main = member("Main") (val Cls_Main)
--memDef_Cls_Main_Main = liftCons0 Main

--type instance SigOf Cls_Main "Main" = '[Named "self"]
-- $(registerMethod ''Cls_Main "Main")

---- ------ Main methods ------ --

---- ====== Method: Main.print ====== --

--memDef_Main_print (noDef -> self) (appDef (val "") -> s) =
--    polyJoin . liftF1 (Value . fmap Safe . print) $ s

--type instance SigOf Main "print" = '[ Named "self", Named "y" ]
-- $(registerMethod ''Main "print")


--memDef_Main_tst self = do
--    call $ appNext (val "!!!") $ member("print") $ (call cons_Main)

--main = fromValue $ memDef_Main_tst (call cons_Main)




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
