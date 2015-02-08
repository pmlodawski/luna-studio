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
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --
#include "pragmas.cpp"

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
memDef_Main_print self s = do 
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Vector type ====== --
data Vector a = Vector a a a deriving (Show, Eq, Ord, Generic, Typeable)
data Cls_Vector  = Cls_Vector deriving (Show, Eq, Ord, Generic, Typeable)

-- ------ Vector.Vector constructor ------ --
cons_Vector = _member("Vector") (val Cls_Vector)
memDef_Cls_Vector_Vector = liftCons3 Vector

-- ====== Method: Cls_Vector.Vector ====== --
memSig_Cls_Vector_Vector = _rtup4(_nuSigArg("self"), _nuSigArg("x"), _nuSigArg("y"), _nuSigArg("z"))
memFnc_Cls_Vector_Vector = (memSig_Cls_Vector_Vector, memDef_Cls_Vector_Vector)
$(registerMethod ''Cls_Vector "Vector")

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector [('Vector, [Just "x", Just "y", Just "z"])])
$(registerFieldAccessors ''Vector ["x", "y", "z"])

-- ------ Vector methods ------ --

-- ====== Method: Main.test ====== --
memSig_Main_test = _rtup2(_nuSigArg("self"), _nuSigArg("x"))
memDef_Main_test _self _x = do 
     _x
     

memFnc_Main_test = (memSig_Main_test, memDef_Main_test)
$(registerMethod ''Main "test")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup2(_nuSigArg("self"), _nuSigArg("f"))
memDef_Main_foo _self _f = do 
     _call(0) (appNext (val (5 :: Int)) _f)
     

memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main.bar ====== --
memSig_Main_bar = _rtup1(_nuSigArg("self"))
memDef_Main_bar _self = do 
     val ()
     

memFnc_Main_bar = (memSig_Main_bar, memDef_Main_bar)
$(registerMethod ''Main "bar")

-- ====== Method: Main.baz ====== --
memSig_Main_baz = _rtup3(_nuSigArg("self"), _npSigArg("x", val (0 :: Int)), _npSigArg("y", val (0 :: Int)))
memDef_Main_baz _self _x _y = do 
     val (_x, _y)
     

memFnc_Main_baz = (memSig_Main_baz, memDef_Main_baz)
$(registerMethod ''Main "baz")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))

-- ====== Method: Main.lambda__50 ====== --
memSig_Main_lambda__50 = _rtup2(_nuSigArg("self"), _nuSigArg("x"))
memDef_Main_lambda__50 _self _x = do 
     val (_x, _x)
     

memFnc_Main_lambda__50 = (memSig_Main_lambda__50, memDef_Main_lambda__50)
$(registerMethod ''Main "lambda__50")
memDef_Main_main _self = do 
     _v <- _call(1) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appByName _name("y") (val (1 :: Int)) cons_Vector)))
     _f <- _member("lambda__50") (_call(3) cons_Main)
     _call(4) (appNext (_call(5) (appNext _f (_member("foo") _self))) (_member("print") _self))
     _call(6) (appNext _v (_member("print") _self))
     _call(7) (appNext (_call(8) (appByName _name("y") (val (7 :: Int)) (_member("baz") _self))) (_member("print") _self))
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

