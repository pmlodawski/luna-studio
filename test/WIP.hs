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
import duasfo[sd kgisdj fiosdjfio ]
import Luna.Target.HS

-- body --
#include "pragmas.h"

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
$(registerType ''Main)

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")


-- ===================================================================
-- Data headers
-- ===================================================================

-- ====== Int type ====== --
-- datatype provided externally
$(registerType ''Int)

-- ====== Bool type ====== --
-- datatype provided externally
$(registerType ''Bool)

-- ------ Bool.True constructor ------ --
cons_True = _member("True") (val Cls_Bool)
memDef_Cls_Bool_True = liftCons0 True

-- ====== Method: Cls_Bool.True ====== --
memSig_Cls_Bool_True = _rtup1(_nuSigArg("self"))
memFnc_Cls_Bool_True = (memSig_Cls_Bool_True, memDef_Cls_Bool_True)
$(registerMethod ''Cls_Bool "True")

-- ------ Bool.False constructor ------ --
cons_False = _member("False") (val Cls_Bool)
memDef_Cls_Bool_False = liftCons0 False

-- ====== Method: Cls_Bool.False ====== --
memSig_Cls_Bool_False = _rtup1(_nuSigArg("self"))
memFnc_Cls_Bool_False = (memSig_Cls_Bool_False, memDef_Cls_Bool_False)
$(registerMethod ''Cls_Bool "False")

-- ====== Vector type ====== --
data Vector a = Vector a a a deriving (Show, Eq, Ord, Generic, Typeable)
$(registerType ''Vector)

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector [('Vector, [Just "x", Just "y", Just "z"])])
$(registerFieldAccessors ''Vector ["x", "y", "z"])

-- ------ Vector.Vector constructor ------ --
cons_Vector = _member("Vector") (val Cls_Vector)
memDef_Cls_Vector_Vector = liftCons3 Vector

-- ====== Method: Cls_Vector.Vector ====== --
memSig_Cls_Vector_Vector = _rtup4(_nuSigArg("self"), _nuSigArg("x"), _nuSigArg("y"), _nuSigArg("z"))
memFnc_Cls_Vector_Vector = (memSig_Cls_Vector_Vector, memDef_Cls_Vector_Vector)
$(registerMethod ''Cls_Vector "Vector")


-- ===================================================================
-- Type Aliases
-- ===================================================================
type V a = Vector a


-- ===================================================================
-- Data declarations
-- ===================================================================

-- ====== Method: Vector.asTuple ====== --
memSig_Vector_asTuple = _rtup1(_nuSigArg("self"))
memDef_Vector_asTuple _self = do 
     val (_call(0) (_member("x") _self), _call(1) (_member("y") _self), _call(2) (_member("z") _self))
     

memFnc_Vector_asTuple = (memSig_Vector_asTuple, memDef_Vector_asTuple)
$(registerMethod ''Vector "asTuple")


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
memDef_Main_print self s = do 
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Int._plus ====== --
memSig_Int__plus = _rtup2(_nuSigArg("self"), _nuSigArg("a"))
memDef_Int__plus self a = do 
     
    liftF2 (+) self a
     

memFnc_Int__plus = (memSig_Int__plus, memDef_Int__plus)
$(registerMethod ''Int "_plus")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup3(_nuSigArg("self"), _nuSigArg("a"), _nuSigArg("b"))
memDef_Main_foo _self _a _b = do 
     val (_a, _b)
     

memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))
memDef_Main_main _self = do 
     _v <- _typed(_call(3) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appByName _name("y") (val (1 :: Int)) cons_Vector))), V Int)
     _call(4) (appNext (_call(5) (_member("asTuple") _v)) (_member("print") _self))
     _f <- appNext (val (1 :: Int)) (_member("foo") _self)
     _call(6) (appNext (_call(7) (appNext (val (2 :: Int)) _f)) (_member("print") _self))
     _call(8) (appNext (val (_call(9) cons_True, _call(10) cons_True)) (_member("print") _self))
     _call(11) (appNext (_call(12) (appNext (val (4 :: Int)) (_member("_plus") (val (2 :: Int))))) (_member("print") _self))
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

