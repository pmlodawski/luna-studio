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

instance ProxyType Cls_Main Cls_Main where
	proxyType = id

instance ProxyType Main Main where
	proxyType = id

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")

-- ------ Main members ------ --

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
memDef_Main_print self s = do 
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Vector type ====== --
data Vector a = Vector a a a deriving (Show, Eq, Ord, Generic, Typeable)
data Vector_Ptr = Vector_Ptr
data Cls_Vector  = Cls_Vector deriving (Show, Eq, Ord, Generic, Typeable)

instance ProxyType Cls_Vector Cls_Vector where
	proxyType = id

instance ProxyType (Vector a) Vector_Ptr where
	proxyType _ = Vector_Ptr

-- ------ Vector.Vector constructor ------ --
cons_Vector = _member("Vector") (val Cls_Vector)
memDef_Cls_Vector_Vector = liftCons3 Vector

-- ====== Method: Cls_Vector.Vector ====== --
memSig_Cls_Vector_Vector = _rtup4(_nuSigArg("self"), _nuSigArg("x"), _nuSigArg("y"), _nuSigArg("z"))
memFnc_Cls_Vector_Vector = (memSig_Cls_Vector_Vector, memDef_Cls_Vector_Vector)
$(registerMethod ''Cls_Vector "Vector")

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector_Ptr [('Vector, [Just "x", Just "y", Just "z"])])
$(registerFieldAccessors ''Vector_Ptr ["x", "y", "z"])

-- ------ Vector members ------ --

-- ====== Method: Vector.asTuple ====== --
memSig_Vector_Ptr_asTuple = _rtup1(_nuSigArg("self"))
memDef_Vector_Ptr_asTuple _self = do 
     val (_call(0) (_member("x") _self), _call(1) (_member("y") _self), _call(2) (_member("z") _self))
     

memFnc_Vector_Ptr_asTuple = (memSig_Vector_Ptr_asTuple, memDef_Vector_Ptr_asTuple)
$(registerMethod ''Vector_Ptr "asTuple")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))
memDef_Main_main _self = do 
     _v <- _call(3) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appByName _name("y") (val (1 :: Int)) cons_Vector)))
     _a <- _call(4) (_member("asTuple") _v)
     _call(5) (appNext _a (_member("print") _self))
     val ()
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

