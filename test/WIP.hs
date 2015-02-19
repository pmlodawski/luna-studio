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
{-# LANGUAGE FunctionalDependencies #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --
#include "pragmas.cpp"

-- ====== Main type ====== --
data Main 
    = Main
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Main)

-- ------ Main.Main constructor ------ --
cons_Main = _member ("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== --------------- ====== --

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1 (_nuSigArg ("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")


-- ===================================================================
-- Data headers
-- ===================================================================

class DataTuple d t | d -> t where
	dataTuple :: d -> t

-- ====== Vector type ====== --
data Vector 
    = Vector Int Int Int
    | Scalar Int
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Vector)

instance DataTuple Vector (Int, Int, Int) where
	dataTuple = dataTuple_Vector

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector [('Vector, [Just "x", Just "y", Just "z"])])
$(registerFieldAccessors ''Vector ["x", "y", "z"])

-- ------ Vector.Vector constructor ------ --
cons_Vector = _member ("Vector") (val Cls_Vector)
memDef_Cls_Vector_Vector = liftCons3 Vector

-- ====== --------------- ====== --

-- ====== Method: Cls_Vector.Vector ====== --
memSig_Cls_Vector_Vector = _rtup4 (_nuSigArg ("self"), _nuSigArg ("x"), _nuSigArg ("y"), _nuSigArg ("z"))
memFnc_Cls_Vector_Vector = (memSig_Cls_Vector_Vector, memDef_Cls_Vector_Vector)
$(registerMethod ''Cls_Vector "Vector")


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2 (_nuSigArg ("self"), _npSigArg ("s", val ("" :: String)))
memDef_Main_print self s = 
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = _call (0) (appNext (val (1 :: Int)) (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

