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
data Main 
    = Main
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Main)

-- ------ Main.Main constructor ------ --
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1 (_nuSigArg ("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")


-- ===================================================================
-- Data headers
-- ===================================================================

-- ====== Bool type ====== --
-- datatype provided externally
$(registerType ''Bool)

-- ------ Bool.True constructor ------ --
memDef_Cls_Bool_True = liftCons0 True

-- ====== Method: Cls_Bool.True ====== --
memSig_Cls_Bool_True = _rtup1 (_nuSigArg ("self"))
memFnc_Cls_Bool_True = (memSig_Cls_Bool_True, memDef_Cls_Bool_True)
$(registerMethod ''Cls_Bool "True")

-- ------ Bool.False constructor ------ --
memDef_Cls_Bool_False = liftCons0 False

-- ====== Method: Cls_Bool.False ====== --
memSig_Cls_Bool_False = _rtup1 (_nuSigArg ("self"))
memFnc_Cls_Bool_False = (memSig_Cls_Bool_False, memDef_Cls_Bool_False)
$(registerMethod ''Cls_Bool "False")

-- ====== Vector type ====== --
data Vector a 
    = Vector a a a
    | Scalar a
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Vector)

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector [('Vector, [Just "x", Just "y", Just "z"]), ('Scalar, [Just "x"])])
$(registerFieldAccessors ''Vector ["x", "y", "z"])

-- ------ Vector.Vector constructor ------ --
memDef_Cls_Vector_Vector = liftCons3 Vector

-- ====== Method: Cls_Vector.Vector ====== --
memSig_Cls_Vector_Vector = _rtup4 (_nuSigArg ("self"), _nuSigArg ("x"), _nuSigArg ("y"), _nuSigArg ("z"))
memFnc_Cls_Vector_Vector = (memSig_Cls_Vector_Vector, memDef_Cls_Vector_Vector)
$(registerMethod ''Cls_Vector "Vector")

-- ------ Vector.Scalar constructor ------ --
memDef_Cls_Vector_Scalar = liftCons1 Scalar

-- ====== Method: Cls_Vector.Scalar ====== --
memSig_Cls_Vector_Scalar = _rtup2 (_nuSigArg ("self"), _nuSigArg ("x"))
memFnc_Cls_Vector_Scalar = (memSig_Cls_Vector_Scalar, memDef_Cls_Vector_Scalar)
$(registerMethod ''Cls_Vector "Scalar")


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2 (_nuSigArg ("self"), _npSigArg ("s", val ("" :: String)))
memDef_Main_print self s = polyJoin . liftF1 (Value . fmap Safe . print) $ s
memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = do 
    _call (0) (appNext (_call (1) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) cons_Vector)))) (_member ("print") _self))
    polyJoin (liftF1 (\pat_base -> case pat_base of 
        Vector {} -> let { _rtupX3 (_x, _y, _z) = (expandEl (layout_Vector pat_base)) } in _call (2) (appNext _x (_member ("print") _self))
        _ -> (error "Non-exhaustive patterns in case")) (_call (3) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) cons_Vector)))))
    (extractRTuple -> _rtupX3 (_x, _y, _z)) <- polyJoin (liftF1 (\pat_base -> case pat_base of 
        Vector {} -> let { _rtupX3 (_x, _y, _z) = (expandEl (layout_Vector pat_base)) } in val _rtupX3 (_x, _y, _z)
        _ -> (error "Non-exhaustive patterns in case")) (_call (4) (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) cons_Vector)))))
    _call (5) (appNext _z (_member ("print") _self))
    _a <- _call (6) cons_True
    _call (7) (appNext _a (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

