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
#include "pragmas.h"

-- ====== Main type ====== --
data Main 
    = Main
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Main)
$(registerCons ''Main ["Main"])

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
$(registerCons ''Bool ["True", "False"])

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

-- ====== List type ====== --
-- datatype provided externally
$(registerType ''List)


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2 (_nuSigArg ("self"), _nuSigArg ("msg"))
memDef_Main_print self msg = autoLift1 print msg
memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Main.whatis ====== --
memSig_Main_whatis = _rtup2 (_nuSigArg ("self"), _nuSigArg ("a"))
memDef_Main_whatis self a = autoLift1 print (val a)
memFnc_Main_whatis = (memSig_Main_whatis, memDef_Main_whatis)
$(registerMethod ''Main "whatis")

-- ====== Method: List.at ====== --
memSig_List_at = _rtup2 (_nuSigArg ("self"), _nuSigArg ("idx"))
memDef_List_at self idx = liftF2 (!!) self idx
memFnc_List_at = (memSig_List_at, memDef_List_at)
$(registerMethod ''List "at")

-- ====== Method: Bool._equals_equals ====== --
memSig_Bool__equals_equals = _rtup2 (_nuSigArg ("self"), _nuSigArg ("a"))
memDef_Bool__equals_equals self a = liftF2 (==) self a
memFnc_Bool__equals_equals = (memSig_Bool__equals_equals, memDef_Bool__equals_equals)
$(registerMethod ''Bool "_equals_equals")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main_foo self a b = liftF2 (==) a b
memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main._equals_equals ====== --
memSig_Main__equals_equals = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main__equals_equals _self _a _b = _call (0) (appNext _b (_member ("_equals_equals") _a))
memFnc_Main__equals_equals = (memSig_Main__equals_equals, memDef_Main__equals_equals)
$(registerMethod ''Main "_equals_equals")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = do 
    _a <- lstCons (val (1 :: Int)) (lstCons (val (2 :: Int)) (lstCons (val (3 :: Int)) (val [])))
    _call (1) (appNext (_call (2) (appNext (val (0 :: Int)) (_member ("at") _a))) (_member ("print") _self))
    _call (3) (appNext _a (_member ("print") _self))
    _call (4) (appNext (_call (5) (appNext (_call (6) cons_False) (appNext (_call (7) cons_True) (_member ("foo") _self)))) (_member ("print") _self))
    _call (8) (appNext (_call (9) (appNext (_call (10) cons_False) (appNext (_call (11) cons_True) (_member ("_equals_equals") _self)))) (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

