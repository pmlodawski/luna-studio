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

-- ====== Method: List.empty ====== --
memSig_List_empty = _rtup1 (_nuSigArg ("self"))
memDef_List_empty self = liftF1 null self
memFnc_List_empty = (memSig_List_empty, memDef_List_empty)
$(registerMethod ''List "empty")

-- ====== Method: List.bool ====== --
memSig_List_bool = _rtup1 (_nuSigArg ("self"))
memDef_List_bool _self = _call (0) (appNext (_call (1) (_member ("empty") _self)) (_member ("not") (_call (2) cons_Main)))
memFnc_List_bool = (memSig_List_bool, memDef_List_bool)
$(registerMethod ''List "bool")

-- ====== Method: Bool._equals_equals ====== --
memSig_Bool__equals_equals = _rtup2 (_nuSigArg ("self"), _nuSigArg ("a"))
memDef_Bool__equals_equals self a = liftF2 (==) self a
memFnc_Bool__equals_equals = (memSig_Bool__equals_equals, memDef_Bool__equals_equals)
$(registerMethod ''Bool "_equals_equals")

-- ====== Method: Bool.bool ====== --
memSig_Bool_bool = _rtup1 (_nuSigArg ("self"))
memDef_Bool_bool _self = _self
memFnc_Bool_bool = (memSig_Bool_bool, memDef_Bool_bool)
$(registerMethod ''Bool "bool")

-- ====== Method: Main.not ====== --
memSig_Main_not = _rtup2 (_nuSigArg ("self"), _nuSigArg ("a"))
memDef_Main_not _self _a = polyJoin (liftF1 (\pat_base -> case pat_base of 
    True -> (_call (3) cons_False)
    False -> (_call (4) cons_True)
    _ -> (error "Non-exhaustive patterns in case")) (_call (5) (_member ("bool") _a)))
memFnc_Main_not = (memSig_Main_not, memDef_Main_not)
$(registerMethod ''Main "not")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main_foo self a b = liftF2 (==) a b
memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main._equals_equals ====== --
memSig_Main__equals_equals = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main__equals_equals _self _a _b = _call (6) (appNext _b (_member ("_equals_equals") _a))
memFnc_Main__equals_equals = (memSig_Main__equals_equals, memDef_Main__equals_equals)
$(registerMethod ''Main "_equals_equals")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = do 
    _a <- val []
    _call (7) (appNext (_call (8) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) (appNext (_call (9) (appNext _a (_member ("not") _self))) (_member ("if_then_else") _self))))) (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")

-- ====== Method: Main.if_then_else ====== --
memSig_Main_if_then_else = _rtup4 (_nuSigArg ("self"), _nuSigArg ("base"), _nuSigArg ("ok"), _nuSigArg ("fail"))
memDef_Main_if_then_else _self _base _ok _fail = polyJoin (liftF1 (\pat_base -> case pat_base of 
    True -> _ok
    False -> _fail
    _ -> (error "Non-exhaustive patterns in case")) (_call (10) (_member ("bool") _base)))
memFnc_Main_if_then_else = (memSig_Main_if_then_else, memDef_Main_if_then_else)
$(registerMethod ''Main "if_then_else")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

