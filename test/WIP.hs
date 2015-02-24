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

-- ====== Foo type ====== --
data Foo a 
    = Foo a
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Foo)
$(registerCons ''Foo ["Foo"])

-- ------ Foo accessors ------ --
$(generateFieldAccessors ''Foo [('Foo, [Just "x"])])
$(registerFieldAccessors ''Foo ["x"])

-- ------ Foo.Foo constructor ------ --
memDef_Cls_Foo_Foo = liftCons1 Foo

-- ====== Method: Cls_Foo.Foo ====== --
memSig_Cls_Foo_Foo = _rtup2 (_nuSigArg ("self"), _nuSigArg ("x"))
memFnc_Cls_Foo_Foo = (memSig_Cls_Foo_Foo, memDef_Cls_Foo_Foo)
$(registerMethod ''Cls_Foo "Foo")


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

-- ====== Method: Main._star ====== --
memSig_Main__star = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main__star _self _a _b = _call (0) (appNext _b (_member ("_star") _a))
memFnc_Main__star = (memSig_Main__star, memDef_Main__star)
$(registerMethod ''Main "_star")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup3 (_nuSigArg ("self"), _nuSigArg ("a"), _nuSigArg ("b"))
memDef_Main_foo _self _a _b = _call (1) (appNext _b (_member ("_star") _a))
memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = do 
    _v <- _call (2) (appNext (val _rtupX2 (val (1 :: Int), val (2 :: Int))) cons_Foo)
    _call (3) (appNext _v (_member ("print") _self))
    _call (4) (appNext (val _rtupX2 (val (1 :: Int), val (2 :: Int))) (_member ("print") _self))
    _call (5) (appNext (_call (6) (_member ("x") _v)) (_member ("print") _self))
    _v <- _call (7) (appNext (val _rtupX2 (val (1 :: Int), val (2 :: Int))) (_member ("set_x") _v))
    _call (8) (appNext _v (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

