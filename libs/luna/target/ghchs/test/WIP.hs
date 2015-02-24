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

-- ====== Float type ====== --
-- datatype provided externally
$(registerType ''Float)


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

-- ====== Method: Float._star ====== --
memSig_Float__star = _rtup2 (_nuSigArg ("self"), _nuSigArg ("a"))
memDef_Float__star self a = liftF2 (*) self a
memFnc_Float__star = (memSig_Float__star, memDef_Float__star)
$(registerMethod ''Float "_star")

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
    _call (2) (appNext (_call (3) (appNext (val (2.0 :: Float)) (appNext (val (1.0 :: Float)) (_member ("_star") _self)))) (_member ("print") _self))
    _call (4) (appNext (_call (5) (appNext (val (4.0 :: Float)) (appNext (val (3.0 :: Float)) (_member ("foo") _self)))) (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

