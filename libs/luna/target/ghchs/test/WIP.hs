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
data Foo 
    = Foo
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Foo)
$(registerCons ''Foo ["Foo"])

-- ------ Foo.Foo constructor ------ --
memDef_Cls_Foo_Foo = liftCons0 Foo

-- ====== Method: Cls_Foo.Foo ====== --
memSig_Cls_Foo_Foo = _rtup1 (_nuSigArg ("self"))
memFnc_Cls_Foo_Foo = (memSig_Cls_Foo_Foo, memDef_Cls_Foo_Foo)
$(registerMethod ''Cls_Foo "Foo")

-- ====== Test type ====== --
data Test 
    = Test Foo
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Test)
$(registerCons ''Test ["Test"])

-- ------ Test accessors ------ --
$(generateFieldAccessors ''Test [('Test, [Just "test"])])
$(registerFieldAccessors ''Test ["test"])

-- ------ Test.Test constructor ------ --
memDef_Cls_Test_Test = liftCons1 Test

-- ====== Method: Cls_Test.Test ====== --
memSig_Cls_Test_Test = _rtup2 (_nuSigArg ("self"), _nuSigArg ("test"))
memFnc_Cls_Test_Test = (memSig_Cls_Test_Test, memDef_Cls_Test_Test)
$(registerMethod ''Cls_Test "Test")


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
memDef_Main_main _self = val ()
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

