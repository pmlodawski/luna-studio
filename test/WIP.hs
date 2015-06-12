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
module Test where

-- imports --
import Luna.Target.HS
import Foo

-- body --
#include "pragmas.h"

-- ====== Test type ====== --
data Test 
    = Test
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Test)
$(registerCons ''Test ["Test"])

-- ------ Test.Test constructor ------ --
memDef_Cls_Test_Test = liftCons0 Test

-- ====== Method: Cls_Test.Test ====== --
memSig_Cls_Test_Test = _rtup1(_nuSigArg("self"))
memFnc_Cls_Test_Test = (memSig_Cls_Test_Test, memDef_Cls_Test_Test)
$(registerMethod ''Cls_Test "Test")


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Test.print ====== --
memSig_Test_print = _rtup2(_nuSigArg("self"), _nuSigArg("msg"))
memDef_Test_print self msg = autoLift1 print msg
memFnc_Test_print = (memSig_Test_print, memDef_Test_print)
$(registerMethod ''Test "print")

-- ====== Method: Test.main ====== --
memSig_Test_main = _rtup1(_nuSigArg("self"))
memDef_Test_main _self = _call(0) (appNext (val (1 :: Int)) (_member("print") _self))
memFnc_Test_main = (memSig_Test_main, memDef_Test_main)
$(registerMethod ''Test "main")

