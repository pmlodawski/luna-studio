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

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
memDef_Main_print self s = do 
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
$(registerMethod ''Main "print")

-- ====== Method: Main.id ====== --
memSig_Main_id = _rtup2(_nuSigArg("self"), _nuSigArg("x"))
memDef_Main_id _self _x = do 
     _x
     

memFnc_Main_id = (memSig_Main_id, memDef_Main_id)
$(registerMethod ''Main "id")

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup3(_nuSigArg("self"), _nuSigArg("a"), _nuSigArg("b"))
memDef_Main_foo _self _a _b = do 
     val (call (appNext _a (_member("id") _self)), call (appNext _b (_member("id") _self)))
     

memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))
memDef_Main_main _self = do 
     call (appNext (val (call (appNext (val (5 :: Int)) (_member("id") _self)), call (appNext (val ("ala" :: String)) (_member("id") _self)))) (_member("print") _self))
     call (appNext (call (appNext (val ("a" :: String)) (appNext (val (1 :: Int)) (_member("foo") _self)))) (_member("print") _self))
     _a <- mkLam _rtup1(_nuSigArg("x")) (\_x -> _x)
     val ()
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

