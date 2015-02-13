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
$(registerType ''Main)

-- ------ Main.Main constructor ------ --
cons_Main = _member("Main") (val Cls_Main)
memDef_Cls_Main_Main = liftCons0 Main

-- ====== --------------- ====== --

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
$(registerMethod ''Cls_Main "Main")


-- ===================================================================
-- Data headers
-- ===================================================================

-- ====== Ala7 type ====== --
data Ala7  = Ola4 deriving (Show, Eq, Ord, Generic, Typeable)
$(registerType ''Ala7)

-- ------ Ala7.Ola4 constructor ------ --
cons_Ola4 = _member("Ola4") (val Cls_Ala7)
memDef_Cls_Ala7_Ola4 = liftCons0 Ola4

-- ====== --------------- ====== --

-- ====== Method: Cls_Ala7.Ola4 ====== --
memSig_Cls_Ala7_Ola4 = _rtup1(_nuSigArg("self"))
memFnc_Cls_Ala7_Ola4 = (memSig_Cls_Ala7_Ola4, memDef_Cls_Ala7_Ola4)
$(registerMethod ''Cls_Ala7 "Ola4")


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1(_nuSigArg("self"))
memDef_Main_main _self = do 
     _call(0) cons_Ola4
     

memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

