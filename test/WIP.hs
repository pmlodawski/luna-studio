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

-- ====== List type ====== --
-- datatype provided externally
$(registerType ''List)


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.foo ====== --
memSig_Main_foo = _rtup1 (_nuSigArg ("self"))
memDef_Main_foo _self = val (1 :: Int)
memFnc_Main_foo = (memSig_Main_foo, memDef_Main_foo)
$(registerMethod ''Main "foo")

-- ====== Method: List.bar ====== --
memSig_List_bar = _rtup1 (_nuSigArg ("self"))
memDef_List_bar _self = _call (0) (_member ("foo") _self)
memFnc_List_bar = (memSig_List_bar, memDef_List_bar)
$(registerMethod ''List "bar")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

