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

-- ====== RGB type ====== --
data RGB 
    = RGB Float Float Float
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''RGB)
$(registerCons ''RGB ["RGB"])

-- ------ RGB accessors ------ --
$(generateFieldAccessors ''RGB [('RGB, [Just "r", Just "g", Just "b"])])
$(registerFieldAccessors ''RGB ["b", "g", "r"])

-- ------ RGB.RGB constructor ------ --
memDef_Cls_RGB_RGB = liftCons3 RGB

-- ====== Method: Cls_RGB.RGB ====== --
memSig_Cls_RGB_RGB = _rtup4 (_nuSigArg ("self"), _nuSigArg ("r"), _nuSigArg ("g"), _nuSigArg ("b"))
memFnc_Cls_RGB_RGB = (memSig_Cls_RGB_RGB, memDef_Cls_RGB_RGB)
$(registerMethod ''Cls_RGB "RGB")

-- ====== Mult type ====== --
data Mult a 
    = Mult a Float
    deriving (Show,Eq,Ord,Generic,Typeable)
$(registerType ''Mult)
$(registerCons ''Mult ["Mult"])

-- ------ Mult accessors ------ --
$(generateFieldAccessors ''Mult [('Mult, [Just "data", Just "mult"])])
$(registerFieldAccessors ''Mult ["data", "mult"])

-- ------ Mult.Mult constructor ------ --
memDef_Cls_Mult_Mult = liftCons2 Mult

-- ====== Method: Cls_Mult.Mult ====== --
memSig_Cls_Mult_Mult = _rtup3 (_nuSigArg ("self"), _nuSigArg ("data"), _nuSigArg ("mult"))
memFnc_Cls_Mult_Mult = (memSig_Cls_Mult_Mult, memDef_Cls_Mult_Mult)
$(registerMethod ''Cls_Mult "Mult")


-- ===================================================================
-- Data declarations
-- ===================================================================

-- ====== Method: RGB.toRGB ====== --
memSig_RGB_toRGB = _rtup1 (_nuSigArg ("self"))
memDef_RGB_toRGB _self = _self
memFnc_RGB_toRGB = (memSig_RGB_toRGB, memDef_RGB_toRGB)
$(registerMethod ''RGB "toRGB")

-- ====== Method: Mult.toRGB ====== --
memSig_Mult_toRGB = _rtup1 (_nuSigArg ("self"))
memDef_Mult_toRGB _self = do 
    _r <- _call (0) (_member ("r") (_call (1) (_member ("data") _self)))
    _g <- _call (2) (_member ("g") (_call (3) (_member ("data") _self)))
    _b <- _call (4) (_member ("b") (_call (5) (_member ("data") _self)))
    _call (6) (appNext _b (appNext _g (appNext (_call (7) (appNext _r (_member ("_star") (_member ("mult") _self)))) cons_RGB)))
    _call (8) (appNext _b (appNext _g (appNext (_call (9) (appNext (_call (10) (_member ("mult") _self)) (_member ("_star") _r))) cons_RGB)))
memFnc_Mult_toRGB = (memSig_Mult_toRGB, memDef_Mult_toRGB)
$(registerMethod ''Mult "toRGB")


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

-- ====== Method: Main.main ====== --
memSig_Main_main = _rtup1 (_nuSigArg ("self"))
memDef_Main_main _self = do 
    _call (11) (appNext (val ("color test" :: String)) (_member ("print") _self))
    _rgb <- _call (12) (appNext (val (3.0 :: Float)) (appNext (val (2.0 :: Float)) (appNext (val (1.0 :: Float)) cons_RGB)))
    _mult <- _call (13) (appNext (val (0.5 :: Float)) (appNext _rgb cons_Mult))
    _call (14) (appNext (_call (15) (_member ("toRGB") _rgb)) (_member ("print") _self))
    _call (16) (appNext (_call (17) (_member ("toRGB") _mult)) (_member ("print") _self))
memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main

