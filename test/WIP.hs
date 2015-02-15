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
data Main = Main
$( registerType ''Main )

-- ------ Main.Main constructor ------ --
cons_Main =(unknown :MacroE "_member" [Lit 
    _lval = String "Main"
](unknown :VarE 
    _name = "val"
 unknown :VarE 
    _name = "Cls_Main"
))
memDef_Cls_Main_Main =(unknown :VarE 
    _name = "liftCons0"
 unknown :VarE 
    _name = "Main"
)

-- ====== --------------- ====== --

-- ====== Method: Cls_Main.Main ====== --
memSig_Cls_Main_Main = unknown :MacroE "_rtup1" [MacroE "_nuSigArg" [Lit 
    _lval = String "self"
]]
memFnc_Cls_Main_Main = unknown :Tuple 
    _items = [VarE 
        _name = "memSig_Cls_Main_Main"
    ,
    VarE 
        _name = "memDef_Cls_Main_Main"
    ]

$( registerMethod ''Cls_Main unknown :Lit 
    _lval = String "Main"
 )


-- ===================================================================
-- Data headers
-- ===================================================================

-- ====== Ala7 type ====== --
data Ala7 = Ola4
$( registerType ''Ala7 )

-- ------ Ala7.Ola4 constructor ------ --
cons_Ola4 =(unknown :MacroE "_member" [Lit 
    _lval = String "Ola4"
](unknown :VarE 
    _name = "val"
 unknown :VarE 
    _name = "Cls_Ala7"
))
memDef_Cls_Ala7_Ola4 =(unknown :VarE 
    _name = "liftCons0"
 unknown :VarE 
    _name = "Ola4"
)

-- ====== --------------- ====== --

-- ====== Method: Cls_Ala7.Ola4 ====== --
memSig_Cls_Ala7_Ola4 = unknown :MacroE "_rtup1" [MacroE "_nuSigArg" [Lit 
    _lval = String "self"
]]
memFnc_Cls_Ala7_Ola4 = unknown :Tuple 
    _items = [VarE 
        _name = "memSig_Cls_Ala7_Ola4"
    ,
    VarE 
        _name = "memDef_Cls_Ala7_Ola4"
    ]

$( registerMethod ''Cls_Ala7 unknown :Lit 
    _lval = String "Ola4"
 )


-- ===================================================================
-- Data declarations
-- ===================================================================


-- ===================================================================
-- Module declarations
-- ===================================================================

-- ====== Method: Main.main ====== --
memSig_Main_main = unknown :MacroE "_rtup1" [MacroE "_nuSigArg" [Lit 
    _lval = String "self"
]]
memDef_Main_main _self = unknown :DoBlock 
    _exprs = [AppE 
        _src = MacroE "_call" [Lit 
            _lval = Int "0"
        ],
         _dst = Var 
            _name = "cons_Ola4"
        
    ]

memFnc_Main_main = unknown :Tuple 
    _items = [VarE 
        _name = "memSig_Main_main"
    ,
    VarE 
        _name = "memDef_Main_main"
    ]

$( registerMethod ''Main unknown :Lit 
    _lval = String "main"
 )


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main =(unknown :VarE 
    _name = "mainMaker"
 unknown :VarE 
    _name = "cons_Main"
)

