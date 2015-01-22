-------- HSC --------
-- extensions --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --
#include "pragmas.cpp"

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic)

-- ------ Main.Main constructor ------ --
cons_Main = member("Main") (val Cls_Main)
memSig_Cls_Main_Main = rtup1(nparam("self"))
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memDef_Main_print rtup2(self, s) = do 
    polyJoin . liftF1 (Value . fmap Safe . print) $ s
     

memSig_Main_print = rtup2(nparam("self"), param)
$(registerMethod ''Main "print")

-- ====== Method: Int._plus ====== --
memDef_Int__plus rtup2(self, a) = do 
    liftF2 (+) self a
     

memSig_Int__plus = rtup2(nparam("self"), param)
$(registerMethod ''Int "_plus")

-- ====== Method: Main.foo ====== --
memDef_Main_foo rtup2(_self, (extractTuple2 -> (_a, _b))) = do 
     call (appNext (val (1 :: Int)) (appNext _a (member("_plus") _self)))
     

memSig_Main_foo = rtup2(nparam("self"), param)
$(registerMethod ''Main "foo")

-- ====== Method: Main._plus ====== --
memDef_Main__plus rtup3(_self, _a, _b) = do 
     call (appNext _b (member("_plus") _a))
     

memSig_Main__plus = rtup3(nparam("self"), param, param)
$(registerMethod ''Main "_plus")

-- ====== Method: Main.main ====== --
memDef_Main_main rtup1(_self) = do 
     call (appNext typed(val [], [Int]) (member("print") _self))
     call (appNext (call (appNext (val (val (5 :: Int), val ("b" :: String))) (member("foo") _self))) (member("print") _self))
     

memSig_Main_main = rtup1(nparam("self"))
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
