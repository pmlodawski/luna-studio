{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic)

-- ------ Main.Main constructor ------ --
cons_Main = member (Proxy :: Proxy "Main") (val Cls_Main)
memSig_Cls_Main_Main = ((mkArg :: NParam "self"), ())
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")
$(generateFieldAccessors 'Main [])

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memDef_Main_print (_self, (_s, ())) = do {
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ _s;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memDef_Main_main (_self, ()) = do {
     call (appNext (val (5 :: Int)) (member (Proxy :: Proxy "print") _self));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
