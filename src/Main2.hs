-- extensions --
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

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --


-- ===================================================================
-- Data types
-- ===================================================================

-- ====== Constructor: Main.Main ====== --
data Main = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main = Cls_Main deriving (Show, Eq, Ord, Generic)
cons_Main = member (Proxy :: Proxy "Main") (val Cls_Main)
memSig_Cls_Main_Main = ((mkArg :: NParam "self"), ())
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")
$(generateFieldAccessors 'Main [])
-- Other data types


-- ===================================================================
-- Type aliases
-- ===================================================================


-- ===================================================================
-- Type defs
-- ===================================================================


-- ===================================================================
-- Module methods
-- ===================================================================

-- ====== Method: Main.print ====== --
memDef_Main_print (_v_34, (_v_4, ())) = do {
     val ();
     autoLift1 print _v_4;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Int._62 ====== --
memDef_Int__62 (_v_36, (_v_11, ())) = do {
     val ();
     liftF2 (>) _v_36 _v_11;
     
}
memSig_Int__62 = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Int "_62")

-- ====== Method: Main.main ====== --
memDef_Main_main (_v_38, ()) = do {
     val ();
     call (appNext (ifThenElse' (call (appNext (val (2 :: Int)) (member (Proxy :: Proxy "_62") (val (1 :: Int)))))(do {
         val (5 :: Int);
         
    })(do {
         val (6 :: Int);
         
    })) (member (Proxy :: Proxy "print") (call cons_Main)));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main