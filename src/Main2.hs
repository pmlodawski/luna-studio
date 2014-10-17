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
{-# LANGUAGE ViewPatterns #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS

-- body --


-- ===================================================================
-- Data types
-- ===================================================================

-- ------ Main.Main constructor ------ --
data Main  = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic)
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
memDef_Main_print (_v_43, (_v_4, ())) = do {
     val ();
     polyJoin . liftF1 (Value . fmap Safe . print) $  _v_4;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Int._62 ====== --
memDef_Int__62 (_v_45, (_v_11, ())) = do {
     val ();
     liftF2 (>)  _v_45 _v_11;
     
}
memSig_Int__62 = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Int "_62")

-- ====== Method: Main._62 ====== --
memDef_Main__62 (_v_47, (_v_19, (_v_21, ()))) = do {
     val ();
     call (appNext _v_21 (member (Proxy :: Proxy "_62") _v_19));
     
}
memSig_Main__62 = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ())))
$(registerMethod ''Main "_62")

-- ====== Method: Main.main ====== --
memDef_Main_main (_v_49, ()) = do {
     val ();
     call (appNext (call (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) (member (Proxy :: Proxy "_62") (call cons_Main))))) (member (Proxy :: Proxy "print") (call cons_Main)));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
