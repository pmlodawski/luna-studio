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
module Main2 where

-- imports --
import Luna.Target.HS

-- body --


-- ===================================================================
-- Data types
-- ===================================================================

-- ------ Main.Main constructor ------ --
data Main = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main = Cls_Main deriving (Show, Eq, Ord, Generic)
cons_Main = member (Proxy :: Proxy "Main") (val Cls_Main)
memSig_Cls_Main_Main = ((mkArg :: NParam "self"), ())
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")
$(generateFieldAccessors 'Main [])
-- Other data types

-- ====== Vector type ====== --
data Vector a = Vector a a a | Scalar a deriving (Show, Eq, Ord, Generic)
data Cls_Vector = Cls_Vector deriving (Show, Eq, Ord, Generic)

-- ------ Vector.Vector constructor ------ --
cons_Vector = member (Proxy :: Proxy "Vector") (val Cls_Vector)
memSig_Cls_Vector_Vector = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ((mkArg :: Param), ()))))
memDef_Cls_Vector_Vector = liftCons3 Vector
$(registerMethod ''Cls_Vector "Vector")
$(generateFieldAccessors 'Vector [Just "x", Just "y", Just "z"])

-- ------ Vector.Scalar constructor ------ --
cons_Scalar = member (Proxy :: Proxy "Scalar") (val Cls_Vector)
memSig_Cls_Vector_Scalar = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
memDef_Cls_Vector_Scalar = liftCons1 Scalar
$(registerMethod ''Cls_Vector "Scalar")
$(generateFieldAccessors 'Scalar [Just "w"])

-- ------ Vector methods ------ --

-- ====== Bool type ====== --
data Cls_Bool = Cls_Bool deriving (Show, Eq, Ord, Generic)

-- ------ Bool.True constructor ------ --
cons_True = member (Proxy :: Proxy "True") (val Cls_Bool)
memSig_Cls_Bool_True = ((mkArg :: NParam "self"), ())
memDef_Cls_Bool_True = liftCons0 True
$(registerMethod ''Cls_Bool "True")
$(generateFieldAccessors 'True [])

-- ------ Bool.False constructor ------ --
cons_False = member (Proxy :: Proxy "False") (val Cls_Bool)
memSig_Cls_Bool_False = ((mkArg :: NParam "self"), ())
memDef_Cls_Bool_False = liftCons0 False
$(registerMethod ''Cls_Bool "False")
$(generateFieldAccessors 'False [])


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
memDef_Main_print (_v_45, (_v_20, ())) = do {
     val ();
     polyJoin . liftF1 (Value . fmap Safe . print) $ _v_20;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memDef_Main_main (_v_47, ()) = do {
     val ();
     _v_28 <- call (appNext (val 3) (appNext (val 2) (appNext (val 1) cons_Vector)));
     call (appNext (call cons_True) (member (Proxy :: Proxy "print") (call cons_Main)));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main