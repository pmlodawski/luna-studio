-------- HSC --------
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

-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic)

-- ------ Main.Main constructor ------ --
cons_Main = member (Proxy :: Proxy "Main") (val Cls_Main)
memSig_Cls_Main_Main = ((mkArg :: NParam "self"), ())
memDef_Cls_Main_Main = liftCons0 Main
$(registerMethod ''Cls_Main "Main")

-- ------ Main methods ------ --

-- ====== Method: Main.print ====== --
memDef_Main_print (self, (s, ())) = do {
     
    polyJoin . liftF1 (Value . fmap Safe . print) $ s;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Int._plus ====== --
memDef_Int__plus (self, (a, ())) = do {
     
    liftF2 (+) self a;
     
}
memSig_Int__plus = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Int "_plus")

-- ====== Vector type ====== --
data Vector a = Vector a a a | Scalar a deriving (Show, Eq, Ord, Generic)
data Cls_Vector  = Cls_Vector deriving (Show, Eq, Ord, Generic)

-- ------ Vector.Vector constructor ------ --
cons_Vector = member (Proxy :: Proxy "Vector") (val Cls_Vector)
memSig_Cls_Vector_Vector = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ((mkArg :: Param), ()))))
memDef_Cls_Vector_Vector = liftCons3 Vector
$(registerMethod ''Cls_Vector "Vector")

-- ------ Vector.Scalar constructor ------ --
cons_Scalar = member (Proxy :: Proxy "Scalar") (val Cls_Vector)
memSig_Cls_Vector_Scalar = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
memDef_Cls_Vector_Scalar = liftCons1 Scalar
$(registerMethod ''Cls_Vector "Scalar")

-- ------ Vector accessors ------ --
$(generateFieldAccessors ''Vector [('Vector, [Just "x", Just "y", Just "z"]), ('Scalar, [Just "x"])])
$(registerFieldAccessors ''Vector ["x", "y", "z"])

-- ------ Vector methods ------ --

-- ====== Method: Main.main ====== --
memDef_Main_main (_self, ()) = do {
     _v <- call (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) cons_Vector)));
     _v <- call (appNext (val (10 :: Int)) (member (Proxy :: Proxy "set_x") _v));
     call (appNext (call (member (Proxy :: Proxy "x") _v)) (member (Proxy :: Proxy "print") _self));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
