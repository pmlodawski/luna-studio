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

-- ====== Vector type ====== --
data Vector a = Vector deriving (Show, Eq, Ord, Generic)
data Cls_Vector  = Cls_Vector deriving (Show, Eq, Ord, Generic)

-- ------ Vector.Vector constructor ------ --
cons_Vector = member (Proxy :: Proxy "Vector") (val Cls_Vector)
memSig_Cls_Vector_Vector = ((mkArg :: NParam "self"), ())
memDef_Cls_Vector_Vector = liftCons0 Vector
$(registerMethod ''Cls_Vector "Vector")
$(generateFieldAccessors 'Vector [])

-- ------ Vector methods ------ --

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

-- ====== Method: Main._plus ====== --
memDef_Main__plus (_self, (_a, (_b, ()))) = do {
     call (appNext _b (member (Proxy :: Proxy "_plus") _a));
     
}
memSig_Main__plus = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ())))
$(registerMethod ''Main "_plus")

-- ====== Method: Main.main ====== --
memDef_Main_main (_self, ()) = do {
     call (appNext (call (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) (member (Proxy :: Proxy "_plus") _self)))) (member (Proxy :: Proxy "print") _self));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
