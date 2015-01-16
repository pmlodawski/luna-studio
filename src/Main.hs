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
{-# LANGUAGE ExtendedDefaultRules #-}

-- module --
module Main where

-- imports --
import Luna.Target.HS
import Data.Typeable

-- body --

print_DBG val = Value $ fmap Safe $ print val

printT val = print_DBG $ typeOf val


-- ====== Main type ====== --
data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)

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

-- ====== Method: Main.main ====== --
memDef_Main_main (_self, ()) = do {
    val ();
    --call (appNext (val []) (member (Proxy :: Proxy "print") _self));
     
}

--foo _self = call (appNext (val []) (member (Proxy :: Proxy "print") _self))

--memSig_Main_main = ((mkArg :: NParam "self"), ())
-- $(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
--main = mainMaker cons_Main


main = fromValue $ call (appNext (val []) (member (Proxy :: Proxy "print") $ call cons_Main))

--main = fromValue $ call (appNext (val []) (member (Proxy :: Proxy "print") (call cons_Main)))