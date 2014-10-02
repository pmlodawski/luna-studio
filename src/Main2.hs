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
memDef_Main_print (_v_26, (_v_4, ())) = do {
     val ();
     polyJoin . liftF1 (Value . fmap Safe . print) $ _v_4;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memDef_Main_main (_v_28, ()) = do {
     val ();
     (extractTuple2 -> (_v_14, _v_15)) <- val ((val (1 :: Int)), (val (2 :: Int)));
     call (appNext _v_14 (member (Proxy :: Proxy "print") (call cons_Main)));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
