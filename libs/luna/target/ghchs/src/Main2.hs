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
     polyJoin . liftF1 (Value . fmap Safe . print) $ _v_4;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Main.main ====== --
memDef_Main_main (_v_36, ()) = do {
     val ();
     _v_12 <- val 3;
     polyJoin (liftF1 (\ a -> (case a of {
        1 -> do {
             call (appNext (val 2) (member (Proxy :: Proxy "print") (call cons_Main)));
             
        };
         2 -> do {
             call (appNext (val 3) (member (Proxy :: Proxy "print") (call cons_Main)));
             
        };
         _ -> error "TODO (!!!) Main2.hs: path/Main2.hs:(...,...)-(...,...): Non-exhaustive patterns in case";
        
    })) _v_12);
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")


-- ===================================================================
-- Main module wrappers
-- ===================================================================
main = mainMaker cons_Main
