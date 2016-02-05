{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

-- module --
module Main3 where

-- -- imports --
-- import Luna.Target.HS
--
-- -- body --
-- #include "pragmas.h"
--
-- -- ====== Main type ====== --
-- data Main  = Main deriving (Show, Eq, Ord, Generic, Typeable)
-- data Cls_Main  = Cls_Main deriving (Show, Eq, Ord, Generic, Typeable)
--
-- -- ------ Main.Main constructor ------ --
-- cons_Main = _member("Main") (val Cls_Main)
-- memDef_Cls_Main_Main = liftCons0 Main
--
-- -- ====== Method: Cls_Main.Main ====== --
-- memSig_Cls_Main_Main = _rtup1(_nuSigArg("self"))
-- memFnc_Cls_Main_Main = (memSig_Cls_Main_Main, memDef_Cls_Main_Main)
-- $(registerMethod ''Cls_Main "Main")
--
-- -- ------ Main methods ------ --
--
-- -- ====== Method: Main.print ====== --
-- memSig_Main_print = _rtup2(_nuSigArg("self"), _npSigArg("s", val ("" :: String)))
-- memDef_Main_print self s = do
--
--     polyJoin . liftF1 (Value . fmap Safe . print) $ s
--
--
-- memFnc_Main_print = (memSig_Main_print, memDef_Main_print)
-- $(registerMethod ''Main "print")
--
-- -- ====== Method: Main.main ====== --
-- memSig_Main_main = _rtup1(_nuSigArg("self"))
-- memDef_Main_main _self = do
--      a <- mkLam (_rtup1(_nuSigArg("a"))) (\a -> a)
--      call (appNext (val []) (_member("print") _self))
--      call (_member("print") _self)
--      --call (appNext (    call $ appNext (val "!!!") $ mkLam (_rtup1(_nuSigArg("a"))) (\a -> a)      ) (_member("print") _self))
--
--
--
-- memFnc_Main_main = (memSig_Main_main, memDef_Main_main)
-- $(registerMethod ''Main "main")
--
--
-- -- ===================================================================
-- -- Main module wrappers
-- -- ===================================================================
-- main = mainMaker cons_Main
