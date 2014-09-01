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
module Main2 where

-- imports --
import Luna.Target.HS

-- body --

import qualified GHC.Prim
import Control.PolyMonad
import Control.PolyApplicative
import Type.BaseType

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

-- ====== Constructor: Vector.Vector ====== --
data Vector a = Vector a a a deriving (Show, Eq, Ord, Generic)
data Cls_Vector = Cls_Vector deriving (Show, Eq, Ord, Generic)
cons_Vector = member (Proxy :: Proxy "Vector") (val Cls_Vector)
memSig_Cls_Vector_Vector = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ((mkArg :: Param), ()))))
memDef_Cls_Vector_Vector = liftCons3 Vector
$(registerMethod ''Cls_Vector "Vector")
$(generateFieldAccessors 'Vector [Just "x", Just "y", Just "z"])

-- ====== Method: Vector.test ====== --
memDef_Vector_test (_v_89, (_v_11, (_v_13, ()))) = do {
     val ();
     val (_v_11, _v_13);
     
}
memSig_Vector_test = ((mkArg :: NParam "self"), ((mkArg :: Param), ((mkArg :: Param), ())))
$(registerMethod ''Vector "test")


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
memDef_Main_print (_v_91, (_v_20, ())) = do {
     val ();
     autoLift1 print _v_20;
     
}
memSig_Main_print = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Main "print")

-- ====== Method: Int._43 ====== --
memDef_Int__43 (_v_93, (_v_27, ())) = do {
     val ();
     liftF2 (+) _v_93 _v_27;
     
}
memSig_Int__43 = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Int "_43")

-- ====== Method: Int._62 ====== --
memDef_Int__62 (_v_95, (_v_36, ())) = do {
     val ();
     liftF2 (>) _v_95 _v_36;
     
}
memSig_Int__62 = ((mkArg :: NParam "self"), ((mkArg :: Param), ()))
$(registerMethod ''Int "_62")

-- ====== Method: Int.inc ====== --
memDef_Int_inc (_v_97, ()) = do {
     val ();
     call (appNext (val (1 :: Int)) (member (Proxy :: Proxy "_43") _v_97));
     
}
memSig_Int_inc = ((mkArg :: NParam "self"), ())
$(registerMethod ''Int "inc")

--instance (args_aosf ~ (m_aoqG (s_aoqH a_aoqI), ()),
--          out_aosg ~ m3_aoqC b_aoqD,
--          PolyMonad (Value Pure) m2_aoqB m3_aoqC,
--          Func obj_aoqJ "_43" argsout_aoqL (m2_aoqB b_aoqD),
--          HasMem "_43" obj_aoqJ args1_aoqK,
--          AppNextArg (Value Pure (Safe Int)) args1_aoqF args_aoqE,
--          AppArgByName "self" (m_aoqG (s_aoqH a_aoqI)) args1_aoqK args1_aoqF,
--          ReadArgs args_aoqE argsout_aoqL,
--          Type.BaseType.BaseType (Proxy a_aoqI) (Proxy obj_aoqJ)) =>
--         Func Int "inc" args_aosf out_aosg where
--  getFunc _ _ = memDef_Int_inc


-- ====== Method: Main.main ====== --
memDef_Main_main (_v_99, ()) = do {
     val ();
     call (appNext (ifThenElse' (call (appNext (val (2 :: Int)) (member (Proxy :: Proxy "_62") (val (1 :: Int)))))(do {
         val (5 :: Int);
         
    })(do {
         val (6 :: Int);
         
    })) (member (Proxy :: Proxy "print") (call cons_Main)));
     call (appNext (call (appNext (val (2 :: Int)) (member (Proxy :: Proxy "_62") (val (1 :: Int))))) (member (Proxy :: Proxy "print") (call cons_Main)));
     _v_74 <- call (appNext (val (3 :: Int)) (appNext (val (2 :: Int)) (appNext (val (1 :: Int)) cons_Vector)));
     call (appNext _v_74 (member (Proxy :: Proxy "print") (call cons_Main)));
     
}
memSig_Main_main = ((mkArg :: NParam "self"), ())
$(registerMethod ''Main "main")
main = mainMaker cons_Main
