---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils where

import qualified Flowbox.Prelude             as Prelude
import           Flowbox.Prelude             hiding (error, id)
import qualified Flowbox.Luna.Data.HAST.Expr as HExpr


mkCFName     = ("CField_" ++)
mkFCName     = ("FC_" ++)
mkGetName    = ("get" ++)
mkTHVarName  = ("'" ++)
mkTHTypeName = ("''" ++)
mkFieldName  = (++"_T")

genTH f a b c = HExpr.AppE (HExpr.Var f) 
              $ HExpr.AppE (HExpr.Var $ mkTHTypeName a) 
              $ HExpr.AppE (HExpr.Var $ mkTHVarName  b) 
              $ HExpr.Var $ mkTHVarName c

genTHF = genTH "mkInst"
genTHC = genTH "mkInstC"

genFCImport name = HExpr.Import False ["Flowbox", "Luna", "FClasses", "U_" ++ name] Nothing

genCFDec name = HExpr.NewTypeD name ["a"] (HExpr.Con name [HExpr.Typed (HExpr.Var "a") (HExpr.Var $ mkGetName name)])

genCon name fnum = HExpr.Function ("con_" ++ name) [] 
                 $ HExpr.AppE (HExpr.Var $ "_mkPure" ++ show fnum) 
                 $ HExpr.Var name