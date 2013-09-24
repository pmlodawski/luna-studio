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


mkCFName     = ("CF_" ++)
mkFCName     = ("FC_" ++)
mkGetName    = ("get" ++)
mkTHVarName  = ("'" ++)
mkTHTypeName = ("''" ++)
mkFieldName  = (++"_T")
mkFuncName   = ("f_" ++ )

mangleName cname fname = cname ++ "_" ++ fname

--mkTName      = (++ "_T")
mkTName i name = mkGetNName i ++ "_" ++ name ++ "_T"

mkCGetCName i = "Get" ++ show i
mkGetNName  i = "get" ++ show i
mkCGetName  i = "_" ++ mkGetNName i

--genTH f a b c = HExpr.AppE (HExpr.Var f) 
--              $ HExpr.AppE (HExpr.Var $ mkTHTypeName a) 
--              $ HExpr.AppE (HExpr.Var $ mkTHVarName  b) 
--              $ HExpr.Var $ mkTHVarName c

genTH f a b c = foldl (HExpr.AppE) (HExpr.Var f) vars where
			    vars = map HExpr.Var [mkTHTypeName a, mkTHVarName  b, mkTHVarName c]
genTHF = genTH "mkInst"
genTHC = genTH "mkInstC"

genFCImport name = HExpr.Import False ["FlowboxM", "Luna", "FClasses", "U_" ++ name] Nothing

genCFDec cname params cfname = 
	HExpr.NewTypeD cfname params 
    $ HExpr.Con cfname [HExpr.Typed t (HExpr.Var $ mkGetName cfname)]
    --where t = (HExpr.Var "a")
    where t = mkPure $ foldl (HExpr.AppE) (HExpr.Var cname) (map HExpr.Var params)

genCon name fnum = HExpr.Function ("con_" ++ name) [] 
                 $ HExpr.AppE (HExpr.Var $ "mkPure" ++ show fnum) 
                 $ HExpr.Var name


mkPure = HExpr.AppE (HExpr.Var "Pure")