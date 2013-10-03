---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils where

import           Flowbox.Prelude             hiding (error, id)
import qualified Flowbox.Luna.Data.HAST.Expr as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit  as HLit


mkCFName     = ("CF_" ++)
mkCCName     = ("CC_" ++)
mkFCName     = ("FC" ++)
mkGetName    = ("get" ++)
mkTHVarName  = ("'" ++)
mkTHTypeName = ("''" ++)
mkFieldName  = (++"_T")
mkFuncName   = ("f_" ++)
mkVarName    = ("_" ++)
mkConsName   = ("_" ++)

mangleName cname fname = cname ++ fname

--mkTName      = (++ "_T")
mkTName i name = mkGetNName i ++ "_" ++ name ++ "_T"

mkCGetCName i = "Get" ++ show i
mkGetNName  i = "get" ++ show i
mkCGetName  i = mkGetNName i

--genTH f a b c = HExpr.AppE (HExpr.Var f) 
--              $ HExpr.AppE (HExpr.Var $ mkTHTypeName a) 
--              $ HExpr.AppE (HExpr.Var $ mkTHVarName  b) 
--              $ HExpr.Var $ mkTHVarName c

genTH f a b c = foldl (HExpr.AppE) (HExpr.Var f) vars where
			    vars = map HExpr.Var [mkTHTypeName a, mkTHVarName b, mkTHVarName c]
genTHInst  = genTH "mkInst"
genTHInstC = genTH "mkInstC"

genFCImport name = HExpr.Import False ["FlowboxM", "Luna", "FClasses", "U" ++ mkVarName name] Nothing


genCFDec cname cfname = foldl HExpr.AppE (HExpr.Var "mkNTWrapper") [ HExpr.Lit $ HLit.String cfname
                                                                   , HExpr.Var $ mkTHTypeName cname
                                                                   ]


genCCDec name = HExpr.DataD name [] [HExpr.Con name []] []

--genCon name fnum = HExpr.Function ("con" ++ mkConsName name) [] 
--                 $ HExpr.AppE (HExpr.Var $ "mkPure" ++ show fnum) 
--                 $ HExpr.Var name

genCon name ccname = HExpr.Function ("con" ++ mkConsName name) [] 
                   $ (HExpr.Var ccname) -- mkPure


mkPure   = HExpr.AppE (HExpr.Var "Pure")
mkPureIO = HExpr.AppE (HExpr.Var "pureIO")
mkGetIO  = HExpr.AppE (HExpr.Var "getIO")
mkIO     = HExpr.AppE (HExpr.ConE ["IO"])


emptyHExpr = mkPureIO (HExpr.Var "()")