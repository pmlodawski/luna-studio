module Batch.Function where

import           Utils.PreludePlus
import qualified Generated.Proto.Dep.Expr.Function   as GenFunction
import qualified Generated.Proto.Dep.Expr.Expr.Cls   as GenCls
import qualified Generated.Proto.Dep.Expr.Expr       as Gen

import qualified Generated.Proto.Dep.Crumb.Crumb       as GenCrumb
import qualified Generated.Proto.Dep.Crumb.Crumb.Cls   as CrumbCls
import qualified Generated.Proto.Dep.Crumb.Module      as CrumbModule
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs as GenBreadcrumbs
import           Batch.Breadcrumbs

import qualified Generated.Proto.Dep.Type.Type       as GenType
import qualified Generated.Proto.Dep.Type.Type.Cls   as TypeCls
import qualified Generated.Proto.Dep.Type.Unknown    as TypeUnknown

import qualified Generated.Proto.Dep.Name.Name       as GenName
import           Text.ProtocolBuffers.Basic          (uFromString)
import           Text.ProtocolBuffers.Extensions     (ExtField(..), putExt)
import qualified Data.Sequence                       as Seq
import qualified Data.Map                            as Map

emptyFunctionExpr :: Gen.Expr
emptyFunctionExpr  = putExt GenFunction.ext (Just emptyFunction)
                   $ Gen.Expr GenCls.Function Nothing $ ExtField Map.empty

emptyFunction :: GenFunction.Function
emptyFunction  = GenFunction.Function Seq.empty
                                      (Just mainName)
                                      Seq.empty
                                      (Just unknownType)
                                      Seq.empty

mainName :: GenName.Name
mainName  = GenName.Name (Just $ uFromString "main") Seq.empty

moduleCrumbExt :: String -> CrumbModule.Module
moduleCrumbExt name = CrumbModule.Module $ Just $ uFromString name

moduleCrumb :: String -> GenCrumb.Crumb
moduleCrumb name = putExt CrumbModule.ext (Just $ moduleCrumbExt name)
                 $ GenCrumb.Crumb CrumbCls.Module $ ExtField Map.empty

moduleBreadcrumbs :: String -> Breadcrumbs
moduleBreadcrumbs name = Breadcrumbs $ GenBreadcrumbs.Breadcrumbs $ Seq.fromList [moduleCrumb name]

unknownType :: GenType.Type
unknownType  = putExt TypeUnknown.ext (Just TypeUnknown.Unknown)
             $ GenType.Type TypeCls.Unknown Nothing $ ExtField Map.empty
