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
import qualified Generated.Proto.Dep.Type.Tuple      as TypeTuple

import qualified Generated.Proto.Dep.Name.Name       as GenName
import           Text.ProtocolBuffers.Basic          (uFromString)
import           Text.ProtocolBuffers.Extensions     (ExtField(..), putExt)
import qualified Data.Sequence                       as Seq
import qualified Data.Map                            as Map

emptyFunction :: String -> GenFunction.Function
emptyFunction name = GenFunction.Function Seq.empty
                                          (Just $ wrapName name)
                                          Seq.empty
                                          (Just returnType)
                                          Seq.empty

emptyFunctionExpr :: String -> Gen.Expr
emptyFunctionExpr name = putExt GenFunction.ext (Just $ emptyFunction name)
                       $ Gen.Expr GenCls.Function Nothing $ ExtField Map.empty

wrapName :: String -> GenName.Name
wrapName name = GenName.Name (Just $ uFromString name) Seq.empty

returnType :: GenType.Type
returnType  = putExt TypeTuple.ext (Just $ TypeTuple.Tuple Seq.empty)
             $ GenType.Type TypeCls.Tuple Nothing $ ExtField Map.empty
