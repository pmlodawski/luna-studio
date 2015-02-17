---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.DEP.Data.Serialize.Proto.Conversion.Expr where

import           Control.Applicative
import qualified Data.Foldable       as F
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Sequence       as Seq

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude                               hiding (cons)
import qualified Generated.Proto.Dep.Expr.Accessor             as GenAccessor
import qualified Generated.Proto.Dep.Expr.Accessor.Cls         as GenAccessorCls
import qualified Generated.Proto.Dep.Expr.App                  as GenApp
import qualified Generated.Proto.Dep.Expr.Arg                  as GenArg
import qualified Generated.Proto.Dep.Expr.Arg_                 as Gen
import qualified Generated.Proto.Dep.Expr.Arg_.Cls             as GenClsArg
import qualified Generated.Proto.Dep.Expr.Assignment           as GenAssignment
import qualified Generated.Proto.Dep.Expr.Case                 as GenCase
import qualified Generated.Proto.Dep.Expr.Con_                 as GenCon_
import qualified Generated.Proto.Dep.Expr.ConD                 as GenConD
import qualified Generated.Proto.Dep.Expr.Condition            as GenCondition
import qualified Generated.Proto.Dep.Expr.Data                 as GenData
import qualified Generated.Proto.Dep.Expr.DataNative           as GenDataNative
import qualified Generated.Proto.Dep.Expr.Expr                 as Gen
import qualified Generated.Proto.Dep.Expr.Expr.Cls             as GenCls
import qualified Generated.Proto.Dep.Expr.Field                as GenField
import qualified Generated.Proto.Dep.Expr.Function             as GenFunction
import qualified Generated.Proto.Dep.Expr.FuncVar              as GenFuncVar
import qualified Generated.Proto.Dep.Expr.Grouped              as GenGrouped
import qualified Generated.Proto.Dep.Expr.Import               as GenImport
import qualified Generated.Proto.Dep.Expr.ImportNative         as GenImportNative
import qualified Generated.Proto.Dep.Expr.Infix                as GenInfix
import qualified Generated.Proto.Dep.Expr.Lambda               as GenLambda
import qualified Generated.Proto.Dep.Expr.List                 as GenList
import qualified Generated.Proto.Dep.Expr.Lit                  as GenLit
import qualified Generated.Proto.Dep.Expr.Match                as GenMatch
import qualified Generated.Proto.Dep.Expr.Named                as GenNamed
import qualified Generated.Proto.Dep.Expr.Native               as GenNative
import qualified Generated.Proto.Dep.Expr.NativeCode           as GenNativeCode
import qualified Generated.Proto.Dep.Expr.NativeVar            as GenNativeVar
import qualified Generated.Proto.Dep.Expr.NOP                  as GenNOP
import qualified Generated.Proto.Dep.Expr.RangeFrom            as GenRangeFrom
import qualified Generated.Proto.Dep.Expr.RangeFromTo          as GenRangeFromTo
import qualified Generated.Proto.Dep.Expr.RecordUpdate         as GenRecordUpdate
import qualified Generated.Proto.Dep.Expr.Ref                  as GenRef
import qualified Generated.Proto.Dep.Expr.RefType              as GenRefType
import qualified Generated.Proto.Dep.Expr.Tuple                as GenTuple
import qualified Generated.Proto.Dep.Expr.TypeAlias            as GenTypeAlias
import qualified Generated.Proto.Dep.Expr.Typed                as GenTyped
import qualified Generated.Proto.Dep.Expr.TypeDef              as GenTypeDef
import qualified Generated.Proto.Dep.Expr.Unnamed              as GenUnnamed
import qualified Generated.Proto.Dep.Expr.Var                  as GenVar
import qualified Generated.Proto.Dep.Expr.Wildcard             as GenWildcard
import           Luna.DEP.AST.Arg                              (Arg)
import qualified Luna.DEP.AST.Arg                              as Arg
import qualified Luna.DEP.AST.AST                              as AST
import           Luna.DEP.AST.Expr                             (Expr)
import qualified Luna.DEP.AST.Expr                             as Expr
import           Luna.DEP.Data.Serialize.Proto.Conversion.Name ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Pat  ()
import qualified Text.ProtocolBuffers.Extensions               as Extensions



instance Convert Expr Gen.Expr where
    encode t = case t of
        Expr.Accessor   i acc' dst -> do let (cls, acc) = case acc' of
                                                Expr.ConAccessor acc_ -> (GenAccessorCls.Con, acc_)
                                                Expr.VarAccessor acc_ -> (GenAccessorCls.Var, acc_)
                                         genExpr GenCls.Accessor i GenAccessor.ext $ GenAccessor.Accessor
                                            cls (encodePJ acc) (encodeJ dst)
        Expr.App        i src args -> genExpr GenCls.App i GenApp.ext $ GenApp.App
                                      (encodeJ src) (encode args)
        Expr.Arg        i pat value
                                   -> genExpr GenCls.Arg i GenArg.ext $ GenArg.Arg
                                      (encodeJ pat) (fmap encode value)
        Expr.Assignment i pat dst  -> genExpr GenCls.Assignment i GenAssignment.ext $ GenAssignment.Assignment
                                      (encodeJ pat) (encodeJ dst)
        Expr.Case  i expr match    -> genExpr GenCls.Case i GenCase.ext $ GenCase.Case
                                      (encodeJ expr) (encode match)
        Expr.Con        i name     -> genExpr GenCls.Con_ i GenCon_.ext $ GenCon_.Con_
                                      (encodePJ name)
        Expr.ConD       i name fields
                                   -> genExpr GenCls.ConD i GenConD.ext $ GenConD.ConD
                                      (encodePJ name) (encode fields)
        Expr.Cond       i cond success mfailure
                                   -> genExpr GenCls.Condition i GenCondition.ext $ GenCondition.Condition
                                      (encodeJ cond) (encode success) (encode $ F.concat mfailure)
        Expr.Data       i cls cons classes methods
                                   -> genExpr GenCls.Data i GenData.ext $ GenData.Data
                                      (encodeJ cls) (encode cons) (encode classes) (encode methods)
        Expr.DataNative i cls cons classes methods
                                   -> genExpr GenCls.DataNative i GenDataNative.ext $ GenDataNative.DataNative
                                      (encodeJ cls) (encode cons) (encode classes) (encode methods)
        Expr.Field      i name cls value
                                   -> genExpr GenCls.Field i GenField.ext $ GenField.Field
                                      (encodePJ name) (encodeJ cls) (fmap encode value)
        Expr.Function   i path name inputs output body
                                   -> genExpr GenCls.Function i GenFunction.ext $ GenFunction.Function
                                      (encodeP path) (encodeJ name) (encode inputs) (encodeJ output) (encode body)
        Expr.Grouped    i expr     -> genExpr GenCls.Grouped i GenGrouped.ext $ GenGrouped.Grouped
                                      (encodeJ expr)
        Expr.Import     i path target rename
                                   -> genExpr GenCls.Import i GenImport.ext $ GenImport.Import
                                      (encodeP path) (encodeJ target) (fmap encodeP rename)
        Expr.ImportNative i segments
                                   -> genExpr GenCls.ImportNative i GenImportNative.ext $ GenImportNative.ImportNative
                                      (encode segments)
        Expr.Infix      i name src dst
                                   -> genExpr GenCls.Infix i GenInfix.ext $ GenInfix.Infix
                                      (encodePJ name) (encodeJ src) (encodeJ dst)
        Expr.Lambda     i inputs output body
                                   -> genExpr GenCls.Lambda i GenLambda.ext $ GenLambda.Lambda
                                      (encode inputs) (encodeJ output) (encode body)
        Expr.Match i pat body      -> genExpr GenCls.Match i GenMatch.ext $ GenMatch.Match
                                      (encodeJ pat) (encode body)
        Expr.Native     i segments -> genExpr GenCls.Native i GenNative.ext $ GenNative.Native
                                      (encode segments)
        Expr.NativeCode i code     -> genExpr GenCls.NativeCode i GenNativeCode.ext $ GenNativeCode.NativeCode
                                      (encodePJ code)
        Expr.NativeVar  i name     -> genExpr GenCls.NativeVar i GenNativeVar.ext $ GenNativeVar.NativeVar
                                      (encodePJ name)
        Expr.List       i items    -> genExpr GenCls.List i GenList.ext $ GenList.List
                                      (encode items)
        Expr.Lit        i lvalue   -> genExpr GenCls.Lit i GenLit.ext $ GenLit.Lit
                                      (encodeJ lvalue)
        Expr.NOP        i          -> genExpr GenCls.NOP i GenNOP.ext GenNOP.NOP
        Expr.RecordUpdate i src selectors expr
                                   -> genExpr GenCls.RecordUpdate i GenRecordUpdate.ext $ GenRecordUpdate.RecordUpdate
                                      (encodeJ src) (encodeP selectors) (encodeJ expr)
        Expr.RangeFrom  i start    -> genExpr GenCls.RangeFrom i GenRangeFrom.ext $ GenRangeFrom.RangeFrom
                                      (encodeJ start)
        Expr.RangeFromTo i start end
                                   -> genExpr GenCls.RangeFromTo i GenRangeFromTo.ext $ GenRangeFromTo.RangeFromTo
                                      (encodeJ start) (encodeJ end)
        Expr.Ref        i dst      -> genExpr GenCls.Ref i GenRef.ext $ GenRef.Ref
                                      (encodeJ dst)
        Expr.RefType    i typename name
                                   -> genExpr GenCls.RefType i GenRefType.ext $ GenRefType.RefType
                                      (encodePJ typename) (encodePJ name)
        Expr.Tuple      i items    -> genExpr GenCls.Tuple i GenTuple.ext $ GenTuple.Tuple
                                      (encode items)
        Expr.TypeAlias  i srcType dstType
                                   -> genExpr GenCls.TypeAlias i GenTypeAlias.ext $ GenTypeAlias.TypeAlias
                                      (encodeJ srcType) (encodeJ dstType)
        Expr.Typed      i cls expr -> genExpr GenCls.Typed i GenTyped.ext $ GenTyped.Typed
                                      (encodeJ cls) (encodeJ expr)
        Expr.TypeDef    i srcType dstType
                                   -> genExpr GenCls.TypeDef i GenTypeDef.ext $ GenTypeDef.TypeDef
                                      (encodeJ srcType) (encodeJ dstType)
        Expr.Var        i name     -> genExpr GenCls.Var i GenVar.ext $ GenVar.Var
                                      (encodePJ name)
        Expr.FuncVar    i fname    -> genExpr GenCls.FuncVar i GenFuncVar.ext $ GenFuncVar.FuncVar
                                      (encodeJ fname)
        Expr.Wildcard   i          -> genExpr GenCls.Wildcard i GenWildcard.ext GenWildcard.Wildcard
        where
            genExpr :: GenCls.Cls -> AST.ID -> Extensions.Key Maybe Gen.Expr v -> v -> Gen.Expr
            genExpr cls i key ext = Extensions.putExt key (Just ext)
                                  $ Gen.Expr cls (encodePJ i) $ Extensions.ExtField Map.empty

    decode t@(Gen.Expr cls_ mtid _) = do
        i <- decodePJ mtid (missing "Expr" "id")
        case cls_ of
            GenCls.Accessor -> do
                GenAccessor.Accessor aclst name dst <- getExt GenAccessor.ext "Expr.Accessor"
                let acls = case aclst of
                        GenAccessorCls.Con -> Expr.ConAccessor
                        GenAccessorCls.Var -> Expr.VarAccessor
                Expr.Accessor i <$> (acls <$> decodePJ name (missing "Expr.Accessor" "name"))
                                <*> decodeJ  dst  (missing "Expr.Accessor" "dst")
            GenCls.App -> do
                GenApp.App src args <- getExt GenApp.ext "Expr.App"
                Expr.App i <$> decodeJ src (missing "Expr.App" "src") <*> decode args
            GenCls.Arg -> do
                GenArg.Arg pat value <- getExt GenArg.ext "Expr.Arg"
                Expr.Arg i <$> decodeJ pat (missing "Expr.Arg" "pat")
                           <*> Maybe.maybe (pure Nothing) (fmap Just . decode) value
            GenCls.Assignment -> do
                GenAssignment.Assignment pat dst <- getExt GenAssignment.ext "Expr.Assignment"
                Expr.Assignment i <$> decodeJ pat (missing "Expr.Assignment" "pat")
                                  <*> decodeJ dst (missing "Expr.Assignment" "dst")
            GenCls.Case -> do
                GenCase.Case expr match <- getExt GenCase.ext "Expr.Case"
                Expr.Case i <$> decodeJ expr (missing "Expr.Case" "expr")
                            <*> decode match
            GenCls.Con_ -> do
                GenCon_.Con_ name <- getExt GenCon_.ext "Expr.Con"
                Expr.Con i <$> decodePJ name (missing "Expr.Con" "name")
            GenCls.ConD -> do
                GenConD.ConD name fields <- getExt GenConD.ext "Expr.ConD"
                Expr.ConD i <$> decodePJ name (missing "Expr.ConD" "name") <*> decode fields
            GenCls.Condition -> do
                GenCondition.Condition cond success failure <- getExt GenCondition.ext "Expr.Condition"
                Expr.Cond i <$> decodeJ cond (missing "Expr.Condition" "cond")
                            <*> decode success <*> if Seq.null failure
                                                            then pure Nothing
                                                            else Just <$> decode failure
            GenCls.Data -> do
                GenData.Data cls cons classes methods <- getExt GenData.ext "Expr.Data"
                Expr.Data i <$> decodeJ cls (missing "Expr.Data" "name")
                            <*> decode cons <*> decode classes <*> decode methods
            GenCls.DataNative -> do
                GenDataNative.DataNative cls cons classes methods <- getExt GenDataNative.ext "Expr.DataNative"
                Expr.DataNative i <$> decodeJ cls (missing "Expr.DataNative" "name")
                            <*> decode cons <*> decode classes <*> decode methods
            GenCls.Field -> do
                GenField.Field name cls value <- getExt GenField.ext "Expr.Field"
                Expr.Field i <$> decodePJ name (missing "Expr.Field" "name")
                             <*> decodeJ  cls  (missing "Expr.Field" "cls" )
                             <*> Maybe.maybe (pure Nothing) (fmap Just . decode) value
            GenCls.Function -> do
                GenFunction.Function path name inputs output body <- getExt GenFunction.ext "Expr.Function"
                Expr.Function i (decodeP path) <$> decodeJ    name (missing "Expr.Function" "name")
                                                   <*> decode inputs
                                                   <*> decodeJ    output (missing "Expr.Function" "output")
                                                   <*> decode body
            GenCls.Grouped -> do
                GenGrouped.Grouped expr <- getExt GenGrouped.ext "Expr.Grouped"
                Expr.Grouped i <$> decodeJ expr (missing "Expr.Grouped" "expr")
            GenCls.Import -> do
                GenImport.Import path target rename <- getExt GenImport.ext "Expr.Import"
                Expr.Import i (decodeP path) <$> decodeJ target (missing "Expr.Import" "target")
                                                 <*> pure (fmap decodeP rename)
            GenCls.ImportNative -> do
                GenImportNative.ImportNative tsegments <- getExt GenImportNative.ext "Expr.ImportNative"
                Expr.ImportNative i <$> decode tsegments
            GenCls.Infix -> do
                GenInfix.Infix name src dst <- getExt GenInfix.ext "Expr.Infix"
                Expr.Infix i <$> decodePJ name (missing "Expr.Infix" "name")
                             <*> decodeJ  src  (missing "Expr.Infix" "src")
                             <*> decodeJ  dst  (missing "Expr.Infix" "dst")
            GenCls.Lambda -> do
                GenLambda.Lambda inputs output body <- getExt GenLambda.ext "Expr.Lambda"
                Expr.Lambda i <$> decode inputs
                              <*> decodeJ output (missing "Expr.Lambda" "output")
                              <*> decode body
            GenCls.List -> do
                GenList.List titems <- getExt GenList.ext "Expr.List"
                Expr.List i <$> decode titems
            GenCls.Lit -> do
                GenLit.Lit lit <- getExt GenLit.ext "Expr.Lit"
                Expr.Lit i <$> decodeJ lit (missing "Expr.Lit" "lit")
            GenCls.Match -> do
                GenMatch.Match pat body <- getExt GenMatch.ext "Expr.Match"
                Expr.Match i <$> decodeJ pat (missing "Expr.Match" "pat")
                             <*> decode body
            GenCls.Native -> do
                GenNative.Native tsegments <- getExt GenNative.ext "Expr.Native"
                Expr.Native i <$> decode tsegments
            GenCls.NativeCode -> do
                GenNativeCode.NativeCode code <- getExt GenNativeCode.ext "Expr.NativeCode"
                Expr.NativeCode i <$> decodePJ code (missing "Expr.NativeCode" "code")
            GenCls.NativeVar -> do
                GenNativeVar.NativeVar name <- getExt GenNativeVar.ext "Expr.NativeVar"
                Expr.NativeVar i <$> decodePJ name (missing "Expr.NativeVar" "name")
            GenCls.NOP -> do
                GenNOP.NOP <- getExt GenNOP.ext "Expr.NOP"
                pure $ Expr.NOP i
            GenCls.RangeFrom -> do
                GenRangeFrom.RangeFrom start <- getExt GenRangeFrom.ext "Expr.RangeFrom"
                Expr.RangeFrom i <$> decodeJ start (missing "Expr.RangeFrom" "start")
            GenCls.RangeFromTo -> do
                GenRangeFromTo.RangeFromTo start end <- getExt GenRangeFromTo.ext "Expr.RangeFromTo"
                Expr.RangeFromTo i <$> decodeJ start (missing "Expr.RangeFromTo" "start")
                                   <*> decodeJ end   (missing "Expr.RangeFromTo" "end")
            GenCls.RecordUpdate -> do
                GenRecordUpdate.RecordUpdate src selectors expr <- getExt GenRecordUpdate.ext "Expr.RecordUpdate"
                Expr.RecordUpdate i <$> decodeJ src  (missing "Expr.RecordUpdate" "src")
                                    <*> pure (decodeP selectors)
                                    <*> decodeJ expr (missing "Expr.RecordUpdate" "expr")
            GenCls.Ref -> do
                GenRef.Ref dst <- getExt GenRef.ext "Expr.Ref"
                Expr.Ref i <$> decodeJ dst (missing "Expr.Ref" "dst")
            GenCls.RefType -> do
                GenRefType.RefType typename name <- getExt GenRefType.ext "Expr.RefType"
                Expr.RefType i <$> decodePJ typename (missing "Expr.RefType" "typename")
                               <*> decodePJ name     (missing "Expr.RefType" "name")
            GenCls.Tuple -> do
                GenTuple.Tuple titems <- getExt GenTuple.ext "Expr.Tuple"
                Expr.Tuple i <$> decode titems
            GenCls.TypeAlias -> do
                GenTypeAlias.TypeAlias srcType dstType <- getExt GenTypeAlias.ext "Expr.TypeAlias"
                Expr.TypeAlias i <$> decodeJ srcType (missing "Expr.TypeAlias" "srcType")
                                 <*> decodeJ dstType (missing "Expr.TypeAlias" "dstType")
            GenCls.TypeDef -> do
                GenTypeDef.TypeDef srcType dstType <- getExt GenTypeDef.ext "Expr.TypeDef"
                Expr.TypeDef i <$> decodeJ srcType (missing "Expr.TypeDef" "srcType")
                               <*> decodeJ dstType (missing "Expr.TypeDef" "dstType")
            GenCls.Typed -> do
                GenTyped.Typed cls expr <- getExt GenTyped.ext "Expr.Typed"
                Expr.Typed i <$> decodeJ cls  (missing "Expr.Typed" "cls")
                             <*> decodeJ expr (missing "Expr.Typed" "expr")
            GenCls.Var -> do
                GenVar.Var name <- getExt GenVar.ext "Expr.Var"
                Expr.Var i <$> decodePJ name (missing "Expr.Var" "name")
            GenCls.FuncVar -> do
                GenFuncVar.FuncVar fname <- getExt GenFuncVar.ext "Expr.FuncVar"
                Expr.FuncVar i <$> decodeJ fname (missing "Expr.FuncVar" "fname")
            GenCls.Wildcard -> do
                GenWildcard.Wildcard <- getExt GenWildcard.ext "Expr.Wildcard"
                pure $ Expr.Wildcard i
       where getExt key datatype = Extensions.getExt key t <?&> missing datatype "extension"


instance Convert (Arg Expr) Gen.Arg_ where
    encode crumb = case crumb of
        Arg.Named i name arg -> genArg GenClsArg.Named   i GenNamed.ext   $ GenNamed.Named     (encodePJ name) (encodeJ arg)
        Arg.Unnamed i arg    -> genArg GenClsArg.Unnamed i GenUnnamed.ext $ GenUnnamed.Unnamed (encodeJ arg)
        where
            genArg :: GenClsArg.Cls -> Int -> Extensions.Key Maybe Gen.Arg_ v -> v -> Gen.Arg_
            genArg cls i key ext = Extensions.putExt key (Just ext)
                                 $ Gen.Arg_ cls (encodePJ i) $ Extensions.ExtField Map.empty
    decode t@(Gen.Arg_ cls ti _) = do
        i <- decodePJ ti (missing "Arg" "id")
        case cls of
            GenClsArg.Named -> do
                GenNamed.Named name arg <- getExt GenNamed.ext "Arg.Named"
                Arg.Named i <$> decodePJ name (missing "Arg.Named" "name")
                            <*> decodeJ  arg  (missing "Arg.Named" "arg")
            GenClsArg.Unnamed -> do
                GenUnnamed.Unnamed arg <- getExt GenUnnamed.ext "Arg.Unnamed"
                Arg.Unnamed i <$> decodeJ  arg  (missing "Arg.Named" "arg")
       where getExt key datatype = Extensions.getExt key t <?&> missing datatype "extension"

