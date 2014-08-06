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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr where

import           Control.Applicative
import qualified Data.Foldable                                     as F
import qualified Data.Map                                          as Map
import qualified Data.Sequence                                     as Seq
import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Common                      as AST
import           Flowbox.Luna.Data.AST.Expr                        (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Pat ()
import           Flowbox.Prelude                                   hiding (cons)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Expr.Accessor                     as GenAccessor
import qualified Generated.Proto.Expr.App                          as GenApp
import qualified Generated.Proto.Expr.AppCons_                     as GenAppCons_
import qualified Generated.Proto.Expr.Arg                          as GenArg
import qualified Generated.Proto.Expr.Assignment                   as GenAssignment
import qualified Generated.Proto.Expr.Case                         as GenCase
import qualified Generated.Proto.Expr.Con_                         as GenCon_
import qualified Generated.Proto.Expr.ConD                         as GenConD
import qualified Generated.Proto.Expr.Condition                    as GenCondition
import qualified Generated.Proto.Expr.Data                         as GenData
import qualified Generated.Proto.Expr.Expr                         as Gen
import qualified Generated.Proto.Expr.Expr.Cls                     as GenCls
import qualified Generated.Proto.Expr.Field                        as GenField
import qualified Generated.Proto.Expr.Function                     as GenFunction
import qualified Generated.Proto.Expr.Grouped                      as GenGrouped
import qualified Generated.Proto.Expr.Import                       as GenImport
import qualified Generated.Proto.Expr.ImportNative                 as GenImportNative
import qualified Generated.Proto.Expr.Infix                        as GenInfix
import qualified Generated.Proto.Expr.Lambda                       as GenLambda
import qualified Generated.Proto.Expr.List                         as GenList
import qualified Generated.Proto.Expr.Lit                          as GenLit
import qualified Generated.Proto.Expr.Match                        as GenMatch
import qualified Generated.Proto.Expr.Native                       as GenNative
import qualified Generated.Proto.Expr.NativeCode                   as GenNativeCode
import qualified Generated.Proto.Expr.NativeVar                    as GenNativeVar
import qualified Generated.Proto.Expr.NOP                          as GenNOP
import qualified Generated.Proto.Expr.RangeFrom                    as GenRangeFrom
import qualified Generated.Proto.Expr.RangeFromTo                  as GenRangeFromTo
import qualified Generated.Proto.Expr.RecordUpdate                 as GenRecordUpdate
import qualified Generated.Proto.Expr.Ref                          as GenRef
import qualified Generated.Proto.Expr.RefType                      as GenRefType
import qualified Generated.Proto.Expr.Tuple                        as GenTuple
import qualified Generated.Proto.Expr.TypeAlias                    as GenTypeAlias
import qualified Generated.Proto.Expr.Typed                        as GenTyped
import qualified Generated.Proto.Expr.TypeDef                      as GenTypeDef
import qualified Generated.Proto.Expr.Var                          as GenVar
import qualified Generated.Proto.Expr.Wildcard                     as GenWildcard
import qualified Text.ProtocolBuffers.Extensions                   as Extensions



instance Convert Expr Gen.Expr where
    encode t = case t of
        Expr.NOP        i          -> genExpr GenCls.NOP i GenNOP.ext GenNOP.NOP
        Expr.Accessor   i name dst -> genExpr GenCls.Accessor i GenAccessor.ext $ GenAccessor.Accessor
                                      (encodePJ name) (encodeJ dst)
        Expr.TypeAlias  i srcType dstType
                                   -> genExpr GenCls.TypeAlias i GenTypeAlias.ext $ GenTypeAlias.TypeAlias
                                      (encodeJ srcType) (encodeJ dstType)
        Expr.TypeDef    i srcType dstType
                                   -> genExpr GenCls.TypeDef i GenTypeDef.ext $ GenTypeDef.TypeDef
                                      (encodeJ srcType) (encodeJ dstType)
        Expr.App        i src args -> genExpr GenCls.App i GenApp.ext $ GenApp.App
                                      (encodeJ src) (encodeList args)
        Expr.AppCons_   i args     -> genExpr GenCls.AppCons_ i GenAppCons_.ext $ GenAppCons_.AppCons_
                                      (encodeList args)
        Expr.Assignment i pat dst  -> genExpr GenCls.Assignment i GenAssignment.ext $ GenAssignment.Assignment
                                      (encodeJ pat) (encodeJ dst)
        Expr.RecordUpdate i src selectors expr
                                   -> genExpr GenCls.RecordUpdate i GenRecordUpdate.ext $ GenRecordUpdate.RecordUpdate
                                      (encodeJ src) (encodeListP selectors) (encodeJ expr)
        Expr.Data       i cls cons classes methods
                                   -> genExpr GenCls.Data i GenData.ext $ GenData.Data
                                      (encodeJ cls) (encodeList cons) (encodeList classes) (encodeList methods)
        Expr.ConD       i name fields
                                   -> genExpr GenCls.ConD i GenConD.ext $ GenConD.ConD
                                      (encodePJ name) (encodeList fields)
        Expr.Con        i name     -> genExpr GenCls.Con_ i GenCon_.ext $ GenCon_.Con_
                                      (encodePJ name)
        Expr.Cond       i cond success mfailure
                                   -> genExpr GenCls.Condition i GenCondition.ext $ GenCondition.Condition
                                      (encodeJ cond) (encodeList success) (encodeList $ F.concat mfailure)
        Expr.Function   i path name inputs output body
                                   -> genExpr GenCls.Function i GenFunction.ext $ GenFunction.Function
                                      (encodeListP path) (encodePJ name) (encodeList inputs) (encodeJ output) (encodeList body)
        Expr.Lambda     i inputs output body
                                   -> genExpr GenCls.Lambda i GenLambda.ext $ GenLambda.Lambda
                                      (encodeList inputs) (encodeJ output) (encodeList body)
        Expr.Grouped    i expr     -> genExpr GenCls.Grouped i GenGrouped.ext $ GenGrouped.Grouped
                                      (encodeJ expr)
        Expr.Import     i path target rename
                                   -> genExpr GenCls.Import i GenImport.ext $ GenImport.Import
                                      (encodeListP path) (encodeJ target) (fmap encodeP rename)
        Expr.ImportNative i segments
                                   -> genExpr GenCls.ImportNative i GenImportNative.ext $ GenImportNative.ImportNative
                                      (encodeList segments)
        Expr.Infix      i name src dst
                                   -> genExpr GenCls.Infix i GenInfix.ext $ GenInfix.Infix
                                      (encodePJ name) (encodeJ src) (encodeJ dst)
        Expr.List       i items    -> genExpr GenCls.List i GenList.ext $ GenList.List
                                      (encodeList items)
        Expr.Lit        i lvalue   -> genExpr GenCls.Lit i GenLit.ext $ GenLit.Lit
                                      (encodeJ lvalue)
        Expr.Tuple      i items    -> genExpr GenCls.Tuple i GenTuple.ext $ GenTuple.Tuple
                                      (encodeList items)
        Expr.Typed      i cls expr -> genExpr GenCls.Typed i GenTyped.ext $ GenTyped.Typed
                                      (encodeJ cls) (encodeJ expr)
        Expr.Var        i name     -> genExpr GenCls.Var i GenVar.ext $ GenVar.Var
                                      (encodePJ name)
        Expr.Wildcard   i          -> genExpr GenCls.Wildcard i GenWildcard.ext GenWildcard.Wildcard
        Expr.RangeFromTo i start end
                                   -> genExpr GenCls.RangeFromTo i GenRangeFromTo.ext $ GenRangeFromTo.RangeFromTo
                                      (encodeJ start) (encodeJ end)
        Expr.RangeFrom  i start    -> genExpr GenCls.RangeFrom i GenRangeFrom.ext $ GenRangeFrom.RangeFrom
                                      (encodeJ start)
        Expr.Field      i name cls value
                                   -> genExpr GenCls.Field i GenField.ext $ GenField.Field
                                      (encodePJ name) (encodeJ cls) (fmap encode value)
        Expr.Arg        i pat value
                                   -> genExpr GenCls.Arg i GenArg.ext $ GenArg.Arg
                                      (encodeJ pat) (fmap encode value)
        Expr.Native     i segments -> genExpr GenCls.Native i GenNative.ext $ GenNative.Native
                                      (encodeList segments)
        Expr.NativeCode i code     -> genExpr GenCls.NativeCode i GenNativeCode.ext $ GenNativeCode.NativeCode
                                      (encodePJ code)
        Expr.NativeVar  i name     -> genExpr GenCls.NativeVar i GenNativeVar.ext $ GenNativeVar.NativeVar
                                      (encodePJ name)
        Expr.Ref        i dst      -> genExpr GenCls.Ref i GenRef.ext $ GenRef.Ref
                                      (encodeJ dst)
        Expr.RefType    i typename name
                                   -> genExpr GenCls.RefType i GenRefType.ext $ GenRefType.RefType
                                      (encodePJ typename) (encodePJ name)
        Expr.Case  i expr match    -> genExpr GenCls.Case i GenCase.ext $ GenCase.Case
                                      (encodeJ expr) (encodeList match)
        Expr.Match i pat body      -> genExpr GenCls.Match i GenMatch.ext $ GenMatch.Match
                                      (encodeJ pat) (encodeList body)
        where
            genExpr :: GenCls.Cls -> AST.ID -> Extensions.Key Maybe Gen.Expr v -> v -> Gen.Expr
            genExpr cls i key ext = Extensions.putExt key (Just ext)
                                  $ Gen.Expr cls (encodePJ i) $ Extensions.ExtField Map.empty

    decode t@(Gen.Expr cls mtid _) = do
        i <- decodeP <$> mtid <?> "Failed to decode Expr: 'id' field is missing"
        case cls of
            GenCls.NOP -> do
                ext <- getExt GenNOP.ext
                GenNOP.NOP <- ext <?> "Failed to decode Expr.NOP: extension is missing"
                pure $ Expr.NOP i
            GenCls.Accessor -> do
                ext <- getExt GenAccessor.ext
                (GenAccessor.Accessor mtname mtdst) <- ext <?> "Failed to decode Expr.Accessor: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.Accessor: 'name' field is missing"
                tdst  <- mtdst  <?> "Failed to decode Expr.Accessor: 'dst' field is missing"
                Expr.Accessor i (decodeP tname) <$> decode tdst
            GenCls.TypeAlias -> do
                ext <- getExt GenTypeAlias.ext
                (GenTypeAlias.TypeAlias mtsrcType mtdstType) <- ext <?> "Failed to decode Expr.TypeAlias: extension is missing"
                tsrcType <- mtsrcType  <?> "Failed to decode Expr.TypeAlias: 'srcType' field is missing"
                tdstType  <- mtdstType <?> "Failed to decode Expr.TypeAlias: 'dstType' field is missing"
                Expr.TypeAlias i <$> decode tsrcType <*> decode tdstType
            GenCls.TypeDef -> do
                ext <- getExt GenTypeDef.ext
                (GenTypeDef.TypeDef mtsrcType mtdstType) <- ext <?> "Failed to decode Expr.TypeDef: extension is missing"
                tsrcType <- mtsrcType  <?> "Failed to decode Expr.TypeDef: 'srcType' field is missing"
                tdstType  <- mtdstType <?> "Failed to decode Expr.TypeDef: 'dstType' field is missing"
                Expr.TypeDef i <$> decode tsrcType <*> decode tdstType
            GenCls.App -> do
                ext <- getExt GenApp.ext
                (GenApp.App mtsrc targs) <- ext <?> "Failed to decode Expr.App: extension is missing"
                tsrc <- mtsrc <?> "Failed to decode Expr.App: 'src' field is missing"
                Expr.App i <$> decode tsrc <*> decodeList targs
            GenCls.AppCons_ -> do
                ext <- getExt GenAppCons_.ext
                (GenAppCons_.AppCons_ targs) <- ext <?> "Failed to decode Expr.AppCons_: extension is missing"
                Expr.AppCons_ i <$> decodeList targs
            GenCls.Assignment -> do
                ext <- getExt GenAssignment.ext
                (GenAssignment.Assignment mtpat mtdst) <- ext <?> "Failed to decode Expr.Assignment: extension is missing"
                tpat <- mtpat <?> "Failed to decode Expr.Assignment: 'pat' field is missing"
                tdst <- mtdst <?> "Failed to decode Expr.Assignment: 'dst' field is missing"
                Expr.Assignment i <$> decode tpat <*> decode tdst
            GenCls.RecordUpdate -> do
                ext <- getExt GenRecordUpdate.ext
                (GenRecordUpdate.RecordUpdate mtsrc tselectors mtexpr) <- ext <?> "Failed to decode Expr.RecordUpdate: extension is missing"
                tsrc  <- mtsrc  <?> "Failed to decode Expr.RecordUpdate: 'src' field is missing"
                texpr <- mtexpr <?> "Failed to decode Expr.RecordUpdate: 'expr' field is missing"
                Expr.RecordUpdate i <$> decode tsrc <*> pure (decodeListP tselectors) <*> decode texpr
            GenCls.Data -> do
                ext <- getExt GenData.ext
                (GenData.Data mtcls tcons tclasses tmethods) <- ext <?> "Failed to decode Expr.Data: extension is missing"
                tcls <- mtcls <?> "Failed to decode Expr.Data: 'name' field is missing"
                Expr.Data i <$> decode tcls <*> decodeList tcons <*> decodeList tclasses <*> decodeList tmethods
            GenCls.ConD -> do
                ext <- getExt GenConD.ext
                (GenConD.ConD mtname tfields) <- ext <?> "Failed to decode Expr.ConD: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.ConD: 'name' field is missing"
                Expr.ConD i (decodeP tname) <$> decodeList tfields
            GenCls.Con_ -> do
                ext <- getExt GenCon_.ext
                (GenCon_.Con_ mtname) <- ext <?> "Failed to decode Expr.Con: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.Con: 'name' field is missing"
                pure $ Expr.Con i (decodeP tname)
            GenCls.Condition -> do
                ext <- getExt GenCondition.ext
                (GenCondition.Condition mtcond tsuccess tfailure) <- ext <?> "Failed to decode Expr.Condition: extension is missing"
                tcond <- mtcond <?> "Failed to decode Expr.Condition: 'cond' field is missing"
                Expr.Cond i <$> decode tcond <*> decodeList tsuccess <*> if Seq.null tfailure
                                                                            then pure Nothing
                                                                            else Just <$> decodeList tfailure
            GenCls.Function -> do
                ext <- getExt GenFunction.ext
                (GenFunction.Function tpath mtname tinputs mtoutput tbody) <- ext <?> "Failed to decode Expr.Function: extension is missing"
                tname   <- mtname   <?> "Failed to decode Expr.Function: 'name' field is missing"
                toutput <- mtoutput <?> "Failed to decode Expr.Function: 'output' field is missing"
                Expr.Function i (decodeListP tpath) (decodeP tname) <$> decodeList tinputs <*> decode toutput <*> decodeList tbody
            GenCls.Lambda -> do
                ext <- getExt GenLambda.ext
                (GenLambda.Lambda tinputs mtoutput tbody) <- ext <?> "Failed to decode Expr.Lambda: extension is missing"
                toutput <- mtoutput <?> "Failed to decode Expr.Lambda: 'output' field is missing"
                Expr.Lambda i <$> decodeList tinputs <*> decode toutput <*> decodeList tbody
            GenCls.Grouped -> do
                ext <- getExt GenGrouped.ext
                (GenGrouped.Grouped mtexpr) <- ext <?> "Failed to decode Expr.Grouped: extension is missing"
                texpr <- mtexpr <?> "Failed to decode Expr.Grouped: 'expr' field is missing"
                Expr.Grouped i <$> decode texpr
            GenCls.Import -> do
                ext <- getExt GenImport.ext
                (GenImport.Import tpath mttarget mtrename) <- ext <?> "Failed to decode Expr.Import: extension is missing"
                ttarget <- mttarget <?> "Failed to decode Expr.Import: 'target' field is missing"
                Expr.Import i (decodeListP tpath) <$> decode ttarget
                                                              <*> pure (fmap decodeP mtrename)
            GenCls.ImportNative -> do
                ext <- getExt GenImportNative.ext
                (GenImportNative.ImportNative tsegments) <- ext <?> "Failed to decode Expr.ImportNative: extension is missing"
                Expr.ImportNative i <$> decodeList tsegments
            GenCls.Infix -> do
                ext <- getExt GenInfix.ext
                (GenInfix.Infix mtname mtsrc mtdst) <- ext <?> "Failed to decode Expr.Infix: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.Infix: 'name' field is missing"
                tsrc  <- mtsrc  <?> "Failed to decode Expr.Infix: 'src' field is missing"
                tdst  <- mtdst  <?> "Failed to decode Expr.Infix: 'dst' field is missing"
                Expr.Infix i (decodeP tname) <$> decode tsrc <*> decode tdst
            GenCls.List -> do
                ext <- getExt GenList.ext
                (GenList.List titems) <- ext <?> "Failed to decode Expr.List: extension is missing"
                Expr.List i <$> decodeList titems
            GenCls.Lit -> do
                ext <- getExt GenLit.ext
                (GenLit.Lit mtlit) <- ext <?> "Failed to decode Expr.Lit: extension is missing"
                tlit <- mtlit <?> "Failed to decode Expr.Lit: 'lit' field is missing"
                Expr.Lit i <$> decode tlit
            GenCls.Tuple -> do
                ext <- getExt GenTuple.ext
                (GenTuple.Tuple titems) <- ext <?> "Failed to decode Expr.Tuple: extension is missing"
                Expr.Tuple i <$> decodeList titems
            GenCls.Typed -> do
                ext <- getExt GenTyped.ext
                (GenTyped.Typed mtcls mtexpr) <- ext <?> "Failed to decode Expr.Typed: extension is missing"
                tcls  <- mtcls  <?> "Failed to decode Expr.Typed: 'cls' field is missing"
                texpr <- mtexpr <?> "Failed to decode Expr.Typed: 'expr' field is missing"
                Expr.Typed i <$> decode tcls <*> decode texpr
            GenCls.Var -> do
                ext <- getExt GenVar.ext
                (GenVar.Var mtname) <- ext <?> "Failed to decode Expr.Var: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.Var: 'name' field is missing"
                pure $ Expr.Var i (decodeP tname)
            GenCls.Wildcard -> do
                ext <- getExt GenWildcard.ext
                GenWildcard.Wildcard <- ext <?> "Failed to decode Expr.Wildcard: extension is missing"
                pure $ Expr.Wildcard i
            GenCls.RangeFromTo -> do
                ext <- getExt GenRangeFromTo.ext
                (GenRangeFromTo.RangeFromTo mtstart mtend) <- ext <?> "Failed to decode Expr.RangeFromTo: extension is missing"
                tstart <- mtstart <?> "Failed to decode Expr.RangeFromTo: 'start' field is missing"
                tend   <- mtend   <?> "Failed to decode Expr.RangeFromTo: 'end' field is missing"
                Expr.RangeFromTo i <$> decode tstart <*> decode tend
            GenCls.RangeFrom -> do
                ext <- getExt GenRangeFrom.ext
                (GenRangeFrom.RangeFrom mtstart) <- ext <?> "Failed to decode Expr.RangeFrom: extension is missing"
                tstart <- mtstart <?> "Failed to decode Expr.RangeFrom: 'start' field is missing"
                Expr.RangeFrom i <$> decode tstart
            GenCls.Field -> do
                ext <- getExt GenField.ext
                (GenField.Field mtname mtcls mtvalue) <- ext <?> "Failed to decode Expr.Field: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.Field: 'name' field is missing"
                tcls  <- mtcls  <?> "Failed to decode Expr.Field: 'cls' field is missing"
                Expr.Field i (decodeP tname) <$> decode tcls  <*> case mtvalue of
                                                                      Nothing     -> pure Nothing
                                                                      Just tvalue -> Just <$> decode tvalue
            GenCls.Arg -> do
                ext <- getExt GenArg.ext
                (GenArg.Arg mtpat mtvalue) <- ext <?> "Failed to decode Expr.Arg: extension is missing"
                tpat <- mtpat <?> "Failed to decode Expr.Arg: 'pat' field is missing"
                Expr.Arg i <$> decode tpat <*> case mtvalue of
                                                    Nothing     -> pure Nothing
                                                    Just tvalue -> Just <$> decode tvalue
            GenCls.Native -> do
                ext <- getExt GenNative.ext
                (GenNative.Native tsegments) <- ext <?> "Failed to decode Expr.Native: extension is missing"
                Expr.Native i <$> decodeList tsegments
            GenCls.NativeCode -> do
                ext <- getExt GenNativeCode.ext
                (GenNativeCode.NativeCode mtcode) <- ext <?> "Failed to decode Expr.NativeCode: extension is missing"
                tcode <- mtcode <?> "Failed to decode Expr.NativeCode: 'code' field is missing"
                pure $ Expr.NativeCode i (decodeP tcode)
            GenCls.NativeVar -> do
                ext <- getExt GenNativeVar.ext
                (GenNativeVar.NativeVar mtname) <- ext <?> "Failed to decode Expr.NativeVar: extension is missing"
                tname <- mtname <?> "Failed to decode Expr.NativeVar: 'name' field is missing"
                pure $ Expr.NativeVar i (decodeP tname)
            GenCls.Ref -> do
                ext <- getExt GenRef.ext
                (GenRef.Ref mtdst) <- ext <?> "Failed to decode Expr.Ref: extension is missing"
                tdst <- mtdst <?> "Failed to decode Expr.Ref: 'dst' field is missing"
                Expr.Ref i <$> decode tdst
            GenCls.RefType -> do
                ext <- getExt GenRefType.ext
                (GenRefType.RefType mttypename mtname) <- ext <?> "Failed to decode Expr.RefType: extension is missing"
                ttypename <- mttypename <?> "Failed to decode Expr.Ref: 'typename' field is missing"
                tname <- mtname <?> "Failed to decode Expr.RefType: 'name' field is missing"
                pure $ Expr.RefType i (decodeP ttypename) (decodeP tname)
            GenCls.Case -> do
                ext <- getExt GenCase.ext
                (GenCase.Case mtexpr tmatch) <- ext <?> "Failed to decode Expr.Case: extension is missing"
                texpr <- mtexpr <?> "Failed to decode Expr.Case: 'expr' field is missing"
                Expr.Case i <$> decode texpr <*> decodeList tmatch
            GenCls.Match -> do
                ext <- getExt GenMatch.ext
                (GenMatch.Match mtpat tbody) <- ext <?> "Failed to decode Expr.Match: extension is missing"
                tpat <- mtpat <?> "Failed to decode Expr.Match: 'pat' field is missing"
                Expr.Match i <$> decode tpat <*> decodeList tbody
       where getExt = flip Extensions.getExt t

