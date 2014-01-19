---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen where

import qualified Flowbox.Prelude                                     as Prelude
import           Flowbox.Prelude                                     hiding (error, id, mod, simple)
import qualified Flowbox.Luna.Data.AST.Expr                          as LExpr
import qualified Flowbox.Luna.Data.AST.Type                          as LType
import qualified Flowbox.Luna.Data.AST.Pat                           as LPat
import qualified Flowbox.Luna.Data.AST.Lit                           as LLit
import qualified Flowbox.Luna.Data.AST.Module                        as LModule
import qualified Flowbox.Luna.Data.HAST.Expr                         as HExpr
import qualified Flowbox.Luna.Data.HAST.Lit                          as HLit
import qualified Flowbox.Luna.Data.HAST.Module                       as HModule
import qualified Flowbox.Luna.Data.HAST.Extension                    as HExtension
import qualified Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState as GenState
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.GenState   (GenState)
import           Flowbox.Luna.Passes.Analysis.FuncPool.Pool            (Pool)
import qualified Flowbox.Luna.Passes.Analysis.FuncPool.Pool          as Pool
import qualified Flowbox.Luna.Passes.Pass                            as Pass
import           Flowbox.Luna.Passes.Pass                              (Pass)
import           Flowbox.System.Log.Logger                             
import           Flowbox.Luna.Passes.Transform.HAST.HASTGen.Utils      
import qualified Luna.Target.HS.Naming                               as Naming
import           Data.String.Utils                                     (join)
import qualified Data.Set                                            as Set

import           Control.Monad.State                                 hiding (mapM, mapM_, join)



type GenPass m = Pass GenState m

type LExpr   = LExpr.Expr
type LType   = LType.Type
type LModule = LModule.Module


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen"


run :: LModule -> Pool -> Pass.Result HExpr
run = (Pass.run_ (Pass.Info "HASTGen") GenState.empty) .: genModule


genModule :: LModule -> Pool -> GenPass HExpr
genModule lmod@(LModule.Module _ cls imports classes _ methods _) fpool = do 
    let (LType.Module _ path) = cls
        --fnames  = Set.toList $ Pool.names fpool
        mod     = HModule.addImport ["Luna", "Target", "HS", "Core"]
                -- $ HModule.addExt HExtension.AutoDeriveTypeable
                $ HModule.addExt HExtension.DataKinds
                $ HModule.addExt HExtension.DeriveDataTypeable
                $ HModule.addExt HExtension.DeriveGeneric
                $ HModule.addExt HExtension.FlexibleInstances
                $ HModule.addExt HExtension.MultiParamTypeClasses
                $ HModule.addExt HExtension.NoMonomorphismRestriction
                $ HModule.addExt HExtension.RebindableSyntax
                $ HModule.addExt HExtension.ScopedTypeVariables
                $ HModule.addExt HExtension.TemplateHaskell
                -- $ HModule.addExt HExtension.TypeFamilies
                $ HModule.addExt HExtension.UndecidableInstances 
                $ HModule.mk path
        name    = last path
        modcls  = LModule.mkClass lmod
        modclss = classes ++ [modcls]

    GenState.setModule mod

    mapM_ genExpr modclss
    mapM_ (genExpr >=> GenState.addImport) imports
    when (name == "Main") $ do
        let funcnames = map (view LExpr.name) methods
        if not $ "main" `elem` funcnames
            then logger warning "No 'main' function defined."
            else GenState.addFunction mainf
    GenState.getModule


mainf :: HExpr
mainf = HExpr.Function "main" [] $
        HExpr.DoBlock [   HExpr.Arrow (HExpr.Var "m")
                        $ HExpr.AppE (HExpr.Var "call0") 
                        $ HExpr.Var "con_Main"
                      ,   mkGetIO
                        $ mkCall0
                        $ HExpr.AppE (mkMemberGetter "main")
                        $ HExpr.Var "m"
                      ] 


genVArgCon arglen name ccname params base = getter where
    argVars    = map (("v" ++).show) [1..arglen]
    exprArgs   = map HExpr.Var argVars
    t          = foldl HExpr.AppE (HExpr.Var ccname) (map HExpr.Var params)
    selfVar    = HExpr.TypedP t $ HExpr.Var "self"
    exprVars   = selfVar : exprArgs
    getter     = HExpr.Function (mkTName arglen name) exprVars
               $ mkPure (foldl HExpr.AppE base exprArgs)


genVArgGetter arglen mname = getter where
    argVars    = map (("v" ++).show) [1..arglen]
    exprArgs   = map HExpr.Var argVars
    exprVars   = HExpr.Var "self" : exprArgs
    getterBase = HExpr.AppE (HExpr.Var $ mkFuncName mname)
               $ HExpr.AppE (HExpr.Var $ mkGetName $ mkCFName mname) (HExpr.Var "self")
    getter     = HExpr.Function (mkTName arglen mname) exprVars
               $ foldl HExpr.AppE getterBase exprArgs


genVArgGetterL arglen mname cfname = getter where
    argVars    = map (("v" ++).show) [1..arglen]
    exprArgs   = map HExpr.Var argVars
    t          = mkPure $ foldl HExpr.AppE (HExpr.Var cfname) (map HExpr.Var [])
    selfVar    = HExpr.TypedP t $ HExpr.Var "self"
    exprVars   = selfVar : exprArgs
    getterBase = (HExpr.Var $ mkFuncName mname)
    getter     = HExpr.Function (mkTName arglen mname) exprVars
               $ foldl HExpr.AppE getterBase exprArgs


-- generate declarations (imports, CF newtypes, THInstC)
genFuncDecl clsname name = do
    let vname  = mkVarName name
        cfName = mkCFName $ mangleName clsname vname

    GenState.addNewType      $ genCFDec clsname cfName
    GenState.addTHExpression $ genTHInstMem name cfName


typeMethodSelf cls inputs = nargs
    where (self:args) = inputs
          nparams     = map (LType.Var 0) (view LType.params cls)
          patbase     = LType.App 0 (LType.Con 0 [view LType.name cls]) nparams
          nself       = self & over LExpr.pat (\p -> LPat.Typed 0 p patbase)
          nargs       = nself:args


--genCon :: GenMonad m => LExpr -> Pass.Result m (HExpr, m0())
genCon dataName (LExpr.ConD _ conName fields) = do
    let fieldlen = length fields
        conMemName = Naming.mkMemName dataName conName
    expr  <- HExpr.Con conName <$> mapM genExpr fields
    let th =  GenState.addTHExpression (thRegisterCon dataName conName fieldlen [])
           *> GenState.addTHExpression (thClsCallInsts conMemName fieldlen 0)
           -- *> GenState.addTHExpression (thGenerateClsGetters conName)
    return (expr, th)


genExpr :: LExpr -> GenPass HExpr
genExpr ast = case ast of
    LExpr.Var      _ name                -> pure $ HExpr.Var $ mkVarName name
    LExpr.Con      _ name                -> pure $ HExpr.Var ("con" ++ mkConsName name)
    LExpr.Function _ path name   
                     inputs output body  -> do
                                            cls <- GenState.getCls
                                            let 
                                                clsName = if (null path) 
                                                    then view LType.name cls
                                                    else (path!!0)
                                                -- FIXME[wd]: type "self" in not extension methods
                                                --            We should type self in extension methods also, but it needs TH to read type params
                                                -- RELATED: COMPILER-42
                                                --ninputs    = if (null path)
                                                --    then (typeMethodSelf cls) inputs
                                                --    else inputs
                                                ninputs = inputs
                                                argNum     = length ninputs
                                                mname      = mangleName clsName $ mkVarName name
                                                vargGetter = genVArgGetter (argNum-1) mname
                                                cgetCName  = mkCGetCName (argNum-1)
                                                cgetName   = mkCGetName  (argNum-1)
                                                getNName   = mkTName (argNum-1) mname
                                                fname      = Naming.mkMemName clsName name


                                            when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

                                            --genFuncDecl clsName name

                                            --f  <-   HExpr.Assignment (HExpr.Var fname) 
                                            --        <$> ( HExpr.AppE (HExpr.Var $ "defFunction" ++ show (argNum + 1))
                                            --              <$> ( HExpr.Lambda <$> (mapM genExpr ninputs)
                                            --                                 <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
                                            --                  )
                                            --            )

                                            f  <-   HExpr.Assignment (HExpr.Var fname) 
                                                          <$> ( HExpr.Lambda <$> (mapM genExpr ninputs)
                                                                             <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
                                                              )

                                            GenState.addFunction f
                                           
                                            ---- GetN functions
                                            --GenState.addFunction vargGetter
                                            
                                            ---- TH snippets
                                            --GenState.addTHExpression $ genTHInst cgetCName getNName cgetName
                                            --GenState.addTHExpression $ thSelfTyped fname clsName

                                            GenState.addTHExpression $ thRegisterFunction fname argNum [] 
                                            GenState.addTHExpression $ thClsCallInsts fname argNum 0
                                            GenState.addTHExpression $ thRegisterMember name clsName fname

                                            return f

    LExpr.Lambda id inputs output body   -> do
                                            let mname      = mkLamName $ show id
                                                fname      = mkFuncName mname
                                                conName    = mkConName fname
                                                cfName     = mkCFLName mname
                                                argNum     = length inputs
                                                cgetCName  = mkCGetCName argNum
                                                cgetName   = mkCGetName  argNum
                                                getNName   = mkTName argNum mname
                                                vargGetter = genVArgGetterL argNum mname cfName

                                            GenState.addDataType $ HExpr.DataD cfName [] [HExpr.Con cfName []] ["Show"]

                                            f  <-   HExpr.Assignment (HExpr.Var fname) 
                                                    <$> ( HExpr.Lambda <$> (mapM genExpr inputs)
                                                                       <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
                                                        )
                                            GenState.addFunction f

                                            GenState.addTHExpression $ thRegisterLambda fname argNum [] 
                                            GenState.addTHExpression $ thClsCallInsts fname argNum 0

                                            --GenState.addFunction vargGetter
                                            --GenState.addTHExpression $ genTHInst cgetCName getNName cgetName

                                            return $ HExpr.Var conName

    LExpr.Arg _ pat _                    -> genPat pat
                                                  
    LExpr.Import _ path target rename    -> do
                                            tname <- case target of
                                                LExpr.Con      _ tname -> pure tname
                                                LExpr.Var      _ tname -> pure tname
                                                LExpr.Wildcard _       -> Pass.fail "Wildcard imports are not supported yet."
                                                _                      -> Pass.fail "Internal error."
                                            case rename of
                                                Just _                 -> Pass.fail "Named imports are not supported yet."
                                                _                      -> pure ()
                                            
                                            return $ HExpr.Import False (["FlowboxM", "Libs"] ++ path ++ [tname]) Nothing where
                     
    LExpr.Data _ cls cons classes methods -> do 
                                           let name        = view LType.name   cls
                                               params      = view LType.params cls
                                               --fieldNames  = map (view LExpr.name) fields
                                               --fieldlen    = length fields
                                               ----tmethods    = map (typeMethodSelf name) methods
                                               --funcNames   = map (view LExpr.name) methods 
                                               --memberNames = fieldNames ++ funcNames
                                               --ccname      = mkCCName name
                                           
                                           GenState.setCls cls
                                           
                                           -- DataType
                                           consTuples <- mapM (genCon name) cons
                                           let consE  = map fst consTuples
                                               consTH = map snd consTuples

                                           --cons   <- HExpr.Con name <$> mapM genExpr fields
                                           let dt = HExpr.DataD name params consE ["Show", "Generic"]
                                           GenState.addDataType dt
        
                                           sequence consTH
                                           ---- get0 value instance
                                           --GenState.addInstance $ genDTGet0 name params
        
                                           --mapM_ genExpr methods
         
                                           ---- CONSTRUCTORS --
       
                                           ---- CC type constructor
                                           --let con = genCCDec ccname
                                           --GenState.addDataType con
          
                                           ---- constructor function
                                           --GenState.addFunction $ genCon name ccname
           
                                           --let test   = genVArgCon fieldlen name ccname [] (HExpr.ConE [name])
                                           --GenState.addFunction $ test
          
                                           -- Constructor TH snippet

                                           --let conNames   = map (view LExpr.name) cons
                                           --    conArgLens = map (length . view LExpr.fields) cons


                                           --let arglen    = fieldlen
                                           --    cgetCName = mkCGetCName arglen
                                           --    cgetName  = mkCGetName  arglen
                                           --    getNName  = mkTName arglen name
                                           ----GenState.addTHExpression $ genTHInst cgetCName getNName cgetName
          
                                           --GenState.addTHExpression $ thRegisterClass name fieldlen [] 
                                           --GenState.addTHExpression $ thClsCallInsts name fieldlen 0
                                           
                                           GenState.addTHExpression $ thGenerateAccessors name
                                           GenState.addTHExpression $ thRegisterAccessors name
                                           GenState.addTHExpression $ thInstsAccessors name
         
            
                                           mapM_ genExpr methods
           
                                                       
                                           return dt

    LExpr.Infix       _ name src dst      -> HExpr.Infix name <$> genExpr src <*> genExpr dst
    LExpr.Assignment  _ pat dst           -> HExpr.Arrow <$> genPat pat <*> genCallExpr dst
    LExpr.Lit         _ value             -> genLit value
    LExpr.Tuple       _ items             -> mkVal . HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    LExpr.Field       _ name fcls _       -> do
                                             cls <- GenState.getCls 
                                             let clsName = view LType.name cls
                                             genTyped HExpr.Typed fcls <*> pure (HExpr.Var $ Naming.mkPropertyName clsName name)
    --LExpr.App         _ src args          -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genCallExpr args)
    LExpr.App         _ src args          -> HExpr.AppE <$> (HExpr.AppE (HExpr.Var "call") <$> genExpr src) <*> (mkRTuple <$> mapM genCallExpr args)
    LExpr.Accessor    _ name dst          -> HExpr.AppE <$> (pure $ mkMemberGetter name) <*> genExpr dst --(get0 <$> genExpr dst))
    LExpr.List        _ items             -> do
                                             let liftEl el = case el of
                                                     LExpr.RangeFromTo {} -> el
                                                     LExpr.RangeFrom   {} -> el
                                                     _                    -> LExpr.List 0 [el]
                                                 (arrMod, elmod) = if any isRange items 
                                                     then (HExpr.AppE (HExpr.Var "concatPure"), liftEl)
                                                     else (Prelude.id, Prelude.id)
                         
                                             mkVal . arrMod . HExpr.ListE <$> mapM (genExpr . elmod) items
    LExpr.RangeFromTo _ start end         -> HExpr.AppE . HExpr.AppE (HExpr.Var "rangeFromTo") <$> genExpr start <*> genExpr end
    LExpr.RangeFrom   _ start             -> HExpr.AppE (HExpr.Var "rangeFrom") <$> genExpr start 
    LExpr.Native      _ segments          -> pure $ HExpr.Native (join "" $ map genNative segments)
    --x                                     -> logger error (show x) *> return HExpr.NOP
    where
        getN n = HExpr.AppE (HExpr.Var $ "call" ++ show n)
        get0   = getN (0::Int)

isRange e = case e of
    LExpr.RangeFromTo {} -> True
    LExpr.RangeFrom   {} -> True
    _                    -> False


genNative expr = case expr of
    LExpr.NativeCode _ code -> code
    LExpr.NativeVar  _ name -> mkVarName name

genCallExpr :: LExpr -> GenPass HExpr
genCallExpr e = trans <$> genExpr e where
    trans = case e of
        LExpr.App        {} -> id
        LExpr.Native     {} -> id
        LExpr.Assignment {} -> id
        LExpr.Lambda     {} -> id
        _                   -> id
        --_                   -> call0
    id     = Prelude.id
    call0  = HExpr.AppE (HExpr.Var "call0")
    ret    = HExpr.AppE $ HExpr.Var "return"

genFuncBody :: [LExpr] -> LType -> GenPass [HExpr]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> (:) <$> case x of
                      LExpr.Assignment _ _ dst -> (genTyped HExpr.TypedE output <*> genCallExpr x)
                      LExpr.Native     {}      -> genCallExpr x
                      _                        -> (genTyped HExpr.TypedE output <*> genCallExpr x) -- mkGetIO <$>
                <*> case x of
                      LExpr.Assignment _ _ dst -> (:[]) <$> (genTyped HExpr.TypedE output <*> pure (mkVal $ HExpr.Tuple [])) -- . mkGetIO 
                      _                        -> pure []
    x:xs -> (:) <$> genCallExpr x <*> genFuncBody xs output


genPat :: LPat.Pat -> GenPass HExpr
genPat p = case p of
    LPat.Var      _ name     -> return $ HExpr.Var (mkVarName name)
    LPat.Typed    _ pat cls  -> genTyped HExpr.TypedP cls <*> genPat pat
    LPat.Tuple    _ items    -> mkPure . HExpr.TupleP <$> mapM genPat items
    LPat.Lit      _ value    -> genLit value
    LPat.Wildcard _          -> return $ HExpr.WildP
    _ -> fail $ show p

genTyped :: (HExpr -> HExpr -> HExpr) -> LType -> GenPass (HExpr -> HExpr)
genTyped cls t = case t of
    LType.Unknown _          -> pure Prelude.id
    _                        -> cls <$> genType t

genType :: LType -> GenPass HExpr
genType t = case t of
    LType.Var     _ name      -> return $ HExpr.Var (name)
    LType.Con     id segments -> return $ HExpr.AppT (HExpr.VarT $ "Pure") -- it has to be pure, otherwise we get very strange type checker error
                                        $ HExpr.AppT (HExpr.VarT $ "s_" ++ show id) 
                                        $ HExpr.ConE segments
    LType.Tuple   _ items    -> HExpr.Tuple <$> mapM genType items
    LType.App     _ src args -> (liftM2 . foldl) (HExpr.AppT) (genType src) (mapM genType args)
    LType.Unknown _          -> logger critical "Cannot generate code for unknown type" *> Pass.fail "Cannot generate code for unknown type"
    --_                        -> fail $ show t

genLit :: LLit.Lit -> GenPass HExpr
genLit lit = case lit of
    LLit.Integer _ str      -> mkLit "Int"    (HLit.Integer str)
    LLit.Float   _ str      -> mkLit "Double" (HLit.Float   str)
    LLit.String  _ str      -> mkLit "String" (HLit.String  str)
    LLit.Char    _ char     -> mkLit "Char"   (HLit.Char    char)
    --_ -> fail $ show lit
    where mkLit cons hast = return . mkVal $ HExpr.TypedE (HExpr.ConT cons) (HExpr.Lit hast)

