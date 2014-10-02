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

module Luna.Pass.Transform.HAST.HASTGen.HASTGen where

import qualified Flowbox.Prelude                                     as Prelude
import           Flowbox.Prelude                                     hiding (error, id, mod, simple, cons, exp)
import qualified Luna.AST.Expr                          as LExpr
import qualified Luna.AST.Type                          as LType
import qualified Luna.AST.Pat                           as LPat
import qualified Luna.AST.Lit                           as LLit
import qualified Luna.AST.Module                        as LModule
import qualified Luna.Data.HAST.Expr                         as HExpr
import qualified Luna.Data.HAST.Lit                          as HLit
import qualified Luna.Data.HAST.Module                       as HModule
import qualified Luna.Data.HAST.Extension                    as HExtension
import qualified Luna.Data.HAST.Comment                      as HComment
import qualified Luna.Pass.Transform.HAST.HASTGen.GenState as GenState
import           Luna.Pass.Transform.HAST.HASTGen.GenState   (GenState)
import qualified Luna.Pass.Pass                            as Pass
import           Luna.Pass.Pass                              (Pass)
import           Flowbox.System.Log.Logger
import           Luna.Pass.Transform.HAST.HASTGen.Utils
import qualified Luna.Target.HS.Host.NamingOld                          as Naming
import qualified Luna.Target.HS.Host.Naming                             as Naming
import           Data.String.Utils                                     (join)
import qualified Luna.Data.HAST.Deriving                     as Deriving
import           Luna.Data.HAST.Deriving                     (Deriving)
import qualified Luna.Data.Name                              as Name
import qualified Luna.AST.Arg                                as Arg
import qualified Luna.AST.Lit.Number                         as Number
import           Data.Maybe                                  (isNothing)

import           Control.Monad.State                                 hiding (mapM, mapM_, join)



type GenPass m = Pass GenState m

type LExpr   = LExpr.Expr
type LType   = LType.Type
type LModule = LModule.Module


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Transform.HAST.HASTGen.HASTGen"



biTuple a b = HExpr.Tuple [a,b]
selfSig = HExpr.TypedE (HExpr.AppT (HExpr.VarT "NParam") (HExpr.LitT $ HLit.String "self")) (HExpr.Var "mkArg")
paramSig = HExpr.TypedE (HExpr.VarT "Param") (HExpr.Var "mkArg")

run :: LModule -> Pass.Result HExpr
run = (Pass.run_ (Pass.Info "HASTGen") GenState.empty) . genModule


stdDerivings :: [Deriving]
stdDerivings = [Deriving.Show, Deriving.Eq, Deriving.Ord, Deriving.Generic]


genModule :: LModule -> GenPass HExpr
genModule (LModule.Module _ cls imports classes typeAliases typeDefs fields methods _) = do
    let (LType.Module _ name path) = cls
        mod     = HModule.addImport ["Luna", "Target", "HS"]
                $ HModule.addExt HExtension.DataKinds
                $ HModule.addExt HExtension.DeriveDataTypeable
                $ HModule.addExt HExtension.DeriveGeneric
                $ HModule.addExt HExtension.DysfunctionalDependencies
                $ HModule.addExt HExtension.NoMonomorphismRestriction
                $ HModule.addExt HExtension.FlexibleContexts
                $ HModule.addExt HExtension.FlexibleInstances
                $ HModule.addExt HExtension.GADTs
                $ HModule.addExt HExtension.RebindableSyntax
                $ HModule.addExt HExtension.TemplateHaskell
                $ HModule.addExt HExtension.UndecidableInstances
                $ HModule.addExt HExtension.ViewPatterns
                $ HModule.mk (path ++ [name])
        params  = view LType.params cls
        modCon  = LExpr.ConD 0 name fields
        modConName = Naming.modCon name
    
    GenState.setModule mod
    
    GenState.addComment $ HExpr.Comment $ HComment.H1 $ "Data types"
    genCon' cls modCon stdDerivings
    GenState.addComment $ HExpr.Comment $ HComment.H5 $ "Other data types"
    mapM_ genExpr classes

    GenState.setCls    cls

    GenState.addComment $ HExpr.Comment $ HComment.H1 $ "Type aliases"
    mapM_ (genExpr >=> GenState.addTypeAlias) typeAliases

    GenState.addComment $ HExpr.Comment $ HComment.H1 $ "Type defs"
    mapM_ (genExpr >=> GenState.addTypeDef)   typeDefs

    --genCon' cls modCon stdDerivings
    -- DataType
    --consTH
    
    --GenState.addTHExpression $ thGenerateAccessors name
    --GenState.addTHExpression $ thRegisterAccessors name
    --GenState.addTHExpression $ thInstsAccessors name

    GenState.addComment $ HExpr.Comment $ HComment.H1 $ "Module methods"
    mapM_ genExpr methods
    
    mapM_ (genExpr >=> GenState.addImport) imports
    when (name == "Main") $ do
        GenState.addComment $ HExpr.Comment $ HComment.H1 $ "Main module wrappers"
        GenState.addFunction $ mainf modConName
    GenState.getModule


mainf modname = HExpr.Function "main" []
      $ HExpr.AppE (HExpr.VarE "mainMaker") (HExpr.VarE modname)


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
           *> GenState.addTHExpression (thClsCallInsts conMemName fieldlen (0::Int))
           -- *> GenState.addTHExpression (thGenerateClsGetters conName)
    return (expr, th)

genCon' cls (LExpr.ConD _ conName fields) derivings = do
    let tpName     = view LType.name cls
        clsConName = "Cls_" ++ tpName
        params     = view LType.params cls
        consClsHE  = HExpr.Con clsConName mempty
        conSigName = Naming.mkMemSig clsConName conName
        conDefName = Naming.mkMemDef clsConName conName
        --FIXME[wd]: to powinno byc zrobione ladniej - z nowym AST!
        getName el = case el of
            LExpr.Field {} -> Just $ view LExpr.name el
            _              -> Nothing

    GenState.addComment $ HExpr.Comment $ HComment.H2 $ "Constructor: " ++ tpName ++ "." ++ conName

    consE  <- HExpr.Con conName <$> mapM genExpr fields

    GenState.addDataType $ HExpr.DataD conName params [consE] derivings
    GenState.addDataType $ HExpr.DataD clsConName mempty [consClsHE] derivings

    GenState.addFunction $ HExpr.Function (Naming.con conName) [] (mkAppE [HExpr.VarE "member", mkProxyE conName, mkVal (HExpr.VarE clsConName)])
    
    GenState.addFunction $ HExpr.Function conSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length fields) paramSig))
    GenState.addFunction $ HExpr.Function conDefName [] (HExpr.AppE (HExpr.VarE $ "liftCons" ++ show (length fields)) (HExpr.VarE conName))
    GenState.addTHExpression $ thRegisterMethod clsConName conName



    GenState.addTHExpression $ thGenerateFieldAccessors conName (fmap getName fields)

mkAppE = seqApp HExpr.AppE 

seqApp :: (a -> a -> a) -> [a] -> a
seqApp f (x:xs) = foldl f x xs

mkProxyE name = HExpr.Typed (HExpr.AppT (HExpr.VarT "Proxy") (HExpr.Lit $ HLit.String name)) (HExpr.VarE "Proxy")

    --return expr


genExpr :: LExpr -> GenPass HExpr
genExpr ast = case ast of
    LExpr.Var      _ name                -> pure $ HExpr.Var $ mkVarName name
    LExpr.Con      _ name                -> pure $ HExpr.Var (Naming.con name)
    LExpr.Function _ path name
                     inputs output body  -> do
                                            let name2 = Name.unified name

                                            cls <- GenState.getCls
                                            let tpName = if (null path)
                                                    then cls ^. LType.name
                                                    else (path!!0) -- FIXME[wd]: needs name resolver

                                                argNum     = length inputs
                                                memDefName      = Naming.mkMemDef tpName name2
                                                memSigName      = Naming.mkMemSig tpName name2

                                            when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

                                            GenState.addComment $ HExpr.Comment $ HComment.H2 $ "Method: " ++ tpName ++ "." ++ name2

                                            -----------------------------------------
                                            -- FIXME[wd]: genFuncSig is naive, it should not handle such cases as (a::Int)::Int
                                            --            it should be implemented using State monad, so HASTGen has to change
                                            --            the architecture a little bit
                                            sigTerms <- mapM genFuncSig inputs
                                            let hInputs   = map fst sigTerms
                                            let typeHints = concat $ map snd sigTerms
                                            --logger error (show typeHints)

                                            let mkTypeHint (id,e) = HExpr.AppE (HExpr.AppE (HExpr.VarE "typeMatch") (HExpr.VarE $ "_v_" ++ show id))
                                                                               (HExpr.Typed e $ HExpr.VarE "undefined")
                                                hTypeHints = map mkTypeHint typeHints
                                            -----------------------------------------

                                            let fBody = (((emptyHExpr : hTypeHints) ++ ) <$> genFuncBody body output)
                                                
                                            -- hInputs
                                            f <- HExpr.Function memDefName [foldr biTuple (HExpr.Tuple []) hInputs] <$> (HExpr.DoBlock <$> fBody)
                                            GenState.addFunction f

                                            GenState.addFunction $ HExpr.Function memSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length inputs - 1) paramSig))

                                            GenState.addTHExpression $ thRegisterMethod tpName name2

                                            return f

    LExpr.Cond   _ cond success failure  -> HExpr.CondE <$> genExpr cond <*> mapM genExpr success <*> mapM genExpr failureExprs
                                            where failureExprs = case failure of
                                                                 Just exprs -> exprs
                                                                 Nothing    -> [LExpr.NOP 0]

    LExpr.Case id expr match             -> mkFlattenCtx <$> (HExpr.AppE <$> lamFunc <*> genExpr expr)
                                            where passVar = HExpr.VarE "a"
                                                  body    = HExpr.CaseE passVar <$> mapM genExpr match
                                                  lam     = HExpr.Lambda [passVar] <$> body
                                                  lamFunc = mkLiftf1 <$> lam
    LExpr.Match id pat body              -> HExpr.Match <$> genPat pat <*> (HExpr.DoBlock <$> mapM genExpr body)

    LExpr.Lambda id inputs output body   -> do
                                            let fname      = Naming.mkLamName $ show id
                                                hName      = Naming.mkHandlerFuncName fname
                                                cfName     = mkCFLName fname
                                                argNum     = length inputs

                                            --GenState.addDataType $ HExpr.DataD cfName [] [HExpr.Con cfName []] [Deriving.Show]

                                            f  <-   HExpr.Assignment (HExpr.Var fname)
                                                    <$> ( HExpr.Lambda <$> (mapM genExpr inputs)
                                                                       <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
                                                        )
                                            --GenState.addFunction f

                                            --GenState.addTHExpression $ thRegisterFunction fname argNum []
                                            --GenState.addTHExpression $ thClsCallInsts fname argNum (0::Int)

                                            --return $ HExpr.Var hName
                                            return $ HExpr.LetExpr HExpr.NOP
    LExpr.Grouped _ expr                 -> genExpr expr
    LExpr.Arg     _ pat _                -> genPat pat
    LExpr.ImportNative _ segments        -> pure $ HExpr.ImportNative (join "" $ map genNative segments)
    LExpr.Import  _ path target rename   -> do
                                            tname <- case target of
                                                LExpr.Con      _ tname -> pure tname
                                                LExpr.Var      _ tname -> pure tname
                                                LExpr.Wildcard _       -> Pass.fail "Wildcard imports are not supported yet."
                                                _                      -> Pass.fail "Internal error."
                                            case rename of
                                                Just _                 -> Pass.fail "Named imports are not supported yet."
                                                _                      -> pure ()

                                            return $ HExpr.Import False (["FlowboxM", "Libs"] ++ path ++ [tname]) Nothing where

    LExpr.Data _ cls cons _classes methods -> do
                                           let name        = view LType.name   cls
                                               params      = view LType.params cls

                                           GenState.setCls cls

                                           --consTuples <- mapM (genCon name) cons
                                           --let consE  = map fst consTuples
                                           --    consTH = map snd consTuples

                                           --let dt = HExpr.DataD name params consE stdDerivings
                                           --GenState.addDataType dt

                                           --sequence_ consTH
                                           mapM (flip (genCon' cls) stdDerivings) cons

                                           --GenState.addTHExpression $ thGenerateAccessors name
                                           --GenState.addTHExpression $ thRegisterAccessors name
                                           --GenState.addTHExpression $ thInstsAccessors name

                                           mapM_ genExpr methods

                                           return $ HExpr.NOP

    LExpr.Infix        id name src dst       -> genExpr (LExpr.App id (LExpr.Var 0 name) $ fmap (Arg.Unnamed 0) [src, dst])
                                                --HExpr.Infix name <$> genExpr src <*> genExpr dst
    LExpr.Assignment   _ pat dst             -> HExpr.Arrow <$> genPat pat <*> genCallExpr dst
    LExpr.RecordUpdate _ src selectors expr  -> genExpr $ (setSteps sels) expr
                                                where setter sel exp val = flip (LExpr.App 0) [Arg.Unnamed 0 val]
                                                                         $ LExpr.Accessor 0 (Naming.mkSetName sel) exp
                                                      getter sel exp     = flip (LExpr.App 0) []
                                                                         $ LExpr.Accessor 0 sel exp
                                                      getSel sel           = foldl (flip($)) src (fmap getter (reverse sel))
                                                      setStep       (x:xs) = setter x (getSel xs)
                                                      setSteps args@(_:[]) = setStep args
                                                      setSteps args@(_:xs) = setSteps xs . setStep args
                                                      setSteps          [] = undefined
                                                      sels = reverse selectors

    LExpr.Lit          _ value               -> genLit value
    LExpr.Tuple        _ items               -> mkVal . HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    LExpr.Field        _ name fcls _         -> genType' fcls
                                               --cls <- GenState.getCls
                                               --let clsName = view LType.name cls
                                               --genTypedSafe HExpr.Typed fcls <*> pure (HExpr.Var $ Naming.mkPropertyName clsName name)
    --LExpr.Field        _ name fcls _         -> do
    --                                           cls <- GenState.getCls
    --                                           let clsName = view LType.name cls
    --                                           genTypedSafe HExpr.Typed fcls <*> pure (HExpr.Var $ Naming.mkPropertyName clsName name)
    --LExpr.App          _ src args             -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genCallExpr args)
    --LExpr.App          _ src args            -> HExpr.AppE <$> (HExpr.AppE (HExpr.Var "call") <$> genExpr src) <*> (mkRTuple <$> mapM genCallExpr args)
    --LExpr.App          _ src args            -> foldr (<*>) (genExpr src) ((fmap.fmap) (HExpr.AppE . (HExpr.AppE (HExpr.VarE "appNext"))) [return $ HExpr.VarE "xxx"]) 
    LExpr.App          _ src args            -> HExpr.AppE (HExpr.VarE "call") <$> foldl (flip (<*>)) (genExpr src) ((fmap.fmap) (HExpr.AppE . (HExpr.AppE (HExpr.VarE "appNext"))) (map genCallExpr $ fmap (view Arg.arg) args)) 
    LExpr.Accessor     _ name dst            -> HExpr.AppE <$> (pure $ mkMemberGetter name) <*> genExpr dst --(get0 <$> genExpr dst))
    LExpr.TypeAlias    _ srcType dstType     -> case srcType of
                                                    LType.Con _ segments                    -> HExpr.TySynD (last segments) [] <$> genType' dstType
                                                    LType.App _ (LType.Con _ segments) args -> HExpr.TySynD (last segments) <$> mapM genType' args <*> genType' dstType
    LExpr.TypeDef      _ srcType dstType     -> case srcType of
                                                    LType.Con _ segments                    -> HExpr.NewTypeD (last segments) [] <$> (HExpr.Con (last segments) . (:[]) <$> genType' dstType)
                                                    LType.App _ (LType.Con _ segments) args -> HExpr.NewTypeD (last segments) <$> mapM genType' args <*> (HExpr.Con (last segments) . (:[]) <$> genType' dstType)
    LExpr.List         _ items               -> do
                                                let liftEl el = case el of
                                                        LExpr.RangeFromTo {} -> el
                                                        LExpr.RangeFrom   {} -> el
                                                        _                    -> LExpr.List 0 [el]
                                                    (arrMod, elmod) = if any isRange items
                                                        then (HExpr.AppE (HExpr.Var "concatPure"), liftEl)
                                                        else (Prelude.id, Prelude.id)

                                                mkVal . arrMod . HExpr.ListE <$> mapM (genExpr . elmod) items
    LExpr.RangeFromTo _ start end            -> HExpr.AppE . HExpr.AppE (HExpr.Var "rangeFromTo") <$> genExpr start <*> genExpr end
    LExpr.RangeFrom   _ start                -> HExpr.AppE (HExpr.Var "rangeFrom") <$> genExpr start
    LExpr.Ref         _ dst                  -> genExpr dst
    LExpr.RefType     _ typeName name        -> pure $ thTypeRef typeName name
    LExpr.Native      _ segments             -> pure $ HExpr.Native (join "" $ map genNative segments)
    LExpr.Typed       _ _cls _expr           -> Pass.fail "Typing expressions is not supported yet." -- Potrzeba uzywac hacku: matchTypes (undefined :: m1(s1(Int)))  (val (5 :: Int))
    LExpr.NOP         _                      -> pure HExpr.NOP
    --x                                        -> logger error (show x) *> return HExpr.NOP

isRange :: LExpr.Expr -> Bool
isRange e = case e of
    LExpr.RangeFromTo {} -> True
    LExpr.RangeFrom   {} -> True
    _                    -> False

genNative :: LExpr.Expr -> String
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
    --call0  = HExpr.AppE (HExpr.Var "call0")
    --ret    = HExpr.AppE $ HExpr.Var "return"

genFuncBody :: [LExpr] -> LType -> GenPass [HExpr]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> (:) <$> case x of
                      --FIXME[wd]: error when f.e func output type of unknown
                      LExpr.Assignment _ _ dst -> genFuncTopLevelExpr x --(genTypedE output <*> genFuncTopLevelExpr x)
                      LExpr.Native     {}      -> genFuncTopLevelExpr x
                      _                        -> genFuncTopLevelExpr x --(genTypedE output <*> genFuncTopLevelExpr x) -- mkGetIO <$>
                <*> case x of
                      LExpr.Assignment _ _ dst -> (:[]) <$> pure (mkVal $ HExpr.Tuple []) --(genTypedE output <*> pure (mkVal $ HExpr.Tuple [])) -- . mkGetIO
                      _                        -> pure []
    x:xs -> (:) <$> genFuncTopLevelExpr x <*> genFuncBody xs output


genFuncTopLevelExpr :: LExpr -> GenPass HExpr
genFuncTopLevelExpr expr = case expr of
    LExpr.RecordUpdate _ (LExpr.Var _ name) _ _ -> genFuncTopLevelExpr $ LExpr.Assignment 0 (LPat.Var 0 name) expr
    _                                           -> genCallExpr expr


genPat :: LPat.Pat -> GenPass HExpr
genPat p = case p of
    LPat.App         _ src args -> foldl HExpr.AppP <$> genPat src <*> mapM genPat args
    LPat.Var         _ name     -> return $ HExpr.Var (mkVarName name)
    LPat.Typed       _ pat cls  -> genTypedP cls <*> genPat pat
    LPat.Tuple       _ items    -> (HExpr.ViewP $ "extractTuple" ++ show (length items)) . HExpr.TupleP <$> mapM genPat items
    LPat.Lit         _ value    -> genLit value
    LPat.Wildcard    _          -> return $ HExpr.WildP
    LPat.RecWildcard _          -> return $ HExpr.RecWildP
    LPat.Con         _ name     -> return $ HExpr.ConP name
    LPat.Grouped     _ p'       -> genPat p'
    --_ -> fail $ show p


genFuncSig ::LExpr.Expr -> GenPass (HExpr, [(Int, HExpr)])
genFuncSig (LExpr.Arg _ pat _) = genPatSig pat

genPatSig :: LPat.Pat -> GenPass (HExpr, [(Int, HExpr)])
genPatSig p = case p of
    --LPat.App         _ src args -> foldl HExpr.AppP <$> genPat src <*> mapM genPat args
    LPat.Var         _ name     -> purePat $ pure $ HExpr.Var (mkVarName name)
    LPat.Typed       id pat cls -> (\(hexp, binds) bind -> (hexp, (pat ^. LPat.id,bind):binds)) <$> genPatSig pat <*> genType False cls
    LPat.Tuple       _ items    -> (\psigs -> ((mkPure . mkSafe . HExpr.TupleP $ map fst psigs), (concat $ map snd psigs))) <$> mapM genPatSig items
                                    --mkPure . HExpr.TupleP <$> mapM genPat items
    --LPat.Tuple       _ items    -> mkPure . HExpr.TupleP <$> mapM genPat items
    LPat.Lit         _ value    -> purePat $ genLit value
    --LPat.Wildcard    _          -> return $ HExpr.WildP
    --LPat.RecWildcard _          -> return $ HExpr.RecWildP
    --LPat.Con         _ name     -> return $ HExpr.ConP name
    --_ -> fail $ show p
    where purePat p = (,) <$> p <*> pure []


genTypedE :: LType -> GenPass (HExpr -> HExpr)
genTypedE = genTyped HExpr.TypedE

genTypedP :: LType -> GenPass (HExpr -> HExpr)
genTypedP = genTyped HExpr.TypedP

genTyped :: (HExpr -> HExpr -> HExpr) -> LType -> GenPass (HExpr -> HExpr)
genTyped = genTypedProto False

genTypedSafe :: (HExpr -> HExpr -> HExpr) -> LType -> GenPass (HExpr -> HExpr)
genTypedSafe = genTypedProto True

genTypedProto :: Bool -> (HExpr -> HExpr -> HExpr) -> LType -> GenPass (HExpr -> HExpr)
genTypedProto safeTyping cls t = case t of
    LType.Unknown _          -> pure Prelude.id
    _                        -> cls <$> genType safeTyping t

genType :: Bool -> LType -> GenPass HExpr
genType safeTyping t = case t of
    LType.Var     _ name     -> return $ thandler (HExpr.Var  name)
    LType.Con     _ segments -> return $ thandler (HExpr.ConE segments)

    LType.Tuple   _ items    -> HExpr.Tuple <$> mapM (genType safeTyping) items
    LType.App     _ src args -> (liftM2 . foldl) (HExpr.AppT) (genType safeTyping src) (mapM (genType safeTyping) args)
    LType.Unknown _          -> logger critical "Cannot generate code for unknown type1" *> Pass.fail "Cannot generate code for unknown type"
    --_                        -> fail $ show t
    where mtype    = HExpr.VarT $ if safeTyping then "Pure" else "m_" ++ show (view LType.id t)
          stype    = HExpr.VarT $ if safeTyping then "Safe" else "s_" ++ show (view LType.id t)
          thandler = HExpr.AppT mtype . HExpr.AppT stype

genType' :: LType -> GenPass HExpr
genType' t = case t of
    LType.Var     _ name     -> return $ HExpr.Var  name
    LType.Con     _ segments -> return $ HExpr.ConE segments

    LType.Tuple   _ items    -> HExpr.Tuple <$> mapM genType' items
    LType.App     _ src args -> (liftM2 . foldl) (HExpr.AppT) (genType' src) (mapM genType' args)
    LType.Unknown _          -> logger critical "Cannot generate code for unknown type2" *> Pass.fail "Cannot generate code for unknown type"
    --_                        -> fail $ show t

genLit :: LLit.Lit -> GenPass HExpr
genLit lit = case lit of
    -- FIXME[wd]: fix the number handling.
    LLit.Number _ (Number.Number base repr exp sign) -> do
        when (base /= 10) $ Pass.fail "number base different than 10 are not yet supported"
        when (not $ isNothing exp) $ Pass.fail "number exponents are not yet supported"
        case repr of
            Number.Float   int frac -> mkLit "Double" (HLit.Float $ int ++ "." ++ frac)
            Number.Decimal int      -> mkLit "Int"    (HLit.Integer int)

    --LLit.Integer _ str      -> mkLit "Int"    (HLit.Integer str)
    --LLit.Float   _ str      -> mkLit "Double" (HLit.Float   str)
    LLit.String  _ str      -> mkLit "String" (HLit.String  str)
    LLit.Char    _ char     -> mkLit "Char"   (HLit.Char    char)
    --_ -> fail $ show lit
    where mkLit cons hast = return . mkVal $ HExpr.TypedE (HExpr.ConT cons) (HExpr.Lit hast)

