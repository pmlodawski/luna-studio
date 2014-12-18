---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Luna.Pass2.Target.HS.HASTGen where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
--import qualified Luna.ASTNew.Enum             as Enum
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl             as Decl
import           Luna.ASTNew.Decl             (LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
--import qualified Luna.ASTNew.Type             as Type
--import           Luna.ASTNew.Type             (Type)
--import qualified Luna.ASTNew.Pat              as Pat
--import           Luna.ASTNew.Pat              (LPat, Pat)
import           Luna.ASTNew.Expr             (LExpr, Expr)
import qualified Luna.ASTNew.Expr             as Expr
--import qualified Luna.ASTNew.Lit              as Lit
--import qualified Luna.ASTNew.Native           as Native
import qualified Luna.ASTNew.Name             as Name
--import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import qualified Luna.Data.HAST.Expr                         as HExpr
import qualified Luna.Data.HAST.Lit                          as HLit
import qualified Luna.Data.HAST.Module                       as HModule
import qualified Luna.Data.HAST.Extension                    as HExtension
--import qualified Luna.Data.HAST.Comment                      as HComment
import           Luna.Data.HAST.Comment (Comment(H1, H2, H3, H4, H5))

import qualified Luna.Data.HAST.Deriving                     as Deriving
import           Luna.Data.HAST.Deriving                     (Deriving)

import           Luna.Pass                    (Pass(Pass), PassMonad)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import qualified Luna.Parser.Parser           as Parser
import           Luna.ASTNew.Name.Pattern     (NamePat(NamePat), Segment(Segment), Arg(Arg))
import qualified Luna.ASTNew.Name.Pattern     as NamePat

import qualified Luna.Pass2.Target.HS.HASTGen.State as State
import           Luna.Pass2.Target.HS.HASTGen.State (addComment, setModule, getModule)
import           Luna.ASTNew.Name.Hash              (Hashable, hash)
--import qualified Luna.Target.HS.Host.NamingOld                          as Naming
import qualified Luna.Target.HS.Host.Naming2 as Naming


----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data HASTGen = HASTGen

type PassResult           m   = PassMonad State.GenState m
type PassCtx          lab m a = (Enumerated lab, Traversal m a)
type Traversal            m a = (Pass.PassCtx m, AST.Traversal        HASTGen (PassResult m) a a)
type DefaultTraversal     m a = (Pass.PassCtx m, AST.DefaultTraversal HASTGen (PassResult m) a a)


type HExpr = HExpr.Expr

------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (Traversal m a) => a -> PassResult m a
traverseM = AST.traverseM HASTGen

defaultTraverseM :: (DefaultTraversal m a) => a -> PassResult m a
defaultTraverseM = AST.defaultTraverseM HASTGen


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: Monad m => Pass State.GenState (Unit (LModule a e) -> PassResult m HExpr)
pass = Pass "HASTGen" "Haskell AST generator" def genUnit

genUnit (Unit m) = genModule m



stdDerivings :: [Deriving]
stdDerivings = [Deriving.Show, Deriving.Eq, Deriving.Ord, Deriving.Generic]


genModule :: Monad m => LModule a e -> PassResult m HExpr
genModule (Label lab (Module path name body)) = do
    let mod     = HModule.addImport ["Luna", "Target", "HS"]
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
                $ HModule.mk (fmap fromString $ path ++ [name])
        --params  = view LType.params cls
        --modCon  = LExpr.ConD 0 name fields
        --modConName = Naming.modCon name
    
    setModule mod
    
    mapM_ genDecl body
    --addComment $ H1 "Data types"
    --genCon' cls modCon stdDerivings
    --mapM_ genExpr classes

    --State.setCls    cls

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Type aliases"
    --mapM_ (genExpr >=> State.addTypeAlias) typeAliases

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Type defs"
    --mapM_ (genExpr >=> State.addTypeDef)   typeDefs

    ----genCon' cls modCon stdDerivings
    ---- DataType
    ----consTH
    
    ----State.addTHExpression $ thGenerateAccessors name
    ----State.addTHExpression $ thRegisterAccessors name
    ----State.addTHExpression $ thInstsAccessors name

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Module methods"
    --mapM_ genExpr methods
    
    --mapM_ (genExpr >=> State.addImport) imports
    --when (name == "Main") $ do
    --    State.addComment $ HExpr.Comment $ HComment.H1 $ "Main module wrappers"
    --    State.addFunction $ mainf modConName
    State.getModule

genCons :: (Monad m, Applicative m, MonadState State.GenState m)
        => Text -> [Text] -> [Decl.LCons a e] -> [Deriving] -> Bool -> m ()
genCons name params cons derivings makeDataType = do
    let conDecls = fmap (view Label.element) cons
        clsConName = "Cls_" <> hash name
        genMyCon (Decl.Cons conName fields) = HExpr.Con (fromString $ toString conName) <$> pure [] -- mapM genExpr fields
    
        
    --    --FIXME[wd]: to powinno byc zrobione ladniej - z nowym AST!
    --    getName el = case el of
    --        LExpr.Field {} -> Just $ view LExpr.name el
    --        _              -> Nothing

    --    genMyCon (LExpr.ConD _ conName fields) = HExpr.Con conName <$> mapM genExpr fields

        genConData (Decl.Cons (convVar -> conName) fields) = do
            let conSigName = Naming.mkMemSig clsConName conName
                conDefName = Naming.mkMemDef clsConName conName
                consFunction = HExpr.Function (Naming.con conName) [] 
                             $ HExpr.app (HExpr.VarE Naming.member) 
                                         [HExpr.proxy conName, HExpr.val (HExpr.VarE clsConName)]

            addComment . H3 $ dotname [name, conName] <> " constructor"
            State.addFunction consFunction
            --State.addFunction $ HExpr.Function conSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length fields) paramSig))
            --State.addFunction $ HExpr.Function conDefName [] (HExpr.AppE (HExpr.VarE $ "liftCons" ++ show (length fields)) (HExpr.VarE conName))
            --State.addTHExpression $ thRegisterMethod clsConName conName
            --State.addTHExpression $ thGenerateFieldAccessors conName (fmap getName fields)

    
    addComment . H2 $ name <> " type"
    consE <- mapM genMyCon conDecls
    if makeDataType then State.addDataType $ HExpr.DataD name params consE derivings
                    else State.addComment  $ H5 "datatype provided externally"
    addClsDataType clsConName derivings
    mapM_ genConData conDecls

addClsDataType clsConName derivings = do
    let consClsHE  = HExpr.Con clsConName mempty
    State.addDataType $ HExpr.DataD clsConName mempty [consClsHE] derivings


--data Cons  a e = Cons   { _consName :: CNameP   , _fields :: [LField a e]                  } deriving (Show, Eq, Generic, Read)

dotname names = mjoin "." names

convVar :: (Wrapper t, Hashable a Text) => t a -> Text
convVar = hash . unwrap



seqApp :: (a -> a -> a) -> a -> [a] -> a
seqApp f a as = foldl f a as



genDecl :: Monad m => LDecl a e -> PassResult m HExpr
genDecl ast@(Label lab decl) = case decl of
    Decl.Data name params cons defs -> do
        genCons (convVar name) (fmap convVar params) cons stdDerivings True
        return $ HExpr.NOP




    --LExpr.Data _ cls cons _classes methods -> do
    --                                       let name        = view LType.name   cls
    --                                           params      = view LType.params cls

    --                                       GenState.setCls cls

    --                                       --consTuples <- mapM (genCon name) cons
    --                                       --let consE  = map fst consTuples
    --                                       --    consTH = map snd consTuples

    --                                       --let dt = HExpr.DataD name params consE stdDerivings
    --                                       --GenState.addDataType dt
                                           

    --                                       --sequence_ consTH
    --                                       --mapM (flip (genCon' cls) stdDerivings) cons

    --                                       genCons cls cons stdDerivings True

    --                                       --GenState.addTHExpression $ thGenerateAccessors name
    --                                       --GenState.addTHExpression $ thRegisterAccessors name
    --                                       --GenState.addTHExpression $ thInstsAccessors name
    --                                       GenState.addComment $ HExpr.Comment $ HComment.H3 $ name ++ " methods"

    --                                       mapM_ genExpr methods

    --                                       return $ HExpr.NOP


genExpr :: Monad m => LExpr a v -> PassResult m HExpr
genExpr ast = undefined -- case ast of
    --LExpr.Var      _ name                -> pure $ HExpr.Var $ mkVarName name
    --LExpr.FuncVar  _ name                -> pure $ HExpr.Var $ mkVarName $ Name.unified name
    --LExpr.Con      _ name                -> pure $ HExpr.Var (Naming.con name)
    --LExpr.Function _ path name
    --                 inputs output body  -> do
    --                                        let name2 = Name.unified name

    --                                        cls <- GenState.getCls
    --                                        let tpName = if (null path)
    --                                                then cls ^. LType.name
    --                                                else (path!!0) -- FIXME[wd]: needs name resolver

    --                                            argNum     = length inputs
    --                                            memDefName      = Naming.mkMemDef tpName name2
    --                                            memSigName      = Naming.mkMemSig tpName name2

    --                                        when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

    --                                        GenState.addComment $ HExpr.Comment $ HComment.H2 $ "Method: " ++ tpName ++ "." ++ name2

    --                                        -----------------------------------------
    --                                        -- FIXME[wd]: genFuncSig is naive, it should not handle such cases as (a::Int)::Int
    --                                        --            it should be implemented using State monad, so HASTGen has to change
    --                                        --            the architecture a little bit
    --                                        sigTerms <- mapM genFuncSig inputs
    --                                        let hInputs   = map fst sigTerms
    --                                        let typeHints = concat $ map snd sigTerms
    --                                        --logger error (show typeHints)

    --                                        let mkTypeHint (id,e) = HExpr.AppE (HExpr.AppE (HExpr.VarE "typeMatch") (HExpr.VarE $ "_v_" ++ show id))
    --                                                                           (HExpr.Typed e $ HExpr.VarE "undefined")
    --                                            hTypeHints = map mkTypeHint typeHints
    --                                        -----------------------------------------

    --                                        let fBody = (((emptyHExpr : hTypeHints) ++ ) <$> genFuncBody body output)
                                                
    --                                        -- hInputs
    --                                        f <- HExpr.Function memDefName [foldr biTuple (HExpr.Tuple []) hInputs] <$> (HExpr.DoBlock <$> fBody)
    --                                        GenState.addFunction f

    --                                        GenState.addFunction $ HExpr.Function memSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length inputs - 1) paramSig))

    --                                        GenState.addTHExpression $ thRegisterMethod tpName name2

    --                                        return f

    --LExpr.Cond   _ cond success failure  -> HExpr.CondE <$> genExpr cond <*> mapM genExpr success <*> mapM genExpr failureExprs
    --                                        where failureExprs = case failure of
    --                                                             Just exprs -> exprs
    --                                                             Nothing    -> [LExpr.NOP 0]

    --LExpr.Case id expr match             -> mkFlattenCtx <$> (HExpr.AppE <$> lamFunc <*> genExpr expr)
    --                                        where passVar = HExpr.VarE "a"
    --                                              caseE   = HExpr.CaseE passVar <$> body'
    --                                              body    = mapM genExpr match
    --                                              lam     = HExpr.Lambda [passVar] <$> caseE
    --                                              lamFunc = mkLiftf1 <$> lam
    --                                              body'   = (\a -> a ++ [HExpr.Match HExpr.WildP (HExpr.AppE (HExpr.VarE "error") (HExpr.Lit $ HLit.String "TODO (!!!) Main.luna: path/Main.luna:(...,...)-(...,...): Non-exhaustive patterns in case"))]) <$> body
    --LExpr.Match id pat body              -> HExpr.Match <$> genPat pat <*> (HExpr.DoBlock <$> mapM genExpr body)

    --LExpr.Lambda id inputs output body   -> do
    --                                        let fname      = Naming.mkLamName $ show id
    --                                            hName      = Naming.mkHandlerFuncName fname
    --                                            cfName     = mkCFLName fname
    --                                            argNum     = length inputs

    --                                        --GenState.addDataType $ HExpr.DataD cfName [] [HExpr.Con cfName []] [Deriving.Show]

    --                                        f  <-   HExpr.Assignment (HExpr.Var fname)
    --                                                <$> ( HExpr.Lambda <$> (mapM genExpr inputs)
    --                                                                   <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
    --                                                    )
    --                                        --GenState.addFunction f

    --                                        --GenState.addTHExpression $ thRegisterFunction fname argNum []
    --                                        --GenState.addTHExpression $ thClsCallInsts fname argNum (0::Int)

    --                                        --return $ HExpr.Var hName
    --                                        return $ HExpr.LetExpr HExpr.NOP
    --LExpr.Grouped _ expr                 -> genExpr expr
    --LExpr.Arg     _ pat _                -> genPat pat
    --LExpr.ImportNative _ segments        -> pure $ HExpr.ImportNative (join "" $ map genNative segments)
    --LExpr.Import  _ path target rename   -> do
    --                                        tname <- case target of
    --                                            LExpr.Con      _ tname -> pure tname
    --                                            LExpr.Var      _ tname -> pure tname
    --                                            LExpr.Wildcard _       -> Pass.fail "Wildcard imports are not supported yet."
    --                                            _                      -> Pass.fail "Internal error."
    --                                        case rename of
    --                                            Just _                 -> Pass.fail "Named imports are not supported yet."
    --                                            _                      -> pure ()

    --                                        return $ HExpr.Import False (["FlowboxM", "Libs"] ++ path ++ [tname]) Nothing where

    --LExpr.Data _ cls cons _classes methods -> do
    --                                       let name        = view LType.name   cls
    --                                           params      = view LType.params cls

    --                                       GenState.setCls cls

    --                                       --consTuples <- mapM (genCon name) cons
    --                                       --let consE  = map fst consTuples
    --                                       --    consTH = map snd consTuples

    --                                       --let dt = HExpr.DataD name params consE stdDerivings
    --                                       --GenState.addDataType dt
                                           

    --                                       --sequence_ consTH
    --                                       --mapM (flip (genCon' cls) stdDerivings) cons

    --                                       genCons cls cons stdDerivings True

    --                                       --GenState.addTHExpression $ thGenerateAccessors name
    --                                       --GenState.addTHExpression $ thRegisterAccessors name
    --                                       --GenState.addTHExpression $ thInstsAccessors name
    --                                       GenState.addComment $ HExpr.Comment $ HComment.H3 $ name ++ " methods"

    --                                       mapM_ genExpr methods

    --                                       return $ HExpr.NOP

    --LExpr.DataNative _ cls cons _classes methods -> do
    --                                       let name        = view LType.name   cls
    --                                           params      = view LType.params cls

    --                                       GenState.setCls cls
    --                                       genCons cls cons stdDerivings False
    --                                       mapM_ genExpr methods
    --                                       return $ HExpr.NOP

    --LExpr.Infix        id name src dst       -> genExpr (LExpr.App id (LExpr.Var 0 name) $ fmap (Arg.Unnamed 0) [src, dst])
    --                                            --HExpr.Infix name <$> genExpr src <*> genExpr dst
    --LExpr.Assignment   _ pat dst             -> HExpr.Arrow <$> genPat pat <*> genCallExpr dst
    --LExpr.RecordUpdate _ src selectors expr  -> genExpr $ (setSteps sels) expr
    --                                            where setter sel exp val = flip (LExpr.App 0) [Arg.Unnamed 0 val]
    --                                                                     $ LExpr.Accessor 0 (LExpr.VarAccessor $ Naming.mkSetName sel) exp
    --                                                  getter sel exp     = flip (LExpr.App 0) []
    --                                                                     $ LExpr.Accessor 0 (LExpr.VarAccessor sel) exp
    --                                                  getSel sel           = foldl (flip($)) src (fmap getter (reverse sel))
    --                                                  setStep       (x:xs) = setter x (getSel xs)
    --                                                  setSteps args@(_:[]) = setStep args
    --                                                  setSteps args@(_:xs) = setSteps xs . setStep args
    --                                                  setSteps          [] = undefined
    --                                                  sels = reverse selectors

    --LExpr.Lit          _ value               -> mkVal <$> genLit value
    --LExpr.Tuple        _ items               -> mkVal . HExpr.Tuple <$> mapM genExpr items -- zamiana na wywolanie funkcji!
    --LExpr.Field        _ name fcls _         -> genType' fcls
    --                                           --cls <- GenState.getCls
    --                                           --let clsName = view LType.name cls
    --                                           --genTypedSafe HExpr.Typed fcls <*> pure (HExpr.Var $ Naming.mkPropertyName clsName name)
    ----LExpr.Field        _ name fcls _         -> do
    ----                                           cls <- GenState.getCls
    ----                                           let clsName = view LType.name cls
    ----                                           genTypedSafe HExpr.Typed fcls <*> pure (HExpr.Var $ Naming.mkPropertyName clsName name)
    ----LExpr.App          _ src args             -> (liftM2 . foldl) HExpr.AppE (getN (length args) <$> genExpr src) (mapM genCallExpr args)
    ----LExpr.App          _ src args            -> HExpr.AppE <$> (HExpr.AppE (HExpr.Var "call") <$> genExpr src) <*> (mkRTuple <$> mapM genCallExpr args)
    ----LExpr.App          _ src args            -> foldr (<*>) (genExpr src) ((fmap.fmap) (HExpr.AppE . (HExpr.AppE (HExpr.VarE "appNext"))) [return $ HExpr.VarE "xxx"]) 
    --LExpr.App          _ src args            -> HExpr.AppE (HExpr.VarE "call") <$> foldl (flip (<*>)) (genExpr src) ((fmap.fmap) (HExpr.AppE . (HExpr.AppE (HExpr.VarE "appNext"))) (map genCallExpr $ fmap (view Arg.arg) args)) 
    --LExpr.Accessor     _ acc dst             -> HExpr.AppE <$> (pure $ mkMemberGetter $ view LExpr.accName acc) <*> genExpr dst --(get0 <$> genExpr dst))
    --LExpr.TypeAlias    _ srcType dstType     -> case srcType of
    --                                                LType.Con _ segments                    -> HExpr.TySynD (last segments) [] <$> genType' dstType
    --                                                LType.App _ (LType.Con _ segments) args -> HExpr.TySynD (last segments) <$> mapM genType' args <*> genType' dstType
    --LExpr.TypeDef      _ srcType dstType     -> case srcType of
    --                                                LType.Con _ segments                    -> HExpr.NewTypeD (last segments) [] <$> (HExpr.Con (last segments) . (:[]) <$> genType' dstType)
    --                                                LType.App _ (LType.Con _ segments) args -> HExpr.NewTypeD (last segments) <$> mapM genType' args <*> (HExpr.Con (last segments) . (:[]) <$> genType' dstType)
    --LExpr.List         _ items               -> if (length items == 1) && (isRange $ items!!0)
    --                                                then mkVal . HExpr.ListE <$> mapM genExpr [items!!0]
    --                                                else if isThereRange then Pass.fail "Arrays with multiple sections are not supported yet!"
    --                                                                     else mkVal . HExpr.ListE <$> mapM genExpr items
    --                                            where isThereRange = any isRange items

    ---- FIXME[wd]: poprawic generacje list - multisection.
    ----LExpr.List         _ items               -> mkVal . arrMod . HExpr.ListE <$> mapM (genExpr . elmod) items
    ----                                            where liftEl el = case el of
    ----                                                      LExpr.RangeFromTo {} -> el
    ----                                                      LExpr.RangeFrom   {} -> el
    ----                                                      _                    -> LExpr.List 0 [el]
    ----                                                  (arrMod, elmod) = if any isRange items
    ----                                                      then (HExpr.AppE (HExpr.Var "concatPure"), liftEl)
    ----                                                      else (Prelude.id, Prelude.id)

                                                

    --LExpr.RangeFromTo _ start end            -> HExpr.AppE . HExpr.AppE (HExpr.Var "rangeFromTo") <$> genExpr start <*> genExpr end
    --LExpr.RangeFrom   _ start                -> HExpr.AppE (HExpr.Var "rangeFrom") <$> genExpr start
    --LExpr.Ref         _ dst                  -> genExpr dst
    --LExpr.RefType     _ typeName name        -> pure $ thTypeRef typeName name
    --LExpr.Native      _ segments             -> pure $ HExpr.Native (join " " $ map genNative segments)
    --LExpr.Typed       _ _cls _expr           -> Pass.fail "Typing expressions is not supported yet." -- Potrzeba uzywac hacku: matchTypes (undefined :: m1(s1(Int)))  (val (5 :: Int))
    --LExpr.NOP         _                      -> pure HExpr.NOP
    ----x                                        -> logger error (show x) *> return HExpr.NOP


--genCon' cls (LExpr.ConD _ conName fields) derivings = do
--    let tpName     = view LType.name cls
--        clsConName = "Cls_" ++ tpName
--        params     = view LType.params cls
--        consClsHE  = HExpr.Con clsConName mempty
--        conSigName = Naming.mkMemSig clsConName conName
--        conDefName = Naming.mkMemDef clsConName conName
--        --FIXME[wd]: to powinno byc zrobione ladniej - z nowym AST!
--        getName el = case el of
--            LExpr.Field {} -> Just $ view LExpr.name el
--            _              -> Nothing

--    GenState.addComment $ HExpr.Comment $ HComment.H3 $ tpName ++ "." ++ conName ++ " constructor"

--    consE  <- HExpr.Con conName <$> mapM genExpr fields

--    GenState.addDataType $ HExpr.DataD conName params [consE] derivings
--    GenState.addDataType $ HExpr.DataD clsConName mempty [consClsHE] derivings

--    GenState.addFunction $ HExpr.Function (Naming.con conName) [] (mkAppE [HExpr.VarE "member", mkProxyE conName, mkVal (HExpr.VarE clsConName)])
    
--    GenState.addFunction $ HExpr.Function conSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length fields) paramSig))
--    GenState.addFunction $ HExpr.Function conDefName [] (HExpr.AppE (HExpr.VarE $ "liftCons" ++ show (length fields)) (HExpr.VarE conName))
--    GenState.addTHExpression $ thRegisterMethod clsConName conName

--    GenState.addTHExpression $ thGenerateFieldAccessors conName (fmap getName fields)


--hashDecl :: (PassCtx lab m a) => LDecl lab a -> Pass m (LDecl lab a)
--hashDecl ast@(Label lab decl) = case decl of
--    Decl.Function path sig output body -> return . Label lab
--                                        $ Decl.Function path (NamePat.mapSegments hashSegment sig) output body
--    _                                  -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

--hashSegment = NamePat.mapSegmentBase hash


--hashPat :: (MonadIO m, Applicative m, Enumerated lab) => LPat lab -> Pass m (LPat lab)
--hashPat ast@(Label lab pat) = case pat of
--    Pat.Var name -> return . Label lab . Pat.Var $ fromString $ hash name
--    _            -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------


--instance (PassCtx lab m a) => AST.Traversal Pass (Pass m) (LDecl lab a) (LDecl lab a) where
--    traverseM _ = hashDecl

--instance (MonadIO m, Applicative m, Enumerated lab) => AST.Traversal Pass (Pass m) (LPat lab) (LPat lab) where
--    traverseM _ = hashPat


