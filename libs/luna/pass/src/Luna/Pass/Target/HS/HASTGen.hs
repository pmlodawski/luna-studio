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

module Luna.Pass.Target.HS.HASTGen where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
--import qualified Luna.Syntax.Enum             as Enum
import           Luna.Syntax.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl             as Decl
import           Luna.Syntax.Decl             (Decl, LDecl, Field(Field))
import qualified Luna.Syntax.Module           as Module
import           Luna.Syntax.Module           (Module(Module), LModule)
import           Luna.Syntax.Unit             (Unit(Unit))
import qualified Luna.Syntax.Label            as Label
import           Luna.Syntax.Label            (Label(Label))
import qualified Luna.Syntax.Type             as Type
import           Luna.Syntax.Type             (Type, LType)
import qualified Luna.Syntax.Pat              as Pat
import           Luna.Syntax.Pat              (LPat, Pat)
import           Luna.Syntax.Expr             (LExpr, Expr)
import qualified Luna.Syntax.Expr             as Expr
import qualified Luna.Syntax.Lit              as Lit
import qualified Luna.Syntax.Lit.Number       as Number
import           Luna.Syntax.Lit              (Lit, LLit)
--import qualified Luna.Syntax.Native           as Native
import qualified Luna.Syntax.Name             as Name
--import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import qualified Luna.Data.HAST.Expr                         as HE
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

import           Luna.Syntax.Arg              (Arg(Arg))
import qualified Luna.Syntax.Arg              as Arg

import qualified Luna.Parser.Parser           as Parser
import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import qualified Luna.Syntax.Name.Pattern     as NamePat

import qualified Luna.Pass.Target.HS.HASTGen.State as State
import           Luna.Pass.Target.HS.HASTGen.State (addComment, setModule, getModule, regFunc, regTHExpr, pushCtx, popCtx, getCtx, withCtx)
import           Luna.Syntax.Name.Hash              (Hashable, hash)
--import qualified Luna.Target.HS.Host.NamingOld                          as Naming
import qualified Luna.Target.HS.Host.Naming2 as Naming
import qualified Luna.Data.HAST.Builder.TH    as TH
import qualified Luna.Data.HAST.Builder.Utils as HUtils
import qualified Luna.Syntax.Name.Pattern as NamePat
import           Luna.Syntax.Name.Pattern (NamePat)
import           Data.Maybe (isNothing)
import qualified Luna.Syntax.Foreign         as Foreign
import           Luna.Syntax.Foreign         (Foreign(Foreign))
import qualified Luna.Syntax.Enum            as Enum

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

-- FIXME[wd]: remove (Show ...) after pass implementation

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data HASTGen = HASTGen

type PassResult           m   = PassMonad State.GenState m
type PassCtx          lab m a = (Enumerated lab, Traversal m a)
type Traversal            m a = (Pass.PassCtx m, AST.Traversal        HASTGen (PassResult m) a a)
type DefaultTraversal     m a = (Pass.PassCtx m, AST.DefaultTraversal HASTGen (PassResult m) a a)

type Ctx m a v = (Monad m, Show a, Show v, Monoid v, Num a)

type HE = HE.Expr

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

mkVal    = HE.AppE (HE.Var "val")


pass :: Ctx m a v => Pass State.GenState (Unit (LModule a (LExpr a v)) -> PassResult m HE)
pass = Pass "HASTGen" "Haskell AST generator" def genUnit

genUnit (Unit m) = genModule m



stdDerivings :: [Deriving]
stdDerivings = [Deriving.Show, Deriving.Eq, Deriving.Ord, Deriving.Generic]


genModule :: Ctx m a v => LModule a (LExpr a v) -> PassResult m HE
genModule (Label lab (Module path body)) = withCtx path $ do
    let mod     = HModule.addImport ["Luna", "Target", "HS"]
                $ HModule.addExt HExtension.DataKinds
                $ HModule.addExt HExtension.DeriveDataTypeable
                $ HModule.addExt HExtension.DeriveGeneric
                $ HModule.addExt HExtension.NoMonomorphismRestriction
                $ HModule.addExt HExtension.FlexibleContexts
                $ HModule.addExt HExtension.FlexibleInstances
                $ HModule.addExt HExtension.GADTs
                $ HModule.addExt HExtension.RebindableSyntax
                $ HModule.addExt HExtension.TemplateHaskell
                $ HModule.addExt HExtension.UndecidableInstances
                $ HModule.addExt HExtension.ViewPatterns
                $ HModule.addExt HExtension.MultiParamTypeClasses
                $ HModule.addExt HExtension.CPP
                -- $ HModule.addExt HExtension.DysfunctionalDependencies
                $ HModule.mk (toList path)
        --params  = view LType.params cls
        --modCon  = LExpr.ConD 0 name fields
        modConName = Naming.mkModCons (hash path)
        modData = Label 0 (Decl.singleData $ fromText $ hash path) :: Num a => LDecl a (LExpr b ())
    

    State.setModule mod

    State.regPragma (HE.Pragma $ HE.Include "pragmas.cpp")
    genDecl modData
    mapM_ genDecl body

    when (path == "Main") $ do
        State.addComment $ H1 "Main module wrappers"
        regFunc $ mainf modConName
    State.getModule

mainf modname = HE.val "main" $ HE.AppE (HE.VarE "mainMaker") (HE.VarE modname)

genCons :: (Monad m, Applicative m, MonadState State.GenState m)
        => Text -> [Text] -> [Decl.LCons a e] -> [Deriving] -> Bool -> m ()
genCons name params cons derivings makeDataType = do
    let conDecls   = fmap (view Label.element) cons
        clsConName = Naming.mkCls name
        genData (Decl.Cons conName fields) = HE.Con (fromString $ toString conName) 
                                           <$> mapM genField fields
    
        getName (Label _ (Decl.Field _ mname _)) = fmap convVar mname

        genConData (Decl.Cons (convVar -> conName) fields) = do
            let fieldNum   = length fields
                conSigName = Naming.mkMemSig clsConName conName
                conDefName = Naming.mkMemDef clsConName conName
                cons       = HE.val (Naming.mkCons conName)
                           $ HE.app (mkMemberGetter conName) 
                                       [HE.AppE HUtils.val (HE.VarE clsConName)]
                consSig    = HE.val conSigName
                           $ HE.rtuple (selfSig : replicate fieldNum paramSig)
                consDef    = HE.val conDefName
                           $ HE.AppE (HE.VarE $ liftCons fieldNum)
                                        (HE.VarE conName)

            addComment . H3 $ dotname [name, conName] <> " constructor"
            mapM regFunc [cons, consSig, consDef]
            regTHExpr $ TH.mkMethod clsConName conName

        getConDesc (Decl.Cons (convVar -> conName) fields) = (conName, fmap getName fields)
        conDescs   = fmap getConDesc conDecls
        fieldNames = Set.toList . Set.fromList . catMaybes . concat $ fmap snd conDescs :: [Text]
        genField (Label _ (Decl.Field tp name val)) = genType tp

    addComment . H2 $ name <> " type"
    consE <- mapM genData conDecls
    if makeDataType then State.addDataType $ HE.DataD name params consE derivings
                    else State.addComment  $ H5 "datatype provided externally"
    addClsDataType clsConName derivings
    mapM_ genConData conDecls

    when (not $ null fieldNames) $ do
        State.addComment . H3 $ name <> " accessors"
        regTHExpr $ TH.mkFieldAccessors2 name conDescs
        regTHExpr $ TH.mkRegFieldAccessors name fieldNames



addClsDataType clsConName derivings = do
    let consClsHE  = HE.Con clsConName mempty
    State.addDataType $ HE.DataD clsConName mempty [consClsHE] derivings



liftCons num = "liftCons" <> fromString (show num)
mkArg    = HE.Var "mkArg"
--paramSig = HE.TypedE mkArg (HE.VarT "Param")
paramSig    = HE.PragmaE "param" []
nparamSig n = HE.PragmaE "nparam" [HE.Lit $ HLit.String n]
selfSig     = nparamSig "self"

--data Cons  a e = Cons   { _consName :: CNameP   , _fields :: [LField a e]                  } deriving (Show, Eq, Generic, Read)

dotname names = mjoin "." names

convVar :: (Wrapper t, Hashable a Text) => t a -> Text
convVar = hash . unwrap



genDecl :: Ctx m a v => LDecl a (LExpr a v)-> PassResult m ()
genDecl ast@(Label lab decl) = case decl of
    Decl.Data (Decl.DataDecl (convVar -> name) params cons defs) -> withCtx (fromText name) $ do
        genCons name (fmap convVar params) cons stdDerivings True
        addComment $ H3 $ name <> " methods"
        --mapM_ genExpr methods
    
    Decl.Func funcDecl -> genFunc funcDecl genFuncBody True
    Decl.Foreign fdecl -> genForeign fdecl
    Decl.Pragma     {} -> return ()

genFunc (Decl.FuncDecl (fmap convVar -> path) sig output body) bodyBuilder mkVarNames = do
    ctx <- getCtx
    let tpName = if (null path)
        then case ctx of
            Nothing -> ""
            Just n  -> hash n
        else (path!!0) -- FIXME[wd]: needs name resolver
                       -- in case of defining extensionmethod inside of a class
                       -- maybe it should have limited scope to all classes inside that one?
        argNum     = length (NamePat.segments sig)
        name       = hash sig
        memDefName = Naming.mkMemDef tpName name
        memSigName = Naming.mkMemSig tpName name
        sigArgs    = NamePat.args sig
        sigPats    = fmap (view Arg.pat) sigArgs
        sigPatsUntyped = fmap splitPatTypes sigPats
    sigHPats   <- if mkVarNames then mapM genPat sigPatsUntyped
                                else mapM genPatNoVarnames sigPatsUntyped

    when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

    fBody <- bodyBuilder body output

    let pfx = case tpName of
                   "" -> ""
                   s  -> s <> "."
    addComment $ H2 $ "Method: " <> pfx <> name

    let func    = HE.Function memDefName [HE.rtuple sigHPats] (HE.DoBlock fBody)
        funcSig = HE.val memSigName (HE.rtuple (selfSig : replicate (length sigArgs - 1) paramSig))
        funcReg = TH.registerMethod tpName name
    mapM_ regFunc [func, funcSig, funcReg]


genForeign (Foreign target a) = case target of
    Foreign.CPP     -> Pass.fail "C++ foreign interface is not supported yet."
    Foreign.Haskell -> case a of
        Decl.FFunc funcDecl -> genFunc funcDecl genFFuncBody False

genFFuncBody txt _ = pure $ [HE.Native txt]

genFuncBody :: Ctx m a v => [LExpr a v] -> Maybe (LType a) -> PassResult m [HE]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> (:) <$> genExpr x
                <*> case unwrap x of
                      Expr.Assignment {} -> (:[]) <$> pure (mkVal $ HE.Tuple [])
                      _                  -> pure []
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output



--genFuncTopLevelExpr :: LExpr -> GenPass HE
--genFuncTopLevelExpr expr = case expr of
--    LExpr.RecordUpdate _ (LExpr.Var _ name) _ _ -> genFuncTopLevelExpr $ LExpr.Assignment 0 (LPat.Var 0 name) expr
--    _                                           -> genExpr expr


--genCallExpr :: LExpr -> GenPass HE
--genCallExpr e = trans <$> genExpr e where
--    trans = case e of
--        LExpr.App        {} -> id
--        LExpr.Native     {} -> id
--        LExpr.Assignment {} -> id
--        LExpr.Lambda     {} -> id
--        _                   -> id
--        --_                   -> call0
--    id     = Prelude.id
--    --call0  = HE.AppE (HE.Var "call0")
--    --ret    = HE.AppE $ HE.Var "return"

--type FuncSig a e = ArgPat (LPat a) e
--type ArgPat  pat expr = NamePat     SegmentName (Arg pat expr)
--data NamePat base arg = NamePat { _prefix   :: Maybe arg
--                                , _base     :: Segment base arg
--                                , _segments :: [Segment SegmentName arg]
--                                } derivin
--        return ()

    --LExpr.Function _ path name
    --                 inputs output body  -> do
    --       let name2 = Name.unified name
    --       cls <- State.getCls
    --       let tpName = if (null path)
    --               then cls ^. LType.name
    --               else (path!!0) -- FIXME[wd]: needs name resolver
    --           argNum     = length inputs
    --           memDefName      = Naming.mkMemDef tpName name2
    --           memSigName      = Naming.mkMemSig tpName name2
    --       when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."
    --       State.addComment $ HE.Comment $ HComment.H2 $ "Method: " ++ tpName ++ "." ++ name2
    --       -----------------------------------------
    --       -- FIXME[wd]: genFuncSig is naive, it should not handle such cases as (a::Int)::Int
    --       --            it should be implemented using State monad, so HASTGen has to change
    --       --            the architecture a little bit
    --       sigTerms <- mapM genFuncSig inputs
    --       let hInputs   = map fst sigTerms
    --       let typeHints = concat $ map snd sigTerms
    --       --logger error (show typeHints)
    --       let mkTypeHint (id,e) = HE.AppE (HE.AppE (HE.VarE "typeMatch") (HE.VarE $ "_v_" ++ show id))
    --                                          (HE.Typed e $ HE.VarE "undefined")
    --           hTypeHints = map mkTypeHint typeHints
    --       -----------------------------------------
    --       let fBody = (((emptyHExpr : hTypeHints) ++ ) <$> genFuncBody body output)                                   
    --       -- hInputs
    --       f <- HE.Function memDefName [foldr biTuple (HE.Tuple []) hInputs] <$> (HE.DoBlock <$> fBody)
    --       regFunc f
    --       regFunc $ HE.Function memSigName [] (foldr biTuple (HE.Tuple []) (selfSig : replicate (length inputs - 1) paramSig))
    --       regTHExpr $ thRegisterMethod tpName name2
    --       return f

--genFuncSig :: Decl.FuncSig a e -> HE
--genFuncSig (NamePat pfx base segs) = undefined

--genType :: Bool -> LType -> GenPass HE
--genType safeTyping t = case t of
--    LType.Var     _ name     -> return $ thandler (HE.Var  name)
--    LType.Con     _ segments -> return $ thandler (HE.ConE segments)

--    LType.Tuple   _ items    -> HE.Tuple <$> mapM (genType safeTyping) items
--    LType.App     _ src args -> (liftM2 . foldl) (HE.AppT) (genType safeTyping src) (mapM (genType safeTyping) args)
--    LType.Unknown _          -> logger critical "Cannot generate code for unknown type1" *> Pass.fail "Cannot generate code for unknown type"
--    --_                        -> fail $ show t
--    where mtype    = HE.VarT $ if safeTyping then "Pure" else "m_" ++ show (view LType.id t)
--          stype    = HE.VarT $ if safeTyping then "Safe" else "s_" ++ show (view LType.id t)
--          thandler = HE.AppT mtype . HE.AppT stype

--test :: Int

-- FIXME[wd]: powinno byc zaimplementowane przy pomocy jakis sprytnych traversali
splitPatTypes :: LPat a -> LPat a
splitPatTypes (Label lab pat) = case pat of
    Pat.Typed       pat cls  -> splitPatTypes pat
    Pat.App         src args -> Label lab $ Pat.App (splitPatTypes src) (fmap splitPatTypes args)
    Pat.Var         name     -> Label lab $ pat
    Pat.Tuple       items    -> Label lab $ Pat.Tuple (fmap splitPatTypes items)
    Pat.Lit         value    -> Label lab $ pat
    Pat.Wildcard             -> Label lab $ pat
    Pat.RecWildcard          -> Label lab $ pat
    Pat.Con         name     -> Label lab $ pat
    Pat.Grouped     p        -> Label lab $ Pat.Grouped $ splitPatTypes p


genPat = genPatGen genPat

genPatNoVarnames = genRec
    where genRec ast@(Label lab pat) = case pat of
           Pat.Var name -> pure $ HE.Var (convVar name)
           _            -> genPatGen genRec ast

genPatGen genRec (Label lab pat) = case pat of
    Pat.App         src args -> HE.appP <$> genRec src <*> mapM genRec args
    Pat.Var         name     -> pure $ HE.Var (Naming.mkVar $ convVar name)
    Pat.Typed       pat cls  -> HE.Typed <$> genRec pat <*> genType cls
    Pat.Tuple       items    -> (HE.ViewP $ "extractTuple" <> (fromString $ show (length items))) . HE.TupleP <$> mapM genRec items
    Pat.Lit         value    -> genLit value
    Pat.Wildcard             -> pure $ HE.WildP
    Pat.RecWildcard          -> pure $ HE.RecWildP
    Pat.Con         name     -> pure $ HE.ConP $ convVar name
    Pat.Grouped     p        -> genRec p


--data Pat a 
--    = App         { _src   :: LPat a    , _args :: [LPat a] }
--    | Typed       { _pat   :: LPat a    , _cls  :: LType a  }
--    | Grouped     { _pat   :: LPat a                        }
--    | Lit         { _lit   :: L a Lit                       }
--    | Tuple       { _items :: [LPat a ]                     }
--    | Con         { _cname :: CName                         }
--    | Var         { _vname :: VName                         }
--    | Wildcard 
--    | RecWildcard
--    deriving (Show, Eq, Generic, Read, Ord)



genType (Label lab t) = case t of
    Type.Var      name            -> pure $ HE.Var (convVar name)
    Type.Con      segments        -> pure $ HE.ConE (fmap convVar segments)
    Type.Tuple    items           -> HE.Tuple <$> mapM genType items
    Type.App      src      args   -> HE.app <$> genType src <*> mapM genType args
    Type.List     item            -> HE.ListT <$> genType item
    --Type.Function inputs   output -> 
    --Type.List     item            -> 
    --Type.Wildcard                 -> 
    --where mtype    = HE.VarT $ "m_" <> fromString (show (777::Int))
          --stype    = HE.VarT $ "s_" <> fromString (show (777::Int))
          --thandler = HE.AppT mtype . HE.AppT stype



mkFlattenCtx = HE.AppE (HE.Var "polyJoin")
mkLiftf1     = HE.AppE (HE.Var "liftF1")


genExpr :: Ctx m a v => LExpr a v -> PassResult m HE
genExpr (Label lab expr) = case expr of
    Expr.Var (Expr.Variable name _)          -> pure . HE.Var $ Naming.mkVar $ hash name
    Expr.Cons  name                          -> pure $ HE.Var $ Naming.mkCons $ hash name
    Expr.Case  expr match                    -> mkFlattenCtx <$> (HE.AppE <$> lamFunc <*> genExpr expr)
                                                   where passVar = HE.VarE "a"
                                                         caseE   = HE.CaseE passVar <$> body'
                                                         body    = mapM genMatch match
                                                         lam     = HE.Lambda [passVar] <$> caseE
                                                         lamFunc = mkLiftf1 <$> lam
                                                         body'   = (\a -> a ++ [HE.Match HE.WildP (HE.AppE (HE.VarE "error") (HE.Lit $ HLit.String "TODO (!!!) Main.luna: path/Main.luna:(...,...)-(...,...): Non-exhaustive patterns in case"))]) <$> body
                                                         genMatch (unwrap -> Expr.Match pat body) = HE.Match <$> genPat pat <*> (HE.DoBlock <$> mapM genExpr body)

    Expr.Lambda inputs output body   -> do
                                            let fname      = Naming.mkLamName $ fromString $ show id
                                                hName      = Naming.mkHandlerFuncName fname
                                                --cfName     = mkCFLName fname
                                                argNum     = length inputs

                                            --State.addDataType $ HE.DataD cfName [] [HE.Con cfName []] [Deriving.Show]

                                            f  <-   HE.Assignment (HE.Var fname)
                                                    <$> ( HE.Lambda <$> pure [] -- (mapM genExpr inputs)
                                                                       <*> (HE.DoBlock <$> ((HE.NOP :) <$> genFuncBody body output))
                                                        )


                                            -- using HE.NOP instead emptyHExpr (?) for now !
                                            --regFunc f

                                            --regTHExpr $ thRegisterFunction fname argNum []
                                            --regTHExpr $ thClsCallInsts fname argNum (0::Int)

                                            --return $ HE.Var hName
                                            return $ HE.LetExpr HE.NOP

    Expr.Grouped expr                      -> genExpr expr
    Expr.Assignment   dst src              -> HE.Arrow <$> genPat dst <*> genExpr src
    Expr.RecUpd  name fieldUpdts           -> HE.Arrow (HE.Var $ Naming.mkVar $ hash name) <$> case fieldUpdts of
        (fieldUpdt:[]) -> genField fieldUpdt
        _              -> Pass.fail "Multi fields updates are not supported yet"
        where src                  = Label 0 $ Expr.Var $ Expr.Variable name mempty
              setter exp field val = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName $ fromText . ("set#"<>) $ hash field) exp) [Expr.AppArg Nothing val]
              getter exp field     = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName field) exp) []
              getSel sel           = foldl (flip($)) src (fmap (flip getter) (reverse sel))
              setStep       (x:xs) = setter (getSel xs) x
              setSteps args@(_:[]) = setStep args
              setSteps args@(_:xs) = setSteps xs . setStep args
              setSteps          [] = undefined
              genField (Expr.FieldUpd sels expr) = genExpr $ setSteps (reverse sels) expr
    Expr.Lit          value               -> mkVal <$> genLit value
    Expr.Tuple        items               -> mkVal . HE.Tuple <$> mapM genExpr items
    Expr.App npat@(NamePat pfx base args)        -> HE.AppE (HE.VarE "call") <$> (foldl (flip (<*>)) (genExpr $ NamePat.segBase base) $ (fmap.fmap) (HE.AppE . (HE.AppE (HE.VarE "appNext"))) (fmap genArg $ NamePat.args npat))
                                                    where genArg (Expr.AppArg mname expr) = (genExpr expr) -- nameMod mname <*> (genExpr expr)
                                                          nameMod mname = case mname of
                                                              Nothing -> return $ HE.AppE (HE.VarE "unnamed")
                                                              Just n  -> Pass.fail "No suppert for named args yet!" -- return $ HE.AppE (HE.VarE "named")
    Expr.Accessor     acc src             -> HE.AppE <$> (pure $ mkMemberGetter $ hash acc) <*> genExpr src --(get0 <$> genExpr src))
    Expr.List         lst                 -> case lst of
                                                 Expr.SeqList items -> mkVal . HE.ListE <$> mapM genExpr items
                                                 Expr.RangeList {}  -> Pass.fail "Range lists are not supported yet"
    Expr.Typed       cls expr             -> (\e t -> HE.PragmaE "typed" [e,t]) <$> genExpr expr
                                                                                <*> genType cls
    --Pass.fail "Typing expressions is not supported yet." -- Potrzeba uzywac hacku: matchTypes (undefined :: m1(s1(Int)))  (val (5 :: Int))
    Expr.Decl _                               -> Pass.fail "Nested declarations are not supported yet"
    Expr.Ref  _                               -> Pass.fail "References are not supported yet"
    p -> Pass.fail $ "Cannot construct: " <> show p

    where id = 789 -- Enum.id lab


mkMemberGetter name = HE.PragmaE "member" [HE.Lit $ HLit.String name]




-- FIXME[wd]: ugly code
genLit :: (Show a, Monad m) => LLit a -> PassResult m HE
genLit (Label lab lit) = case lit of
    -- FIXME[wd]: fix the number handling.
    Lit.Number (Number.Number base repr exp sign) -> do
        when (base /= 10) $ Pass.fail "number base different than 10 are not yet supported"
        when (not $ isNothing exp) $ Pass.fail "number exponents are not yet supported"
        let sign' = case sign of
                        Number.Positive -> ""
                        Number.Negative -> "-"
        case repr of
            Number.Float   int frac -> mkLit "Double" (HLit.Float   $ sign' <> fromString int <> "." <> fromString frac)
            Number.Decimal int      -> mkLit "Int"    (HLit.Integer $ sign' <> fromString int)

    --Lit.Integer _ str      -> mkLit "Int"    (HLit.Integer str)
    --Lit.Float   _ str      -> mkLit "Double" (HLit.Float   str)
    Lit.String str      -> mkLit "String" (HLit.String  $ fromString str)
    Lit.Char   char     -> mkLit "Char"   (HLit.Char    char)
    --_ -> fail $ show lit
    where mkLit cons hast = return $ HE.TypedE (HE.Lit hast) (HE.ConT cons)
    --where mkLit cons hast = return $ HE.Lit hast




--genCon' cls (LExpr.ConD _ conName fields) derivings = do
--    let tpName     = view LType.name cls
--        clsConName = "Cls_" ++ tpName
--        params     = view LType.params cls
--        consClsHE  = HE.Con clsConName mempty
--        conSigName = Naming.mkMemSig clsConName conName
--        conDefName = Naming.mkMemDef clsConName conName
--        --FIXME[wd]: to powinno byc zrobione ladniej - z nowym AST!
--        getName el = case el of
--            LExpr.Field {} -> Just $ view LExpr.name el
--            _              -> Nothing

--    State.addComment $ HE.Comment $ HComment.H3 $ tpName ++ "." ++ conName ++ " constructor"

--    consE  <- HE.Con conName <$> mapM genExpr fields

--    State.addDataType $ HE.DataD conName params [consE] derivings
--    State.addDataType $ HE.DataD clsConName mempty [consClsHE] derivings

--    regFunc $ HE.Function (Naming.con conName) [] (mkAppE [HE.VarE "member", mkProxyE conName, mkVal (HE.VarE clsConName)])
    
--    regFunc $ HE.Function conSigName [] (foldr biTuple (HE.Tuple []) (selfSig : replicate (length fields) paramSig))
--    regFunc $ HE.Function conDefName [] (HE.AppE (HE.VarE $ "liftCons" ++ show (length fields)) (HE.VarE conName))
--    regTHExpr $ thRegisterMethod clsConName conName

--    regTHExpr $ thGenerateFieldAccessors conName (fmap getName fields)


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


