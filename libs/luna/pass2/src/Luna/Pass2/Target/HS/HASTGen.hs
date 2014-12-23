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
import           Luna.ASTNew.Decl             (Decl, LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
import qualified Luna.ASTNew.Type             as Type
import           Luna.ASTNew.Type             (Type, LType)
import qualified Luna.ASTNew.Pat              as Pat
import           Luna.ASTNew.Pat              (LPat, Pat)
import           Luna.ASTNew.Expr             (LExpr, Expr)
import qualified Luna.ASTNew.Expr             as Expr
import qualified Luna.ASTNew.Lit              as Lit
import qualified Luna.ASTNew.Lit.Number       as Number
import           Luna.ASTNew.Lit              (Lit, LLit)
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

import           Luna.ASTNew.Arg              (Arg(Arg))
import qualified Luna.ASTNew.Arg              as Arg

import qualified Luna.Parser.Parser           as Parser
import           Luna.ASTNew.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import qualified Luna.ASTNew.Name.Pattern     as NamePat

import qualified Luna.Pass2.Target.HS.HASTGen.State as State
import           Luna.Pass2.Target.HS.HASTGen.State (addComment, setModule, getModule, regFunc, regTHExpr, pushCtx, popCtx, getCtx, withCtx)
import           Luna.ASTNew.Name.Hash              (Hashable, hash)
--import qualified Luna.Target.HS.Host.NamingOld                          as Naming
import qualified Luna.Target.HS.Host.Naming2 as Naming
import qualified Luna.Data.HAST.Builder.TH    as TH
import qualified Luna.Data.HAST.Builder.Utils as HUtils
import qualified Luna.ASTNew.Name.Pattern as NamePat
import           Luna.ASTNew.Name.Pattern (NamePat)
import           Data.Maybe (isNothing)
import qualified Luna.ASTNew.Foreign         as Foreign
import           Luna.ASTNew.Foreign         (Foreign(Foreign))


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

mkVal    = HExpr.AppE (HExpr.Var "val")


pass :: Ctx m a v => Pass State.GenState (Unit (LModule a (LExpr a v)) -> PassResult m HExpr)
pass = Pass "HASTGen" "Haskell AST generator" def genUnit

genUnit (Unit m) = genModule m



stdDerivings :: [Deriving]
stdDerivings = [Deriving.Show, Deriving.Eq, Deriving.Ord, Deriving.Generic]


genModule :: Ctx m a v => LModule a (LExpr a v) -> PassResult m HExpr
genModule (Label lab (Module path body)) = withCtx path $ do
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
                $ HModule.mk (toList path)
        --params  = view LType.params cls
        --modCon  = LExpr.ConD 0 name fields
        modConName = Naming.mkModCons (hash path)
        modData = Label 0 (Decl.singleData $ fromText $ hash path) :: Num a => LDecl a (LExpr b ())
    
    State.setModule mod
    genDecl modData
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
    
    ----regTHExpr $ thGenerateAccessors name
    ----regTHExpr $ thRegisterAccessors name
    ----regTHExpr $ thInstsAccessors name

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Module methods"
    --mapM_ genExpr methods
    
    --mapM_ (genExpr >=> State.addImport) imports
    when (path == "Main") $ do
        State.addComment $ H1 "Main module wrappers"
        regFunc $ mainf modConName
    State.getModule

mainf modname = HExpr.val "main" $ HExpr.AppE (HExpr.VarE "mainMaker") (HExpr.VarE modname)

genCons :: (Monad m, Applicative m, MonadState State.GenState m)
        => Text -> [Text] -> [Decl.LCons a e] -> [Deriving] -> Bool -> m ()
genCons name params cons derivings makeDataType = do
    let conDecls   = fmap (view Label.element) cons
        clsConName = Naming.mkCls name
        genData (Decl.Cons conName fields) = HExpr.Con (fromString $ toString conName) 
                                           <$> mapM genField fields
    
        getName (Label _ (Decl.Field _ mname _)) = fmap convVar mname

        genConData (Decl.Cons (convVar -> conName) fields) = do
            let fieldNum   = length fields
                conSigName = Naming.mkMemSig clsConName conName
                conDefName = Naming.mkMemDef clsConName conName
                cons       = HExpr.val (Naming.mkCons conName)
                           $ HExpr.app (HExpr.VarE Naming.member) 
                                       [HExpr.proxy conName, HExpr.AppE HUtils.val (HExpr.VarE clsConName)]
                consSig    = HExpr.val conSigName
                           $ HExpr.rtuple (selfSig : replicate fieldNum paramSig)
                consDef    = HExpr.val conDefName
                           $ HExpr.AppE (HExpr.VarE $ liftCons fieldNum)
                                        (HExpr.VarE conName)

            addComment . H3 $ dotname [name, conName] <> " constructor"
            mapM regFunc [cons, consSig, consDef]
            regTHExpr $ TH.mkMethod clsConName conName
            regTHExpr $ TH.mkFieldAccessors conName (fmap getName fields)

        genField (Label _ (Decl.Field tp name val)) = genType tp

--data Field a e = Field  { _fType    :: LType a , _fName  :: Maybe VNameP, _fVal :: Maybe e } deriving (Show, Eq, Generic, Read)



    addComment . H2 $ name <> " type"
    consE <- mapM genData conDecls
    if makeDataType then State.addDataType $ HExpr.DataD name params consE derivings
                    else State.addComment  $ H5 "datatype provided externally"
    addClsDataType clsConName derivings
    mapM_ genConData conDecls



addClsDataType clsConName derivings = do
    let consClsHE  = HExpr.Con clsConName mempty
    State.addDataType $ HExpr.DataD clsConName mempty [consClsHE] derivings


liftCons num = "liftCons" <> fromString (show num)
mkArg    = HExpr.Var "mkArg"
paramSig = HExpr.TypedE mkArg (HExpr.VarT "Param")
selfSig  = HExpr.TypedE mkArg (HExpr.AppT (HExpr.VarT "NParam") (HExpr.LitT $ HLit.String Naming.self)) 

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

    let func    = HExpr.Function memDefName [HExpr.rtuple sigHPats] (HExpr.DoBlock fBody)
        funcSig = HExpr.val memSigName (HExpr.rtuple (selfSig : replicate (length sigArgs - 1) paramSig))
        funcReg = TH.registerMethod tpName name
    mapM_ regFunc [func, funcSig, funcReg]


genForeign (Foreign target a) = case target of
    Foreign.CPP     -> Pass.fail "C++ foreign interface is not supported yet."
    Foreign.Haskell -> case a of
        Decl.FFunc funcDecl -> genFunc funcDecl genFFuncBody False

genFFuncBody txt _ = pure $ [HExpr.Native txt]

genFuncBody :: Ctx m a v => [LExpr a v] -> Maybe (LType a) -> PassResult m [HExpr]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> (:) <$> genExpr x
                <*> case unwrap x of
                      Expr.Assignment {} -> (:[]) <$> pure (mkVal $ HExpr.Tuple [])
                      _                  -> pure []
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output



--genFuncTopLevelExpr :: LExpr -> GenPass HExpr
--genFuncTopLevelExpr expr = case expr of
--    LExpr.RecordUpdate _ (LExpr.Var _ name) _ _ -> genFuncTopLevelExpr $ LExpr.Assignment 0 (LPat.Var 0 name) expr
--    _                                           -> genExpr expr


--genCallExpr :: LExpr -> GenPass HExpr
--genCallExpr e = trans <$> genExpr e where
--    trans = case e of
--        LExpr.App        {} -> id
--        LExpr.Native     {} -> id
--        LExpr.Assignment {} -> id
--        LExpr.Lambda     {} -> id
--        _                   -> id
--        --_                   -> call0
--    id     = Prelude.id
--    --call0  = HExpr.AppE (HExpr.Var "call0")
--    --ret    = HExpr.AppE $ HExpr.Var "return"

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
    --       State.addComment $ HExpr.Comment $ HComment.H2 $ "Method: " ++ tpName ++ "." ++ name2
    --       -----------------------------------------
    --       -- FIXME[wd]: genFuncSig is naive, it should not handle such cases as (a::Int)::Int
    --       --            it should be implemented using State monad, so HASTGen has to change
    --       --            the architecture a little bit
    --       sigTerms <- mapM genFuncSig inputs
    --       let hInputs   = map fst sigTerms
    --       let typeHints = concat $ map snd sigTerms
    --       --logger error (show typeHints)
    --       let mkTypeHint (id,e) = HExpr.AppE (HExpr.AppE (HExpr.VarE "typeMatch") (HExpr.VarE $ "_v_" ++ show id))
    --                                          (HExpr.Typed e $ HExpr.VarE "undefined")
    --           hTypeHints = map mkTypeHint typeHints
    --       -----------------------------------------
    --       let fBody = (((emptyHExpr : hTypeHints) ++ ) <$> genFuncBody body output)                                   
    --       -- hInputs
    --       f <- HExpr.Function memDefName [foldr biTuple (HExpr.Tuple []) hInputs] <$> (HExpr.DoBlock <$> fBody)
    --       regFunc f
    --       regFunc $ HExpr.Function memSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length inputs - 1) paramSig))
    --       regTHExpr $ thRegisterMethod tpName name2
    --       return f

--genFuncSig :: Decl.FuncSig a e -> HExpr
--genFuncSig (NamePat pfx base segs) = undefined

--genType :: Bool -> LType -> GenPass HExpr
--genType safeTyping t = case t of
--    LType.Var     _ name     -> return $ thandler (HExpr.Var  name)
--    LType.Con     _ segments -> return $ thandler (HExpr.ConE segments)

--    LType.Tuple   _ items    -> HExpr.Tuple <$> mapM (genType safeTyping) items
--    LType.App     _ src args -> (liftM2 . foldl) (HExpr.AppT) (genType safeTyping src) (mapM (genType safeTyping) args)
--    LType.Unknown _          -> logger critical "Cannot generate code for unknown type1" *> Pass.fail "Cannot generate code for unknown type"
--    --_                        -> fail $ show t
--    where mtype    = HExpr.VarT $ if safeTyping then "Pure" else "m_" ++ show (view LType.id t)
--          stype    = HExpr.VarT $ if safeTyping then "Safe" else "s_" ++ show (view LType.id t)
--          thandler = HExpr.AppT mtype . HExpr.AppT stype

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
           Pat.Var name -> pure $ HExpr.Var (convVar name)
           _            -> genPatGen genRec ast

genPatGen genRec (Label lab pat) = case pat of
    Pat.App         src args -> HExpr.appP <$> genRec src <*> mapM genRec args
    Pat.Var         name     -> pure $ HExpr.Var (Naming.mkVar $ convVar name)
    Pat.Typed       pat cls  -> HExpr.Typed <$> genRec pat <*> genType cls
    Pat.Tuple       items    -> (HExpr.ViewP $ "extractTuple" <> (fromString $ show (length items))) . HExpr.TupleP <$> mapM genRec items
    Pat.Lit         value    -> genLit value
    Pat.Wildcard             -> pure $ HExpr.WildP
    Pat.RecWildcard          -> pure $ HExpr.RecWildP
    Pat.Con         name     -> pure $ HExpr.ConP $ convVar name
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
    Type.Var      name            -> pure $ HExpr.Var (convVar name)
    Type.Con      segments        -> pure $ HExpr.ConE (fmap convVar segments)
    Type.Tuple    items           -> HExpr.Tuple <$> mapM genType items
    Type.App      src      args   -> HExpr.app <$> genType src <*> mapM genType args
    --Type.Function inputs   output -> 
    --Type.List     item            -> 
    --Type.Wildcard                 -> 
    --where mtype    = HExpr.VarT $ "m_" <> fromString (show (777::Int))
          --stype    = HExpr.VarT $ "s_" <> fromString (show (777::Int))
          --thandler = HExpr.AppT mtype . HExpr.AppT stype



mkFlattenCtx = HExpr.AppE (HExpr.Var "polyJoin")
mkLiftf1     = HExpr.AppE (HExpr.Var "liftF1")


genExpr :: Ctx m a v => LExpr a v -> PassResult m HExpr
genExpr (Label lab expr) = case expr of
    Expr.Var (Expr.Variable name _)          -> pure . HExpr.Var $ Naming.mkVar $ hash name
    Expr.Cons  name                          -> pure $ HExpr.Var $ Naming.mkCons $ hash name
    Expr.Case  expr match                    -> mkFlattenCtx <$> (HExpr.AppE <$> lamFunc <*> genExpr expr)
                                                   where passVar = HExpr.VarE "a"
                                                         caseE   = HExpr.CaseE passVar <$> body'
                                                         body    = mapM genMatch match
                                                         lam     = HExpr.Lambda [passVar] <$> caseE
                                                         lamFunc = mkLiftf1 <$> lam
                                                         body'   = (\a -> a ++ [HExpr.Match HExpr.WildP (HExpr.AppE (HExpr.VarE "error") (HExpr.Lit $ HLit.String "TODO (!!!) Main.luna: path/Main.luna:(...,...)-(...,...): Non-exhaustive patterns in case"))]) <$> body
                                                         genMatch (unwrap -> Expr.Match pat body) = HExpr.Match <$> genPat pat <*> (HExpr.DoBlock <$> mapM genExpr body)

    --Expr.Lambda id inputs output body   -> do
    --                                        let fname      = Naming.mkLamName $ show id
    --                                            hName      = Naming.mkHandlerFuncName fname
    --                                            cfName     = mkCFLName fname
    --                                            argNum     = length inputs

    --                                        --State.addDataType $ HExpr.DataD cfName [] [HExpr.Con cfName []] [Deriving.Show]

    --                                        f  <-   HExpr.Assignment (HExpr.Var fname)
    --                                                <$> ( HExpr.Lambda <$> (mapM genExpr inputs)
    --                                                                   <*> (HExpr.DoBlock <$> ((emptyHExpr :) <$> genFuncBody body output))
    --                                                    )
    --                                        --regFunc f

    --                                        --regTHExpr $ thRegisterFunction fname argNum []
    --                                        --regTHExpr $ thClsCallInsts fname argNum (0::Int)

    --                                        --return $ HExpr.Var hName
    --                                        return $ HExpr.LetExpr HExpr.NOP
    Expr.Grouped expr                      -> genExpr expr
    Expr.Assignment   dst src              -> HExpr.Arrow <$> genPat dst <*> genExpr src
    Expr.RecUpd  name fieldUpdts           -> case fieldUpdts of
        (fieldUpdt:[]) -> genField fieldUpdt
        _              -> Pass.fail "Multi fields updates are not supported yet"
        where src                  = Label 0 $ Expr.Var $ Expr.Variable name mempty
              setter exp field val = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName $ fromText . Naming.setter $ hash field) exp) [Expr.AppArg Nothing val]
              getter exp field     = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName field) exp) []
              getSel sel           = foldl (flip($)) src (fmap (flip getter) (reverse sel))
              setStep       (x:xs) = setter (getSel xs) x
              setSteps args@(_:[]) = setStep args
              setSteps args@(_:xs) = setSteps xs . setStep args
              setSteps          [] = undefined
              genField (Expr.FieldUpd sels expr) = genExpr $ setSteps (reverse sels) expr
    Expr.Lit          value               -> mkVal <$> genLit value
    Expr.Tuple        items               -> mkVal . HExpr.Tuple <$> mapM genExpr items
    Expr.App npat@(NamePat pfx base args)        -> HExpr.AppE (HExpr.VarE "call") <$> (foldl (flip (<*>)) (genExpr $ NamePat.segBase base) $ (fmap.fmap) (HExpr.AppE . (HExpr.AppE (HExpr.VarE "appNext"))) (fmap genArg $ NamePat.args npat))
                                                    where genArg (Expr.AppArg mname expr) = (genExpr expr) -- nameMod mname <*> (genExpr expr)
                                                          nameMod mname = case mname of
                                                              Nothing -> return $ HExpr.AppE (HExpr.VarE "unnamed")
                                                              Just n  -> Pass.fail "No suppert for named args yet!" -- return $ HExpr.AppE (HExpr.VarE "named")
    Expr.Accessor     acc src             -> HExpr.AppE <$> (pure $ mkMemberGetter $ hash acc) <*> genExpr src --(get0 <$> genExpr src))
    Expr.List         lst                 -> case lst of
                                                 Expr.SeqList items -> mkVal . HExpr.ListE <$> mapM genExpr items
                                                 Expr.RangeList {}  -> Pass.fail "Range lists are not supported yet"
    Expr.Typed       _cls _expr           -> Pass.fail "Typing expressions is not supported yet." -- Potrzeba uzywac hacku: matchTypes (undefined :: m1(s1(Int)))  (val (5 :: Int))
    Expr.Decl _                               -> Pass.fail "Nested declarations are not supported yet"
    Expr.Ref  _                               -> Pass.fail "References are not supported yet"
    p -> Pass.fail $ "Cannot construct: " <> show p


mkMemberGetter name = HExpr.AppE (HExpr.VarE "member") $ HExpr.proxy name




-- FIXME[wd]: ugly code
genLit :: (Show a, Monad m) => LLit a -> PassResult m HExpr
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
    where mkLit cons hast = return $ HExpr.TypedE (HExpr.Lit hast) (HExpr.ConT cons)
    --where mkLit cons hast = return $ HExpr.Lit hast




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

--    State.addComment $ HExpr.Comment $ HComment.H3 $ tpName ++ "." ++ conName ++ " constructor"

--    consE  <- HExpr.Con conName <$> mapM genExpr fields

--    State.addDataType $ HExpr.DataD conName params [consE] derivings
--    State.addDataType $ HExpr.DataD clsConName mempty [consClsHE] derivings

--    regFunc $ HExpr.Function (Naming.con conName) [] (mkAppE [HExpr.VarE "member", mkProxyE conName, mkVal (HExpr.VarE clsConName)])
    
--    regFunc $ HExpr.Function conSigName [] (foldr biTuple (HExpr.Tuple []) (selfSig : replicate (length fields) paramSig))
--    regFunc $ HExpr.Function conDefName [] (HExpr.AppE (HExpr.VarE $ "liftCons" ++ show (length fields)) (HExpr.VarE conName))
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


