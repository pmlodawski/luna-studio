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
{-# LANGUAGE TypeFamilies #-}

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
import qualified Luna.Data.HAST.Extension                    as HExt
--import qualified Luna.Data.HAST.Comment                      as HComment
import           Luna.Data.HAST.Comment (Comment(H1, H2, H3, H4, H5))

import qualified Luna.Data.HAST.Deriving                     as Deriving
import           Luna.Data.HAST.Deriving                     (Deriving)

import           Luna.Pass                    (Pass(Pass), PassMonad)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import           Luna.Syntax.Arg              (Arg(Arg), LArg)
import qualified Luna.Syntax.Arg              as Arg

import qualified Luna.Parser.Parser           as Parser
import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import qualified Luna.Syntax.Name.Pattern     as NamePat

import qualified Luna.Pass.Target.HS.HASTGen.State as State
import           Luna.Pass.Target.HS.HASTGen.State (addComment, setModule, getModule, regFunc, regTHExpr, pushCtx, popCtx, getCtx, withCtx, regDecl)
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

type Ctx m a v = (Enumerated a, Monad m, Monoid v, Num a, MonadIO m, v~())

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
stdDerivings = [ Deriving.Show, Deriving.Eq, Deriving.Ord
               , Deriving.Generic, Deriving.Typeable
               ]


genModule :: Ctx m a v => LModule a (LExpr a v) -> PassResult m HE
genModule (Label lab (Module path body)) = withCtx path $ do
    let mod     = HModule.addImport ["Luna", "Target", "HS"]
                $ foldr HModule.addExt (HModule.mk (toList path))
                $ [ HExt.DataKinds
                  , HExt.DeriveDataTypeable
                  , HExt.DeriveGeneric
                  , HExt.NoMonomorphismRestriction
                  , HExt.FlexibleContexts
                  , HExt.FlexibleInstances
                  , HExt.TypeFamilies
                  , HExt.RebindableSyntax
                  , HExt.TemplateHaskell
                  , HExt.UndecidableInstances
                  , HExt.ViewPatterns
                  , HExt.MultiParamTypeClasses
                  , HExt.CPP
                  , HExt.DysfunctionalDependencies
                  , HExt.ExtendedDefaultRules
                  ]
        modConName = Naming.mkModCons (hash path)
        modData = Label (0::Int) (Decl.singleData $ fromText $ hash path)

    State.setModule mod

    State.regPragma (HE.Pragma $ HE.Include "pragmas.cpp")
    genDecl modData
    mapM_ genDecl body

    when (path == "Main") $ do
        State.addComment $ H1 "Main module wrappers"
        regFunc $ mainf modConName
    State.getModule

mainf modname = HE.val "main" $ HE.AppE (HE.VarE "mainMaker") (HE.VarE modname)

--genCons :: (Monad m, Applicative m, MonadState State.GenState m)
--        => Text -> [Text] -> [Decl.LCons a e] -> [Deriving] -> Bool -> m ()
genCons name params cons derivings makeDataType = do
    let conDecls   = fmap (view Label.element) cons
        clsConName = Naming.mkCls name
        clsConName' = Naming.mkCls' name
        genData (Decl.Cons conName fields) = HE.Con (fromString $ toString conName) 
                                           <$> mapM genField fields
    
        getName (Label _ (Decl.Field _ mname _)) = fmap convVar mname

        genConData (Decl.Cons (convVar -> conName) fields) = do
            let fieldNum   = length fields
                conDefName = Naming.mkMemDef clsConName conName
                cons       = HE.val (Naming.mkCons conName)
                           $ HE.app (mkMemberGetter conName) 
                                       [HE.AppE HUtils.val (HE.VarE clsConName)]
                consDef    = HE.val conDefName
                           $ HE.AppE (HE.VarE $ liftCons fieldNum)
                                     (HE.VarE conName)
                func       = Decl.FuncDecl [fromText clsConName'] 
                            (NamePat Nothing (Segment conName [Arg (Label (0::Int) $ Pat.Var "self") Nothing]) []) 
                            Nothing []
                            
            addComment . H3 $ dotname [name, conName] <> " constructor"
            mapM regFunc [cons, consDef]
            genFuncNoBody func
            --regTHExpr $ TH.mkMethod clsConName conName

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
paramSig    = HE.MacroE "param" []
nparamSig n = HE.AppT (HE.ConT "Named") (HE.Lit $ HLit.String n)
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
    
    Decl.Func funcDecl -> genStdFunc funcDecl
    Decl.Foreign fdecl -> genForeign fdecl
    Decl.Pragma     {} -> return ()

genStdFunc f = genFunc f (Just genFuncBody) True
genFuncNoBody f = genFunc f Nothing True
genFunc (Decl.FuncDecl (fmap convVar -> path) sig output body) bodyBuilder mkVarNames = do
    when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

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
        memFncName = Naming.mkMemFnc tpName name
        sigArgs    = NamePat.args sig


    let pfx = case tpName of
                   "" -> ""
                   s  -> s <> "."
    addComment $ H2 $ "Method: " <> pfx <> name

    funcSig <- genFuncSig sigArgs
    
    let sigDecl = HE.val memSigName funcSig
        funcReg = TH.registerMethod tpName name
        funcFck = HE.val memFncName $ HE.Tuple [HE.VarE memSigName, HE.VarE memDefName]

    regDecl sigDecl

    case bodyBuilder of
        Nothing      -> return ()
        Just builder -> do
            sigHPats <- genFuncPats sigArgs mkVarNames
            funcDef <- HE.Function memDefName sigHPats <$> (HE.DoBlock <$> builder body output)
            regFunc funcDef

    regFunc funcFck
    regFunc funcReg

genFuncPats sigArgs mkVarNames = do
    let sigPats        = fmap (view Arg.pat) sigArgs
        sigPatsUntyped = fmap splitPatTypes sigPats

    if mkVarNames then mapM genPat sigPatsUntyped
                  else mapM genPatNoVarnames sigPatsUntyped
    
genFuncSig sigArgs = do
    let sigPats    = fmap (view Arg.pat) sigArgs
        sigVals    = fmap (view Arg.val) sigArgs
        sigNames   = fmap getSigName sigPats
    hdefs <- (mapM.mapM) genExpr sigVals
    return $ HE.rtuple $ fmap genSigArg (zip sigNames hdefs)

genSigArg (name, val) = (HE.MacroE ('_' : npfx : vpfx : "SigArg") nargs) where
    (vpfx, vargs) = case val of
        Nothing -> ('u', [])
        Just v  -> ('p', [v])
    (npfx, nargs) = case name of
        Nothing -> ('u', vargs)
        Just n  -> ('n', (HE.Lit $ HLit.String n):vargs)


getSigName (unwrap -> p) = case p of
    Pat.Var name -> Just (toText . unwrap $ name)
    _            -> Nothing

genForeign (Foreign target a) = case target of
    Foreign.CPP     -> Pass.fail "C++ foreign interface is not supported yet."
    Foreign.Haskell -> case a of
        Decl.FFunc funcDecl -> genFunc funcDecl (Just genFFuncBody) False

genFFuncBody txt _ = pure $ [HE.Native txt]

genFuncBody :: Ctx m a v => [LExpr a v] -> Maybe (LType a) -> PassResult m [HE]
genFuncBody exprs output = case exprs of
    []   -> pure []
    x:[] -> (:) <$> genExpr x
                <*> case unwrap x of
                      Expr.Assignment {} -> (:[]) <$> pure (mkVal $ HE.Tuple [])
                      _                  -> pure []
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output




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
    Pat.Tuple       items    -> (HE.ViewP $ (HE.VarE $ "extractTuple" <> (fromString $ show (length items)))) . HE.TupleP <$> mapM genRec items
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


ofType :: a -> a -> a
ofType = const 



genBody = \case
    [b] -> b
    b   -> HE.DoBlock b

--genExpr :: Ctx m a v => LExpr a v -> PassResult m HE
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

    Expr.Lambda inputs output body         -> do
                                              let args = fmap unwrap inputs
                                              pats   <- genFuncPats args True
                                              sig    <- genFuncSig args
                                              hbody  <- mapM genExpr body
                                              return $ HE.app (HE.VarE "mkLam") [sig, HE.Lambda pats $ genBody hbody]

    Expr.Grouped expr                      -> genExpr expr
    Expr.Assignment   dst src              -> HE.Arrow <$> genPat dst <*> genExpr src
    Expr.RecUpd  name fieldUpdts           -> HE.Arrow (HE.Var $ Naming.mkVar $ hash name) <$> case fieldUpdts of
        (fieldUpdt:[]) -> genField fieldUpdt
        _              -> Pass.fail "Multi fields updates are not supported yet"
        where src                  = Label 0 $ Expr.Var $ Expr.Variable name ()
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
    --Expr.App npat@(NamePat pfx base args)        -> (\s -> HE.MacroE "_call" [HE.Lit . HLit.Int . fromString $ show id, s]) <$> (foldl (flip (<*>)) (genExpr $ NamePat.segBase base) $ (fmap.fmap) (HE.AppE . (HE.AppE (HE.VarE "appNext"))) (fmap genArg $ NamePat.args npat))
    Expr.App npat@(NamePat pfx base args)        -> HE.AppE (HE.MacroE "_call" [HE.Lit . HLit.Int . fromString $ show id]) <$> (foldl (flip (<*>)) (genExpr $ NamePat.segBase base) $ (fmap.fmap) (HE.AppE . (HE.AppE (HE.VarE "appNext"))) (fmap genArg $ NamePat.args npat))
                                                    where genArg (Expr.AppArg mname expr) = (genExpr expr) -- nameMod mname <*> (genExpr expr)
                                                          nameMod mname = case mname of
                                                              Nothing -> return $ HE.AppE (HE.VarE "unnamed")
                                                              Just n  -> Pass.fail "No suppert for named args yet!" -- return $ HE.AppE (HE.VarE "named")
    Expr.Accessor     acc src             -> HE.AppE <$> (pure $ mkMemberGetter $ hash acc) <*> genExpr src --(get0 <$> genExpr src))
    Expr.List         lst                 -> case lst of
                                                 Expr.SeqList items -> mkVal . HE.ListE <$> mapM genExpr items
                                                 Expr.RangeList {}  -> Pass.fail "Range lists are not supported yet"
    Expr.Typed       cls expr             -> (\e t -> HE.MacroE "typed" [e,t]) <$> genExpr expr
                                                                                <*> genType cls
    Expr.Decl _                               -> Pass.fail "Nested declarations are not supported yet"
    Expr.Curry _                              -> Pass.fail "References are not supported yet"
    --p -> Pass.fail $ "Cannot construct: " <> show p

    where id = Enum.id lab


mkMemberGetter name = HE.MacroE "_member" [HE.Lit $ HLit.String name]




-- FIXME[wd]: ugly code
genLit :: Monad m => LLit a -> PassResult m HE
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


