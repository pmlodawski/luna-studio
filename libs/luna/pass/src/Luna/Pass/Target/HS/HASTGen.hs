---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Luna.Pass.Target.HS.HASTGen where

import           Data.Maybe (catMaybes, isJust)
import qualified Data.Set   as Set
import           Data.Text.Lazy                   (pack)
import           Flowbox.Prelude                   hiding (Traversal)
import           Luna.Pass                         (Pass (Pass), PassMonad)
import qualified Luna.Pass                         as Pass
import           Luna.Pass.Target.HS.HASTGen.State (addComment, addImport, genCallID, getCtx, regDecl, regFunc, regTHExpr, withCtx)
import qualified Luna.Pass.Target.HS.HASTGen.State as State
import           Luna.Syntax.Arg                   (Arg (Arg))
import qualified Luna.Syntax.Arg                   as Arg
import           Luna.Syntax.Decl                  (LDecl)
import qualified Luna.Syntax.Decl                  as Decl
import           Luna.Syntax.Enum                  (Enumerated)
import           Luna.Syntax.Expr                  (LExpr)
import qualified Luna.Syntax.Expr                  as Expr
import           Luna.Syntax.Foreign               (Foreign (Foreign))
import qualified Luna.Syntax.Foreign               as Foreign
import           Luna.Syntax.Label                 (Label (Label))
import qualified Luna.Syntax.Label                 as Label
import           Luna.Syntax.Lit                   (LLit)
import qualified Luna.Syntax.Lit                   as Lit
import qualified Luna.Syntax.Lit.Number            as Number
import           Luna.Syntax.Module                (LModule, Module (Module))
import qualified Luna.Syntax.Name                  as Name
import           Luna.Syntax.Name.Hash             (Hashable, hash)
import qualified Luna.Syntax.Name.Path             as Path
import           Luna.Syntax.Name.Pattern          (NamePat (NamePat), Segment (Segment))
import qualified Luna.Syntax.Name.Pattern          as NamePat
import           Luna.Syntax.Pat                   (LPat)
import qualified Luna.Syntax.Pat                   as Pat
import qualified Luna.Syntax.Traversals            as AST
import           Luna.Syntax.Type                  (LType)
import qualified Luna.Syntax.Type                  as Type
import           Luna.Syntax.Unit                  (Unit (Unit))
import qualified Luna.Target.HS.AST.Builder.TH     as TH
import           Luna.Target.HS.AST.Comment        (Comment (H1, H2, H3, H5))
import qualified Luna.Target.HS.AST.Deriving       as Deriving
import qualified Luna.Target.HS.AST.Expr           as HE
import qualified Luna.Target.HS.AST.Extension      as HExt
import qualified Luna.Target.HS.AST.Lit            as HLit
import qualified Luna.Target.HS.AST.Module         as HModule
import qualified Luna.Target.HS.Host.Naming2       as Naming
--import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
--import qualified Luna.Syntax.Enum             as Enum
--import qualified Luna.Syntax.Native           as Native
--import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
--import qualified Luna.Target.HS.AST.Comment                      as HComment
import           Luna.Target.HS.AST.Comment (Comment(H1, H2, H3, H4, H5))

import qualified Luna.Target.HS.AST.Deriving                     as Deriving
import           Luna.Target.HS.AST.Deriving                     (Deriving)

import           Luna.Pass                    (Pass(Pass), PassMonad)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)
import           Luna.Data.ImportInfo         (ImportInfo, Import(Import))
import qualified Luna.Data.ImportInfo         as II

import           Luna.Syntax.Arg              (Arg(Arg), LArg)
import qualified Luna.Syntax.Arg              as Arg

import qualified Luna.Parser.Parser           as Parser
import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import qualified Luna.Syntax.Name.Pattern     as NamePat

import qualified Luna.Pass.Target.HS.HASTGen.State as State
import           Luna.Pass.Target.HS.HASTGen.State (addComment, setModule, getModule, regFunc, regTHExpr, pushCtx, popCtx, getCtx, withCtx, regDecl, genCallID, addImport)
import           Luna.Syntax.Name.Hash              (Hashable, hash)
--import qualified Luna.Target.HS.Host.NamingOld                          as Naming

-- FIXME[wd]: remove (Show ...) after pass implementation

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data HASTGen = HASTGen

type PassResult           m   = PassMonad State.GenState m
type PassCtx          lab m a = (Enumerated lab, Traversal m a)
type Traversal            m a = (Pass.PassCtx m, AST.Traversal        HASTGen (PassResult m) a a)
type DefaultTraversal     m a = (Pass.PassCtx m, AST.DefaultTraversal HASTGen (PassResult m) a a)

type Ctx m a v = (Enumerated a, Monad m, Monoid v, Num a, v~(), Show a)

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

mkVal    = HE.AppE "val"


makeImportList :: ImportInfo -> [HE.Expr]
makeImportList info = map mkImp (info ^. II.imports )
    where mkImp = \(Import path _ _ _ rn) -> HE.Import (isJust rn) (II.qualPathToList path) (toText <$> rn) Nothing


pass :: Ctx m a v => Pass State.GenState (ImportInfo -> Unit (LModule a (LExpr a v)) -> PassResult m HE)
pass = Pass "HASTGen" "Haskell AST generator" def genUnit

passExpr :: Ctx m a v => Pass State.GenState (LExpr a v -> PassResult m HE)
passExpr = Pass "HASTGen" "Haskell AST generator" def genExpr

genUnit importInfo (Unit m) = genModule importInfo m




genNonEmptySec header lst f = unless (null lst) $ do
    State.addComment header
    f lst


genModule :: Ctx m a v => ImportInfo -> LModule a (LExpr a v) -> PassResult m HE
genModule importInfo (Label lab (Module path body)) = withCtx (fromText $ view Path.name path) $ do
    let mod     = HModule.addImport (HE.Import False ["Luna", "Target", "HS"] Nothing Nothing)
                $ foldl (.) id (map HModule.addImport (makeImportList importInfo))
                $ foldr HModule.addExt (HModule.mk modBaseName modPath)
                  [ HExt.DataKinds
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
        modBaseName      = view Path.name path
        modPath          = view Path.path path
        modConName       = Naming.mkModCons (hash modBaseName)
        modData          = Label (0::Int) (Decl.singleData $ fromText $ hash modBaseName)
        (fdatas, decls1) = extractDataDecls body
        (tAls , decls)   = extractTypeAls decls1
        datas            = fmap snd fdatas


    State.setModule mod

    State.regPragma (HE.Pragma $ HE.Include "pragmas.h")
    genDecl modData

    genNonEmptySec (H1 "Data headers")        fdatas $ mapM_ (uncurry genDataDeclHeaders)
    genNonEmptySec (H1 "Type Aliases")        tAls   $ mapM_ genDecl
    genNonEmptySec (H1 "Data declarations")   datas  $ mapM_ genDataDeclDefs
    genNonEmptySec (H1 "Module declarations") decls  $ mapM_ genDecl

    when (path == "Main") $ do
        State.addComment $ H1 "Main module wrappers"
        regFunc $ mainf modConName

    State.getModule

extractDataDecls = foldl go ([],[]) where
    go (datas, decls) ld@(Label lab decl) = case decl of
        Decl.Data d                             -> (datas ++ [(False,d)], decls)
        Decl.Foreign (Foreign _ (Decl.FData d)) -> (datas ++ [(True,d)], decls)
        _           -> (datas, ld:decls)

extractTypeAls = foldl go ([],[]) where
    go (a,as) ld@(Label lab decl) = case decl of
        Decl.TpAls {} -> (ld:a, as   )
        _             -> (a   , ld:as)

mainf modname = HE.val "main" $ HE.AppE "mainMaker" (HE.VarE modname)

genDataDeclHeaders :: (Monad m, Enumerated lab, Num lab, Show lab) => Bool -> Decl.DataDecl lab (LExpr lab ()) -> PassResult m ()
genDataDeclHeaders isNative (Decl.DataDecl (convVar -> name) params cons defs) = withCtx (fromText name) $ do
    addComment . H2 $ name <> " type"
    if not isNative then State.addDataType =<< consDecl
                    else State.addComment  $ H5 "datatype provided externally"
    regTHExpr $ TH.mkRegType name
    unless (null cons) $ regTHExpr $ TH.mkRegCons name conNames

    unless (null fieldNames) $ do
        State.addComment . H3 $ name <> " accessors"
        regTHExpr $ TH.mkFieldAccessors2 name conDescs
        regTHExpr $ TH.mkRegFieldAccessors name fieldNames
    mapM_ (genCon name paramsTxt derivings isNative) cons

    where derivings  = Deriving.stdDerivings
          paramsTxt  = fmap convVar params
          conDecls   = fmap (view Label.element) cons
          conDescs   = fmap getConDesc conDecls
          fieldNames = Set.toList . Set.fromList . catMaybes . concat $ fmap snd conDescs :: [Text]
          consE      = mapM genData conDecls
          consDecl   = HE.DataD name paramsTxt <$> consE <*> pure derivings
          getConDesc (Decl.Cons (convVar -> conName) fields) = (conName, fmap getLFieldName fields)
          genField   (Label _ (Decl.Field tp name val))      = genType tp
          genData    (Decl.Cons conName fields)              = HE.Con (hash conName) <$> mapM genField fields
          getConName con = hash $ Decl._consName $ unwrap con
          conNames   = fmap getConName cons

--data Cons  a e = Cons   { _consName :: CNameP  , _fields :: [LField a e]                   } deriving (Show, Generic, Eq, Read)

genDataDeclDefs :: (Monad m, Enumerated lab, Num lab, Show lab) => Decl.DataDecl lab (LExpr lab ()) -> PassResult m ()
genDataDeclDefs (Decl.DataDecl (convVar -> name) params cons defs) = withCtx (fromText name) $
    mapM_ genDecl defs

genDataDecl :: (Monad m, Enumerated lab, Num lab, Show lab) => Bool -> Decl.DataDecl lab (LExpr lab ()) -> PassResult m ()
genDataDecl isNative d = do
    genDataDeclHeaders isNative d
    genDataDeclDefs d

--genCons :: (Monad m, Applicative m, MonadState State.GenState m)
--        => Text -> [Text] -> [Decl.LCons a e] -> [Deriving] -> Bool -> m ()

getLFieldName (Label _ (Decl.Field _ mname _)) = fmap convVar mname

genCon name params derivings isNative cons = do
    let conDecls    = view Label.element cons
        clsConName  = Naming.mkCls name
        clsConName' = Naming.mkCls' name
        genConData (Decl.Cons (convVar -> conName) fields) = do
            let fieldNum   = length fields
                fieldNames = fmap getLFieldName fields
                conDefName = Naming.mkMemDef clsConName conName
                consDef    = HE.val conDefName
                           $ HE.AppE (HE.VarE $ liftCons fieldNum)
                                     (HE.VarE conName)
                args       = fmap mkArgFromName fieldNames
                -- FIXME[wd]: '@' is ugly hack for names double-hashing
                func       = Decl.FuncDecl [fromString . ("@" ++) . fromText $ clsConName']
                            (NamePat Nothing (Segment ("@" <> conName) (selfArg : args)) [])
                            Nothing []
                mkArgFromName name = case name of
                    Just n -> Arg (Label (0::Int) $ Pat.Var $ fromText n) Nothing
                    Nothing -> error "TODO!"

            addComment . H3 $ dotname [name, conName] <> " constructor"
            regFunc consDef
            genFuncNoBody func
            --regTHExpr $ TH.mkMethod clsConName conName

        getConDesc (Decl.Cons (convVar -> conName) fields) = (conName, fmap getLFieldName fields)
        conDesc    = getConDesc conDecls
        fieldNames = Set.toList . Set.fromList . catMaybes $ snd conDesc :: [Text]
        genField (Label _ (Decl.Field tp name val)) = genType tp

    genConData conDecls


--underscoreToHash = \case
--  [] -> []
--  (a:as) -> (a':underscoreToHash as)
--    where a' = if a == '_' then '#'
--                           else a


addClsDataType clsConName derivings = do
    let consClsHE  = HE.Con clsConName mempty
    State.addDataType $ HE.DataD clsConName mempty [consClsHE] derivings


liftCons num = "liftCons" <> fromString (show num)
mkArg    = "mkArg"
--paramSig = HE.TypedE mkArg (HE.VarT "Param")
paramSig    = HE.MacroE "param" []
nparamSig n = HE.AppT (HE.ConT "Named") (HE.Lit $ HLit.String n)
selfSig     = nparamSig "self"
selfArg     = Arg (Label 0 $ Pat.Var "self") Nothing

--data Cons  a e = Cons   { _consName :: CNameP   , _fields :: [LField a e]                  } deriving (Show, Eq, Generic, Read)

dotname names = mjoin "." names

convVar :: (Wrapper t, Hashable a Text) => t a -> Text
convVar = hash . unwrap

genDecl :: (Monad m, Enumerated lab, Num lab, Show lab) => LDecl lab (LExpr lab ()) -> PassResult m ()
genDecl ast@(Label lab decl) = case decl of
    Decl.Imp     imp       -> {- return () -} genQualifiedImp imp
    Decl.Func    funcDecl -> genStdFunc funcDecl
    Decl.Foreign fdecl    -> genForeign fdecl
    Decl.Data    ddecl    -> genDataDecl False ddecl
    Decl.Pragma  {}       -> return ()
    Decl.TpAls   dst src  -> State.regDecl =<< (HE.TypeD <$> genType dst <*> genType src)


genQualifiedImp :: (Monad m) => Decl.Imp -> PassResult m ()
genQualifiedImp (Decl.ModImp path (Just rename)) = State.regDecl =<< (return $ (HE.Assignment (HE.Var . pack $ "cons_" ++ (toString rename)) (HE.VarE . pack $ (toString rename) ++ ".cons_"  ++ (toString . last $ path))))
genQualifiedImp _ = return ()

genStdFunc :: (Monad m, Enumerated lab, Num lab, Show lab)
           => Decl.FuncDecl lab (LExpr lab ()) [LExpr lab ()] -> PassResult m ()
genStdFunc f = genFunc f (Just genFuncBody) True

genFuncNoBody :: (Monad m, Enumerated lab, Num lab, Show lab)
              => Decl.FuncDecl lab (LExpr lab ()) body -> PassResult m ()
genFuncNoBody f = genFunc f Nothing True


genFunc :: (Monad m, Enumerated lab, Num lab, Show lab)
        => Decl.FuncDecl lab (LExpr lab ()) body -> Maybe (body -> Decl.FuncOutput lab -> PassResult m [HE]) -> Bool -> PassResult m ()
genFunc (Decl.FuncDecl (fmap convVar -> path) sig output body) bodyBuilder mkVarNames = do
    when (length path > 1) $ Pass.fail "Complex method extension paths are not supported yet."

    ctx <- getCtx
    let tpName = if null path
        then case ctx of
            Nothing -> ""
            Just n  -> hash n
        else head path -- FIXME[wd]: needs name resolver
                       -- in case of defining extensionmethod inside of a class
                       -- maybe it should have limited scope to all classes inside that one?
        argNum     = length (NamePat.segments sig)
        name       = hash sig -- fromString $ ">>" ++ show sig ++ "<<"
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

    regFunc   funcFck
    regTHExpr funcReg

genFuncPats sigArgs mkVarNames = do
    let sigPats        = fmap (view Arg.pat) sigArgs
        sigPatsUntyped = fmap splitPatTypes sigPats

    if mkVarNames then mapM genPat sigPatsUntyped
                  else mapM genPatNoVarnames sigPatsUntyped

genFuncSig :: (Monad m, Enumerated lab, Num lab, Show lab) => [Arg a (LExpr lab ())] -> PassResult m HE
genFuncSig sigArgs = do
    let sigPats    = fmap (view Arg.pat) sigArgs
        sigVals    = fmap (view Arg.val) sigArgs
        sigNames   = fmap getSigName sigPats
    hdefs <- (mapM.mapM) genExpr sigVals
    return $ HE.rTuple $ fmap genSigArg (zip sigNames hdefs)

genSigArg (name, val) = HE.MacroE ('_' : npfx : vpfx : "SigArg") nargs where
    (vpfx, vargs) = case val of
        Nothing -> ('u', [])
        Just v  -> ('p', [v])
    (npfx, nargs) = case name of
        Nothing -> ('u', vargs)
        Just n  -> ('n', (HE.Lit $ HLit.String n):vargs)



getSigName (unwrap -> p) = case p of
    Pat.Var name -> Just (toText . unwrap $ name)
    _            -> Nothing

genForeign :: (Monad m, Enumerated a, Num a, Show a) => Foreign (Decl.ForeignDecl a (LExpr a ())) -> PassResult m ()
genForeign (Foreign target a) = case target of
    Foreign.CPP     -> Pass.fail "C++ foreign interface is not supported yet."
    Foreign.Haskell -> case a of
        Decl.FFunc decl -> genFunc decl (Just genFFuncBody) False
        Decl.FData decl -> genDataDecl True decl
        Decl.FImp  imp  -> addImport $ HE.Var imp
        where genImp = \case
                  -- genImp uses 'toText' in order not to mangle the names for foreigns
                  Decl.ModImp  path rename -> addImport $ HE.Import True  (fmap toText path) (fmap toText rename) Nothing
                  Decl.DeclImp path tgts   -> addImport $ HE.Import False (fmap toText path) Nothing htgts where
                      htgts = case sequence $ fmap genTgt tgts of
                          Right lst -> Just lst
                          Left  _   -> Nothing

                      genTgt = \case
                          Decl.ImpVar   name rename -> Right $ toText name
                          Decl.ImpType  name rename -> Right $ toText name
                          Decl.Wildcard hiding      -> Left "TODO"


genFFuncBody :: Monad m => Text -> a -> PassResult m [HE]
genFFuncBody txt _ = pure [HE.Native txt]

genFuncBody :: Ctx m a v => [LExpr a v] -> Maybe (LType a) -> PassResult m [HE]
--genFuncBody (transAssigExprs -> exprs) output = case exprs of
genFuncBody exprs output = case exprs of
    []   -> (:[]) <$> pure (mkVal $ HE.Tuple [])
    [x]  -> (:) <$> genExpr x
                <*> case unwrap x of
                      Expr.Assignment {} -> (:[]) <$> pure (mkVal $ HE.Tuple [])
                      Expr.RecUpd     {} -> (:[]) <$> pure (mkVal $ HE.Tuple [])
                      _                  -> pure []
    x:xs -> (:) <$> genExpr x <*> genFuncBody xs output




-- FIXME[wd]: powinno byc zaimplementowane przy pomocy jakis sprytnych traversali
splitPatTypes :: LPat a -> LPat a
splitPatTypes (Label lab pat) = case pat of
    Pat.Typed       pat cls  -> splitPatTypes pat
    Pat.App         src args -> Label lab $ Pat.App (splitPatTypes src) (fmap splitPatTypes args)
    Pat.Var         name     -> Label lab pat
    Pat.Tuple       items    -> Label lab $ Pat.Tuple (fmap splitPatTypes items)
    Pat.Lit         value    -> Label lab pat
    Pat.Wildcard             -> Label lab pat
    Pat.RecWildcard          -> Label lab pat
    Pat.Con         name     -> Label lab pat
    Pat.Grouped     p        -> Label lab $ Pat.Grouped $ splitPatTypes p


genPat = genPatGen genPat

genPatNoVarnames = genRec where
    genRec ast@(Label lab pat) = case pat of
        Pat.Var name -> pure $ HE.Var (convVar name)
        _            -> genPatGen genRec ast


genPatGen genRec (Label lab pat) = case pat of
    --Pat.App         src args -> HE.appP <$> genRec src <*> mapM genRec args
    Pat.App         src args -> case unwrap src of
                                    Pat.Con name -> HE.AppP (HE.ConP $ convVar name) . HE.ViewP "expandEl" . HE.rTupleX <$> mapM genRec args
    Pat.Var         name     -> pure $ HE.Var (Naming.mkVar $ convVar name)
    Pat.Typed       pat cls  -> HE.Typed <$> genRec pat <*> genType cls
    Pat.Tuple       items    -> HE.ViewP (HE.VarE "extractRTuple") . HE.rTupleX <$> mapM genRec items
    Pat.Lit         value    -> genLit value
    Pat.Wildcard             -> pure   HE.WildP
    Pat.RecWildcard          -> pure   HE.RecWildP
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



mkFlattenCtx = HE.AppE "polyJoin"
mkLiftf1     = HE.AppE "liftF1"


ofType :: a -> a -> a
ofType = const



genBody = \case
    [b] -> b
    b   -> HE.DoBlock b


--regExpr e = do
--    s <- get
--    put $ s ++ [e]

----genExpr2 [] = pure ()

--genExpr2s exprs = put (exprs, [])

--genStateExpr e = do
--    (exprs, done) <- get
--    case exprs of
--        (a:as) -> do put (es, done ++ [e])
--                     genExpr2 genStateExpr a
--        _      -> pure ()

--genExpr2 f (Label lab expr) = f $ case expr of
--    Expr.Curry e                             -> genExpr2 id e
--    Expr.Grouped e                           -> genExpr2 id e

transAssigExprs [] = []
transAssigExprs (Label lab e:es) = case e of
    Expr.Assignment dst src -> case unwrap dst of
        Pat.Var     {} -> continue
        Pat.Tuple   {} -> continue
        --Pat.Grouped a  -> transAssigExprs [a] ++ transAssigExprs es
        _          -> [Label lab $ Expr.Case src [Label 0 $ Expr.Match dst $ transAssigExprs es]]
    _ -> continue
    where continue = Label lab e : transAssigExprs es

gatherAppPat pat = go pat [] where
    go (Label lab p) apps = case p of
        Pat.Grouped a        -> go a apps
        Pat.App     base arg -> go base (apps ++ [arg])
        _                    -> (Label lab p, apps)

extractPatVars pat = go pat [] where
    go p lst = case unwrap p of
        Pat.Var     name  -> lst ++ [name]
        Pat.Tuple   items -> foldl (flip go) lst items
        Pat.Grouped gp    -> go gp lst



--genPatMatch = genRec where
--    genRec ast@(Label lab pat) = case pat of
--        Pat.Var name -> pure $ HE.Var (Naming.mkVar $ convVar name)
--        --Pat.Var name -> pure $ HE.ViewP "val" $ HE.Var (Naming.mkVar $ convVar name)
--        _            -> genPatGen genRec ast

genPatMatch patBase (Label lab pat) expr = case pat of
    Pat.Grouped     p        -> genPatMatch patBase p expr
    Pat.App         src args -> case unwrap src of
                                    Pat.Con name -> return $ HE.Match hpat $ HE.LetBlock [recExp] expr where
                                        recTup = HE.rTupleX $ fmap genVars args
                                        recLayout = HE.AppE "expandEl" (HE.AppE (HE.VarE $ "layout_" <> conName) $ HE.VarE patBase)
                                        recExp = HE.Assignment recTup recLayout
                                        hpat = HE.AppP (HE.ConP conName) HE.RecWildP
                                        conName = convVar name
                                    -- . HE.ViewP "expandEl" . HE.rTupleX $ fmap genVars args
                                    _ -> error "Non constructor based pattern application is not supported yet."
    --Pat.Tuple       args    -> return $ HE.rTupleX $ fmap genVars args
    --Pat.Wildcard            -> return $ HE.WildP
    --Pat.Lit         lit     -> genLit lit
    --Pat.Var         name    -> pure $ HE.ViewP "val" $ HE.Var (Naming.mkVar $ convVar name)
    Pat.Con name            -> return $ HE.Match (HE.ConP $ convVar name) expr
    a                       -> error $ "Pattern match not supported: " ++ show a

    where genVars (Label lab' pat') = case pat' of
              Pat.Var name -> HE.Var . Naming.mkVar $ convVar name
              _            -> error "Complex pattern matches are not supported yet."


--collectPatVars pats = go pats [] where
--    go [] names     = names
--    go (a:as) names = case unwrap a of
--        Pat.Var name      -> go as (name:names)
--        Pat.App base args -> go [base] names


collectPatVars a = case unwrap a of
    Pat.Var     name      -> [name]
    Pat.App     base args -> collectPatVars base ++ concat (fmap collectPatVars args)
    Pat.Grouped g         -> collectPatVars g
    Pat.Tuple   items     -> concat $ fmap collectPatVars items
    _                     -> []

genExpr :: (Monad m, Enumerated a, Num a, Show a) => LExpr a () -> PassResult m HE
genExpr (Label lab expr) = case expr of
    Expr.Curry e                             -> genExpr e
    Expr.Grouped expr                        -> genExpr expr
    Expr.Assignment dst src                  ->  go dst where
                                                    go p = case unwrap p of
                                                        Pat.Var     {} -> simple
                                                        Pat.Tuple   {} -> simple
                                                        Pat.Grouped g  -> go g
                                                        _              -> complex
                                                    simple  = HE.Arrow <$> genPat dst <*> genExpr src
                                                    complex =   HE.Arrow (HE.ViewP "extractRTuple" $ HE.rTupleX $ fmap (fromText . Naming.mkVar . hash) varNames)
                                                            <$> (genExpr $ Label 0 $ Expr.Case src [Label 0 $ Expr.Match dst [Label 0 expr]])
                                                            where varNames = collectPatVars dst
                                                                  expr     = Expr.Tuple $ fmap (Label 0 . flip Expr.var ()) varNames
    Expr.Lit          value                  -> mkVal <$> genLit value
    Expr.Tuple        items                  -> mkVal . HE.rTupleX <$> mapM genExpr items
    Expr.Var (Expr.Variable name _)          -> pure . HE.Var $ Naming.mkVar $ hash name
    Expr.Cons  name                          -> pure $ HE.Var $ Naming.mkCons $ hash name
    Expr.Accessor     acc src                -> HE.AppE <$> (pure $ mkMemberGetter $ hash acc) <*> genExpr src --(get0 <$> genExpr src))
    Expr.Typed       cls expr                -> (\e t -> HE.MacroE "_typed" [e,t]) <$> genExpr expr <*> genType cls
    Expr.Case  expr match -> mkFlattenCtx <$> (HE.AppE <$> lamFunc <*> genExpr expr)
        where passVar = "pat_base"
              caseE   = HE.CaseE passVar <$> body'
              body    = mapM genMatch match
              lam     = HE.Lambda [passVar] <$> caseE
              lamFunc = mkLiftf1 <$> lam
              body'   = (\a -> a ++ [HE.Match HE.WildP (HE.AppE "error" (HE.Lit $ HLit.String "Non-exhaustive patterns in case"))]) <$> body
              genMatch (unwrap -> Expr.Match pat body) = genPatMatch passVar pat =<< (HE.DoBlock <$> mapM genExpr body)
    Expr.Lambda inputs output body -> do
        lid <- genCallID
        ctx <- getCtx
        tpName <- case ctx of
            Nothing -> Pass.fail "Running lambda generation without context!"
            Just n  -> return $ hash n

        let inputs' = fmap unwrap inputs
            fname   = "lambda#" <> fromString (show lid)
            func    = Decl.Func $ Decl.FuncDecl [] (NamePat.single fname (selfArg : inputs')) Nothing body
            lamRef  = Expr.Accessor (Name.TypeName $ fromText fname) $ Label 0 $ Expr.app (Label 0 $ Expr.Cons $ fromText tpName) []
        genDecl (Label lab func)
        genExpr (Label (0::Int) lamRef)
      where args = fmap unwrap inputs

    Expr.RecUpd  name fieldUpdts -> HE.Arrow (HE.Var $ Naming.mkVar $ hash name) <$> case fieldUpdts of
        [fieldUpdt] -> genField fieldUpdt
        _           -> Pass.fail "Multi fields updates are not supported yet"
        where src                  = Label 0 $ Expr.Var $ Expr.Variable name ()
              setter exp field val = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName $ fromText . ("set#"<>) $ hash field) exp) [Expr.AppArg Nothing val]
              getter exp field     = Label 0 $ Expr.app (Label 0 $ Expr.Accessor (Name.VarName field) exp) []
              getSel sel           = foldl (flip($)) src (fmap (flip getter) (reverse sel))
              setStep       (x:xs) = setter (getSel xs) x
              setSteps args@[_]    = setStep args
              setSteps args@(_:xs) = setSteps xs . setStep args
              setSteps          [] = undefined
              genField (Expr.FieldUpd sels expr) = genExpr $ setSteps (reverse sels) expr

    Expr.App npat@(NamePat pfx base args) -> mod <*> foldl (flip (<*>)) (genExpr segBase) ((fmap.fmap) HE.AppE argGens)
      where argGens = genArg <$> NamePat.args npat
            genArg (Expr.AppArg mname expr) = nameMod mname <$> genExpr expr
            nameMod mname = case mname of
                Nothing -> HE.AppE "appNext"
                Just n  -> HE.AppE $ HE.AppE "appByName" (HE.MacroE "_name" [HE.Lit $ HLit.String n])
            segBase = NamePat.segBase base
            mod = case unwrap segBase of
                Expr.Curry {} -> pure id
                _             -> (\cid -> HE.AppE (HE.MacroE "_call" [HE.Lit . HLit.Int . fromString $ show cid])) <$> genCallID

    Expr.List lst -> case lst of
        Expr.SeqList items -> foldr (\a b -> HE.app "lstCons" [a,b]) (mkVal $ HE.ListE []) <$> mapM genExpr items
        --Expr.SeqList items -> mkVal . HE.ListE <$> mapM genExpr items
        Expr.RangeList {}  -> Pass.fail "Range lists are not supported yet"

    Expr.Meta meta -> case unwrap meta of
        Type.MetaCons n -> return HE.NOP
        _               -> Pass.fail "Only meta-constructors are supported now"

    Expr.Decl _ -> Pass.fail "Nested declarations are not supported yet"
    Expr.Wildcard -> return "_"


mkMemberGetter name = HE.MacroE "_member" [HE.Lit $ HLit.String name]




-- FIXME[wd]: ugly code
genLit :: Monad m => LLit a -> PassResult m HE
genLit (Label lab lit) = case lit of
    -- FIXME[wd]: fix the number handling.
    Lit.Number (Number.Number base repr exp sign) -> do
        when (base /= 10) $ Pass.fail "number base different than 10 are not yet supported"
        when (isJust exp) $ Pass.fail "number exponents are not yet supported"
        let sign' = case sign of
                        Number.Positive -> ""
                        Number.Negative -> "-"
        case repr of
            Number.Float   int frac -> mkLit "Float" (HLit.Float   $ sign' <> fromString int <> "." <> fromString frac)
            Number.Decimal int      -> mkLit "Int"   (HLit.Integer $ sign' <> fromString int)

    --Lit.Integer _ str      -> mkLit "Int"    (HLit.Integer str)
    --Lit.Float   _ str      -> mkLit "Float" (HLit.Float   str)
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


