---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.DEP.AST.ASTConvertion where

import           Control.Monad.Trans.Either
import qualified Data.Maybe                 as Maybe
import qualified Data.Text.Lazy             as Text

import           Flowbox.Prelude
import           Luna.DEP.AST.Arg           as DArg
import qualified Luna.DEP.AST.Expr          as DExpr
import qualified Luna.DEP.AST.Lit           as DLit
import qualified Luna.DEP.AST.Module        as DModule
import qualified Luna.DEP.AST.Name          as DName
import qualified Luna.DEP.AST.Pat           as DPat
import qualified Luna.DEP.AST.Type          as DType
import           Luna.Syntax.Arg            (Arg (Arg))
import qualified Luna.Syntax.Arg            as Arg
import           Luna.Syntax.Decl           (LDecl)
import qualified Luna.Syntax.Decl           as Decl
import           Luna.Syntax.Enum           (IDTag (IDTag))
import           Luna.Syntax.Expr           (LExpr)
import qualified Luna.Syntax.Expr           as Expr
import           Luna.Syntax.Foreign        (Foreign (Foreign))
import qualified Luna.Syntax.Foreign        as Foreign
import           Luna.Syntax.Label          (Label (Label))
import           Luna.Syntax.Lit            (LLit)
import qualified Luna.Syntax.Lit            as Lit
import           Luna.Syntax.Module         (LModule)
import qualified Luna.Syntax.Module         as Module
import           Luna.Syntax.Name.Path      (QualPath (QualPath))
import           Luna.Syntax.Name.Pattern   (NamePat (NamePat), Segment (Segment))
import           Luna.Syntax.Pat            (LPat)
import qualified Luna.Syntax.Pat            as Pat
import           Luna.Syntax.Type           (LType)
import qualified Luna.Syntax.Type           as Type



class ASTConvertion a m b where
    convertAST :: a -> EitherT Error m b

data Error = ConvertionError String
           | IllegalConversion String
           | UnknownConversion String
           | UnsupportedConversion String
           | ASTError String
           deriving (Show)

instance (Monad m, Applicative m) => ASTConvertion DExpr.Expr m (LExpr IDTag ()) where
    convertAST = \case
        DExpr.NOP         id           -> left $ IllegalConversion "NOP"
        DExpr.Ref         {}           -> left $ UnknownConversion "Expr.Ref"
        DExpr.RefType     {}           -> left $ UnknownConversion "Expr.RefType"
        DExpr.Native      {}           -> left $ UnknownConversion "Expr.Native"
        DExpr.NativeCode  {}           -> left $ UnknownConversion "Expr.NativeCode"
        DExpr.NativeVar   {}           -> left $ UnknownConversion "Expr.NativeVar"
        DExpr.RangeFromTo {}           -> left $ IllegalConversion "Expr.RangeFromTo"
        DExpr.RangeFrom   {}           -> left $ IllegalConversion "Expr.RangeFrom"
        DExpr.Wildcard    id           -> right $ l id Expr.Wildcard
        DExpr.Var         id n         -> right . l id $ Expr.Var (Expr.Variable (fromString n) ())
        DExpr.Con         id n         -> right . l id $ Expr.Cons (fromString n)
        DExpr.Typed       id t e       -> l id <$> (Expr.Typed <$> convertAST t <*> convertAST e)
        DExpr.Assignment  id p e       -> l id <$> (Expr.Assignment <$> convertAST p <*> convertAST e)
        DExpr.Grouped     id e         -> l id . Expr.Grouped <$> convertAST e
        DExpr.Tuple       id es        -> l id . Expr.Tuple   <$> mapM convertAST es
        DExpr.Lit         id lit       -> l id . Expr.Lit     <$> convertAST lit
        DExpr.Accessor    id acc dst   -> l id . Expr.Accessor (fromString accName) <$> convertAST dst
                                       where accName = DExpr._accName acc
        DExpr.List        id items     -> l id . Expr.List <$> lst where
            lst = case items of
                [DExpr.RangeFromTo id s e] -> Expr.RangeList <$> (Expr.Linear <$> convertAST s <*> (Just <$> convertAST e))
                [DExpr.RangeFrom   id s  ] -> Expr.RangeList <$> (Expr.Linear <$> convertAST s <*> pure Nothing)
                xs                         -> Expr.SeqList <$> mapM convertAST xs
        DExpr.RecordUpdate id src sel expr -> case src of
            DExpr.Var _ n ->  l id . Expr.RecUpd (fromString n) . pure
                          <$> (Expr.FieldUpd (fmap fromString sel) <$> convertAST expr)
            _             -> left $ IllegalConversion "Record update with non-variable base"
        DExpr.Case id expr match -> l id <$> (Expr.Case <$> convertAST expr <*> mapM convertMatch match) where
            convertMatch = \case
                DExpr.Match mid p body -> l mid <$> (Expr.Match <$> convertAST p <*> mapM convertAST body)
                _                      -> left $ ASTError "Non-match pattern found in case expression"
        DExpr.App id src args -> l id . Expr.App <$> (NamePat Nothing <$> (Segment <$> convertAST src <*> mapM convertArg args) <*> pure []) where
            convertArg = \case
                DArg.Named _ n a -> Expr.AppArg (Just $ fromString n) <$> convertAST a
                DArg.Unnamed _ a -> Expr.AppArg Nothing               <$> convertAST a
        where l id = Label (IDTag id)


instance (Monad m, Applicative m) => ASTConvertion DLit.Lit m (LLit IDTag) where
    convertAST = \case
        DLit.Char   id c -> right . l id $ Lit.Char c
        DLit.String id s -> right . l id $ Lit.String s
        DLit.Number id n -> right . l id $ Lit.Number (read . show $ n)
        where l id = Label (IDTag id)


instance (Monad m, Applicative m) => ASTConvertion DType.Type m (LType IDTag) where
    convertAST = \case
        DType.Unknown  id     -> right . l id $ Type.Wildcard
        DType.Var      id n   -> right . l id $ Type.Var (fromString n)
        DType.Con      id sgs -> right . l id $ Type.Con (fmap fromString sgs)
        DType.Tuple    id ts  -> l id . Type.Tuple <$> mapM convertAST ts
        DType.List     id t   -> l id . Type.List  <$> convertAST t
        DType.Function id i o -> l id <$> (Type.Function <$> mapM convertAST i <*> convertAST o)
        DType.App      id s a -> l id <$> (Type.App <$> convertAST s <*> mapM convertAST a)
        DType.Data     {}     -> left $ IllegalConversion "Type.Data"
        DType.Module   {}     -> left $ IllegalConversion "Type.Module"
        where l id = Label (IDTag id)


instance (Monad m, Applicative m) => ASTConvertion DPat.Pat m (LPat IDTag) where
    convertAST = \case
        DPat.Var         id n   -> right . l id $ Pat.Var (fromString n)
        DPat.Wildcard    id     -> right . l id $ Pat.Wildcard
        DPat.RecWildcard id     -> right . l id $ Pat.RecWildcard
        DPat.Con         id n   -> right . l id $ Pat.Con (fromString n)
        DPat.Lit         id lit -> l id . Pat.Lit      <$> convertAST lit
        DPat.Tuple       id ps  -> l id . Pat.Tuple    <$> mapM convertAST ps
        DPat.Grouped     id p   -> l id . Pat.Grouped  <$> convertAST p
        DPat.App         id s a -> l id <$> (Pat.App   <$> convertAST s <*> mapM convertAST a)
        DPat.Typed       id p c -> l id <$> (Pat.Typed <$> convertAST p <*> convertAST c)
        where l id = Label (IDTag id)


instance (Monad m, Applicative m) => ASTConvertion DExpr.Expr m (LDecl IDTag (LExpr IDTag ())) where
     convertAST = convertExpr2Decl

convertExpr2Decl :: (Monad m, Applicative m) => DExpr.Expr -> EitherT Error m (LDecl IDTag (LExpr IDTag ()))
convertExpr2Decl = \case
        DExpr.Import id path tgt rename -> l id . Decl.Imp <$> convertImp where
            convertImp = if Maybe.isJust rename
                then right $ Decl.ModImp modPath (fmap fromString rename)
                else Decl.DeclImp modPath <$> case tgt of
                    DExpr.Var _ n -> right [Decl.ImpVar  (fromString n) Nothing]
                    DExpr.Con _ n -> right [Decl.ImpType (fromString n) Nothing]
            modPath = fmap fromString path
        --DExpr.Import id path tgt rename -> l id . Decl.Imp (fmap fromString path) Nothing . pure <$> convertTgt tgt rename where
        --    convertTgt tgt rename = case tgt of
        --        DExpr.Var _ n -> right $ Decl.ImpVar (fromString n) (fmap fromString rename)
        --        DExpr.Con _ n -> right $ Decl.ImpVar (fromString n) (fmap fromString rename)
        --        _             -> left $ IllegalConversion "Import with non-variable base"
        DExpr.ImportNative {} -> left $ UnsupportedConversion "Expr.ImportNative"
        DExpr.TypeAlias id src dst -> l id <$> (Decl.TpAls <$> convertAST src <*> convertAST dst)
        DExpr.TypeDef   id src dst -> l id <$> (Decl.TpWrp <$> convertAST src <*> convertAST dst)
        DExpr.Data      id t cons cls methods -> do
            ddecl <- case t of
                DType.Data _ n params ->  Decl.DataDecl (fromString n) (fmap fromString params)
                                      <$> mapM convertCons cons
                                      <*> mapM convertAST methods
                _                     -> left $ IllegalConversion "Data declaration with unsupported type"

            return . l id . Decl.Data $ ddecl
            where convertCons (DExpr.ConD cid n fields) = l cid . Decl.Cons (fromString n) <$> mapM convertField fields
                  convertField (DExpr.Field fid n t mv) = l fid <$> (Decl.Field <$> convertAST t <*> pure (Just $ fromString n) <*> mapM convertAST mv)
        DExpr.DataNative id t cons cls methods -> do
            (Label lab (Decl.Data decl)) <- convertExpr2Decl $ DExpr.Data id t cons cls methods
            return $ l id $ Decl.Foreign $ Foreign Foreign.Haskell $ Decl.FData decl
        DExpr.Function id path n ins ot [DExpr.Native _ segments] -> l id . Decl.Foreign . Foreign Foreign.Haskell . Decl.FFunc <$> decl where
            decl = Decl.FuncDecl (fmap fromString path)
                                 <$> (NamePat Nothing <$> (Segment (fromString $ DName._base n) <$> mapM convertArg ins) <*> pure [])
                                 <*> pure Nothing
                                 <*> convertSegments segments
            convertSegments seg = Text.intercalate " " <$> mapM convertSegment seg
            convertSegment seq = case seq of
                DExpr.NativeCode _ code -> return $ Text.pack code
                DExpr.NativeVar  _ var  -> return $ Text.pack var
        DExpr.Function id path n ins ot body -> l id . Decl.Func <$> decl where
            decl = Decl.FuncDecl (fmap fromString path)
                                 <$> (NamePat Nothing <$> (Segment (fromString $ DName._base n) <$> mapM convertArg ins) <*> pure [])
                                 <*> pure Nothing
                                 <*> mapM convertAST body
        where
            l id = Label (IDTag id)
            convertArg (DExpr.Arg _ p mv) = Arg <$> convertAST p <*> mapM convertAST mv
            convertArg (DExpr.Arg _ p mv) = Arg <$> convertAST p <*> mapM convertAST mv


instance (Monad m, Applicative m) => ASTConvertion DModule.Module m (LModule IDTag (LExpr IDTag ())) where
    convertAST (DModule.Module id (DType.Module tid n path) imps cls tals tdefs fields methods mods) =
        l id . Module.Module (QualPath (fmap fromString path) (fromString n))
                      <$> mapM convertAST (imps ++ cls ++ tals ++ tdefs ++ fields ++ methods)
        where l id = Label (IDTag id)

