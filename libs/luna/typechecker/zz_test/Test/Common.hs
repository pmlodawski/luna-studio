{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Common where

--import Luna.Typechecker.AST
--import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type
--import Luna.Typechecker.TypeEnv
--import Luna.Typechecker.TypecheckClass

-- luna-core
import Luna.ASTNew.Expr       as NExpr
import Luna.ASTNew.Label      as Label
import Luna.ASTNew.Lit        as Lit
import Luna.ASTNew.Pat        as NPat
import Luna.ASTNew.Lit.Number as Num
import Luna.ASTNew.Name       as NName
import Luna.ASTNew.Type       as NType
import Luna.ASTNew.Arg        as NArg
import Luna.ASTNew.Decl       as NDecl
import Luna.ASTNew.Native     as NNative

-- luna-logger
import Logger

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Identity

import Data.Traversable (traverse)


-- ------------------------------------------------------------
-- ██╗██████╗     ███╗   ███╗ █████╗ ██╗  ██╗███████╗██████╗
-- ██║██╔══██╗    ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝██╔══██╗
-- ██║██║  ██║    ██╔████╔██║███████║█████╔╝ █████╗  ██████╔╝
-- ██║██║  ██║    ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝  ██╔══██╗
-- ██║██████╔╝    ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗██║  ██║
-- ╚═╝╚═════╝     ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
-- ------------------------------------------------------------

newtype VarIDMakerT m a = VarIDMakerT { runVarIDMakerT :: Int -> m (Int, a) }

instance (Monad m) => Monad (VarIDMakerT m) where
  return x = VarIDMakerT (\i -> return (i, x))
  m >>= gn = VarIDMakerT aux
    where aux i = do (j, x) <- runVarIDMakerT m i
                     runVarIDMakerT (gn x) j

instance MonadTrans VarIDMakerT where
  lift ma = VarIDMakerT aux
    where aux i = do res <- ma
                     return (i, res)

instance (Monad m) => Functor (VarIDMakerT m) where
  fmap = liftM

instance (Monad m) => Applicative (VarIDMakerT m) where
  pure = return
  (<*>) = ap


nextVarID :: (Monad m) => VarIDMakerT m Int
nextVarID = VarIDMakerT aux
  where aux i = return (i+1, i)

mkLabel :: (Monad m) => a -> VarIDMakerT m (F a)
mkLabel v = do i <- nextVarID
               return (Identity v)



type F = Identity




-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ███████╗██╗  ██╗██████╗ ██████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ██╔════╝╚██╗██╔╝██╔══██╗██╔══██╗
-- ██╔████╔██║███████║█████╔╝ █████╗      █████╗   ╚███╔╝ ██████╔╝██████╔╝
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██╔══╝   ██╔██╗ ██╔═══╝ ██╔══██╗
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ███████╗██╔╝ ██╗██║     ██║  ██║
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝
-- ------------------------------------------------------------------------------------------------------------------------

mkELambda :: (Monad m) => [NArg.Arg F (Expr F a)] -> NType.Type F -> [Expr F a] -> VarIDMakerT m (Expr F a)
mkELambda args outp body = liftM3 NExpr.Lambda (mapM mkLabel args) (mkLabel outp) (mapM mkLabel body)

mkERecUpt :: (Monad m) => Expr F a -> [String] -> Expr F a -> VarIDMakerT m (Expr F a)
mkERecUpt src select expr = liftM3 NExpr.RecUpdt (mkLabel src) (return $ map NName.VName select) (mkLabel expr)

mkEApp :: (Monad m) => NExpr.Expr F a -> [(Maybe String, NExpr.Expr F a)] -> VarIDMakerT m (NExpr.Expr F a)
mkEApp src args = liftM2 NExpr.App (mkLabel src) (mapM aux args >>= mkLabel . NExpr.Seq)
  where aux (Just s,  e) = do le <- mkLabel e
                              return (NExpr.Named le (NName.VName s))
        aux (Nothing, e) = do le <- mkLabel e
                              return (NExpr.Unnamed le)

mkECase :: (Monad m) => NExpr.Expr F a -> [NExpr.Match F a] -> VarIDMakerT m (NExpr.Expr F a)
mkECase ex ms = liftM2 NExpr.Case (mkLabel ex) (mapM mkLabel ms)

mkETyped :: (Monad m) => NType.Type F -> NExpr.Expr F a -> VarIDMakerT m (NExpr.Expr F a)
mkETyped ty ex = liftM2 NExpr.Typed (mkLabel ty) (mkLabel ex)

-- (~=) :: (Monad m) => Pat F -> Expr F a -> VarIDMakerT m (Expr F a)
-- dst ~= src = liftM2 NExpr.Assignment (mkLabel dst) (mkLabel src)

mkEAssignment :: (Monad m) => Pat F -> Expr F a -> VarIDMakerT m (NExpr.Expr F a)
mkEAssignment dst src = liftM2 NExpr.Assignment (mkLabel dst) (mkLabel src)

mkEAccessor :: (Monad m) => NName.Name -> Expr F a -> VarIDMakerT m (NExpr.Expr F a)
mkEAccessor name expr = liftM (NExpr.Accessor name) (mkLabel expr)

mkERef :: (Monad m) => Expr F a -> VarIDMakerT m (NExpr.Expr F a)
mkERef ex = liftM NExpr.Ref (mkLabel ex)

mkEList :: (Monad m) => [Expr F a] -> VarIDMakerT m (NExpr.Expr F a)
mkEList es = liftM NExpr.List (mapM mkLabel es)

mkETuple :: (Monad m) => [Expr F a] -> VarIDMakerT m (NExpr.Expr F a)
mkETuple es = liftM NExpr.Tuple (mapM mkLabel es)

mkEGrouped :: (Monad m) => Expr F a -> VarIDMakerT m (NExpr.Expr F a)
mkEGrouped e = liftM NExpr.Grouped (mkLabel e)

mkECons :: (Monad m) => String -> VarIDMakerT m (NExpr.Expr F a)
mkECons n = return $ NExpr.Cons (NName.CName n)

mkEDecl :: (Monad m) => NDecl.Decl F (NExpr.Expr F a) -> VarIDMakerT m (NExpr.Expr F a)
mkEDecl sd = do ssd <- foo sd
                lsd <- mkLabel ssd
                return (NExpr.Decl lsd)
  where 
        foo :: (Monad m) => Decl F (Expr F a) -> VarIDMakerT m (Decl F (F (Expr F a)))
        foo (NDecl.Data        tname   params   cons    defs )         = do lcons <- mapM bar cons
                                                                            return (NDecl.Data        tname   params   lcons   _     ) -- [RDecl F (F (Expr F a1))]
        bar :: (Monad m) => F (Cons F e) -> VarIDMakerT m (F (Cons F (F e)))
        bar (Identity x) = do xx <- baz x
                              return (Identity xx)
        baz :: (Monad m) => Cons F e -> VarIDMakerT m (Cons F (F e))
        baz (NDecl.Cons cn fs) = do ffs <- mapM quux fs
                                    return (NDecl.Cons cn ffs)
                                    --_ -- VarIDMakerT m1 (Cons F (F e))
        quux :: (Monad m) => F (Field F e) -> VarIDMakerT m (F (Field F (F e)))
        quux (Identity (NDecl.Field ft fn Nothing)) = return (Identity (NDecl.Field ft fn Nothing))
        quux (Identity (NDecl.Field ft fn (Just fv))) = do ffv <- mkLabel fv
                                                           return (Identity (NDecl.Field ft fn (Just ffv)))
                               --fx <- _ 
                               -- x :: Field F e
        --quux (NDecl.Field ft fn fv) = do ffv <- _
        --                                 return (NDecl.Field ft fn ffv)
              -- fs :: (Field F e) -> F (Field F (F e))
        --foo (NDecl.Function    path    fname    inputs  output  body ) =          NDecl.Function    path    fname    inputs  output  body 
        --foo (NDecl.Import      modPath targets )                       =          NDecl.Import      modPath targets 
        --foo (NDecl.TypeAlias   dstType srcType )                       =          NDecl.TypeAlias   dstType srcType 
        --foo (NDecl.TypeWrapper dstType srcType )                       =          NDecl.TypeWrapper dstType srcType 
        --foo (NDecl.Native      native)                                 =          NDecl.Native      native
  --where aux :: (Monad m) => NDecl.Decl F (NExpr.Expr F a) -> VarIDMakerT m (F (Decl F (F (Expr F a))))
        --aux d = do fd <- foo d
        --           mkLabel fd
        ---- foo r@(NDecl.Data     { NDecl._cons   = cons,   NDecl._defs = defs }) = return 
        ---- foo r@(NDecl.Function { NDecl._inputs = inputs, NDecl._body = body }) = undefined
        ---- foo r@(NDecl.Native   { NDecl._native = native                     }) = undefined
        ---- foo r@(NDecl.Import mp trgts)                                         = return (NDecl.Import mp trgts)
        ---- foo r@(NDecl.TypeAlias rt s)                                          = return (NDecl.TypeAlias rt s)
        ---- foo r@(NDecl.TypeWrapper dst src)                                     = return (NDecl.TypeWrapper dst src)
        --bar :: F (Cons F (Expr F a)) -> VarIDMakerT m1 (RCons F (F (Expr F a)))
        --bar 


mkELit :: (Monad m) => Lit.Lit -> VarIDMakerT m (NExpr.Expr F a)
mkELit l = liftM NExpr.Lit (mkLabel l)

mkENative :: (Monad m) => NNative.Native (NExpr.Expr F a) -> VarIDMakerT m (NExpr.Expr F a)
mkENative n = liftM NExpr.Native (aux n)
  where aux (NNative.Code s) = return (NNative.Code s)
        aux (NNative.AST  e) = liftM NNative.AST (mkLabel e)

mkEVar :: (Monad m) => a -> VarIDMakerT m (NExpr.Expr F a)
mkEVar a = return (NExpr.Var a)

mkEWildcard :: (Monad m) => VarIDMakerT m (NExpr.Expr F a)
mkEWildcard = return NExpr.Wildcard

-- ------------------------------------------------------------------------------------------------------------------------

mkEMatch :: (Monad m) => NPat.Pat F -> [NExpr.Expr F a] -> VarIDMakerT m (Match F a)
mkEMatch pat bodies = liftM2 NExpr.Match (mkLabel pat) (mapM mkLabel bodies)

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ██████╗  █████╗ ████████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ██╔══██╗██╔══██╗╚══██╔══╝
-- ██╔████╔██║███████║█████╔╝ █████╗      ██████╔╝███████║   ██║
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██╔═══╝ ██╔══██║   ██║
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ██║     ██║  ██║   ██║
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚═╝     ╚═╝  ╚═╝   ╚═╝
-- ------------------------------------------------------------------------------------------------------------------------

mkPTupleM :: (Monad m) => [NPat.Pat F] -> VarIDMakerT m (NPat.Pat F)
mkPTupleM pats = liftM NPat.Tuple (mapM mkLabel pats)

mkPLitM :: (Monad m) => Lit.Lit -> VarIDMakerT m (NPat.Pat F)
mkPLitM l = liftM NPat.Lit (mkLabel l)

mkPGroupedM :: (Monad m) => NPat.Pat F -> VarIDMakerT m (NPat.Pat F)
mkPGroupedM pat = liftM NPat.Grouped (mkLabel pat)

-- mkPTypedM :: (Monad m) => NPat.Pat F -> NType.Type F -> VarIDMakerT m (NPat.Pat F)
-- mkPTypedM pat ty = liftM2 NPat.Grouped (mkLabel pat) (mkLabel ty)

mkPAppM :: (Monad m) => NPat.Pat F -> [NPat.Pat F] -> VarIDMakerT m (NPat.Pat F)
mkPAppM pat pats = liftM2 NPat.App (mkLabel pat) (mapM mkLabel pats)

mkPCon :: String -> NPat.Pat f
mkPCon x = NPat.Con { NPat._cname = NName.CName x }

mkPVar :: String -> NPat.Pat f
mkPVar x = NPat.Var { NPat._vname = NName.VName x }

mkPWild :: NPat.Pat f
mkPWild = NPat.Wildcard

mkPRecWild :: NPat.Pat f
mkPRecWild = NPat.RecWildcard

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ██╗     ██╗████████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ██║     ██║╚══██╔══╝
-- ██╔████╔██║███████║█████╔╝ █████╗      ██║     ██║   ██║
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██║     ██║   ██║
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ███████╗██║   ██║
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚══════╝╚═╝   ╚═╝
-- ------------------------------------------------------------------------------------------------------------------------

class    LunaLit a      where mkLit :: a -> Lit.Lit
instance LunaLit Char   where mkLit   = Lit.Char
instance LunaLit String where mkLit   = Lit.String
instance LunaLit Int    where mkLit n = Lit.Number Num.Number { _base = 10
                                                              , _repr = Num.Decimal (show $ abs n)
                                                              , _exp  = Nothing
                                                              , _sign = if n >= 0 then Num.Positive else Num.Negative
                                                              }

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ████████╗██╗   ██╗██████╗ ███████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝
-- ██╔████╔██║███████║█████╔╝ █████╗         ██║    ╚████╔╝ ██████╔╝█████╗
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝         ██║     ╚██╔╝  ██╔═══╝ ██╔══╝
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗       ██║      ██║   ██║     ███████╗
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝       ╚═╝      ╚═╝   ╚═╝     ╚══════╝
-- ------------------------------------------------------------------------------------------------------------------------

mkTUnknown :: NType.Type f
mkTUnknown = NType.Unknown

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗     █████╗ ██████╗  ██████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ██╔══██╗██╔══██╗██╔════╝
-- ██╔████╔██║███████║█████╔╝ █████╗      ███████║██████╔╝██║  ███╗
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██╔══██║██╔══██╗██║   ██║
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ██║  ██║██║  ██║╚██████╔╝
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝
-- ------------------------------------------------------------------------------------------------------------------------

mkAArgM :: (Monad m) => Pat F -> Maybe a -> VarIDMakerT m (NArg.Arg F a)
mkAArgM pat val = do lapat <- mkLabel pat
                     return NArg.Arg { NArg._pat   = lapat
                                     , NArg._value = val
                                     }

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ██████╗ ███████╗ ██████╗██╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ██╔══██╗██╔════╝██╔════╝██║
-- ██╔████╔██║███████║█████╔╝ █████╗      ██║  ██║█████╗  ██║     ██║
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██║  ██║██╔══╝  ██║     ██║
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ██████╔╝███████╗╚██████╗███████╗
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚═════╝ ╚══════╝ ╚═════╝╚══════╝
-- ------------------------------------------------------------------------------------------------------------------------

-- undefined ###

-- ------------------------------------------------------------------------------------------------------------------------
-- ███╗   ███╗ █████╗ ██╗  ██╗███████╗    ███╗   ██╗ █████╗ ████████╗██╗██╗   ██╗███████╗
-- ████╗ ████║██╔══██╗██║ ██╔╝██╔════╝    ████╗  ██║██╔══██╗╚══██╔══╝██║██║   ██║██╔════╝
-- ██╔████╔██║███████║█████╔╝ █████╗      ██╔██╗ ██║███████║   ██║   ██║██║   ██║█████╗
-- ██║╚██╔╝██║██╔══██║██╔═██╗ ██╔══╝      ██║╚██╗██║██╔══██║   ██║   ██║╚██╗ ██╔╝██╔══╝
-- ██║ ╚═╝ ██║██║  ██║██║  ██╗███████╗    ██║ ╚████║██║  ██║   ██║   ██║ ╚████╔╝ ███████╗
-- ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝    ╚═╝  ╚═══╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═══╝  ╚══════╝                                                                                           
-- ------------------------------------------------------------------------------------------------------------------------

mkNCode :: [Either String String] -> Native e
mkNCode = Code . map (either NNative.Str NNative.Var)

mkNAST :: e -> Native e
mkNAST = NNative.AST

-- ------------------------------------------------------------------------------------------------------------------------
--  █████╗ ███████╗███████╗███████╗██████╗ ████████╗███████╗
-- ██╔══██╗██╔════╝██╔════╝██╔════╝██╔══██╗╚══██╔══╝██╔════╝
-- ███████║███████╗███████╗█████╗  ██████╔╝   ██║   ███████╗
-- ██╔══██║╚════██║╚════██║██╔══╝  ██╔══██╗   ██║   ╚════██║
-- ██║  ██║███████║███████║███████╗██║  ██║   ██║   ███████║
-- ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝
-- ------------------------------------------------------------------------------------------------------------------------

testowe :: (Monad m) => VarIDMakerT m (Expr F String)
testowe = do let x = mkPVar "x"
             y <- mkEVar "y"
             x `mkEAssignment` y

-- ------------------------------------------------------------
-- ██╗   ██╗████████╗██╗██╗     ███████╗
-- ██║   ██║╚══██╔══╝██║██║     ██╔════╝
-- ██║   ██║   ██║   ██║██║     ███████╗
-- ██║   ██║   ██║   ██║██║     ╚════██║
-- ╚██████╔╝   ██║   ██║███████╗███████║
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
-- ------------------------------------------------------------

--type TypeDecl = (String, Scheme, Expr)

---- we need no dep on lens, that's good
--type Lens s t a b = Functor f => (a -> f b) -> s -> f t

--infixl 8 ^.
--(^.) :: s -> Lens s t a b -> a
--s^.l = getConst (l Const s)

--tid :: Lens TypeDecl TypeDecl TID TID
--tid f (t, s, as) = (\(TID t') -> (t', s, as)) <$> f (TID t)

--scheme :: Lens TypeDecl TypeDecl Scheme Scheme
--scheme f (t, s, as) = (\s' -> (t, s', as)) <$> f s

--expr :: Lens TypeDecl TypeDecl Expr Expr
--expr f (t, s, as) = (\as' -> (t, s, as')) <$> f as

-- ------------------------------------------------------------
-- ██████╗ ██████╗  ██████╗  ██████╗ ██████╗  █████╗ ███╗   ███╗███████╗
-- ██╔══██╗██╔══██╗██╔═══██╗██╔════╝ ██╔══██╗██╔══██╗████╗ ████║██╔════╝
-- ██████╔╝██████╔╝██║   ██║██║  ███╗██████╔╝███████║██╔████╔██║███████╗
-- ██╔═══╝ ██╔══██╗██║   ██║██║   ██║██╔══██╗██╔══██║██║╚██╔╝██║╚════██║
-- ██║     ██║  ██║╚██████╔╝╚██████╔╝██║  ██║██║  ██║██║ ╚═╝ ██║███████║
-- ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝
-- ------------------------------------------------------------

--constBG :: TypeDecl
--constBG =
--    ( "const"
--    , Scheme [TyID "a", TyID "b"] (mkTyFun (TVar $ Tyvar $ TyID "a") (mkTyFun (TVar $ Tyvar $ TyID "b") (TVar $ Tyvar $ TyID "a")))
--    , EAbs (TyID "x") (EAbs (TyID "y") (EVar (TyID "x")))
--    )
