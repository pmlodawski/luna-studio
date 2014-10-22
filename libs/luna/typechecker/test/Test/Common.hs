module Test.Common where

import Luna.Typechecker.AST
import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type
import Luna.Typechecker.TypeEnv
import Luna.Typechecker.TypecheckClass
import Logger

-- ------------------------------------------------------------
-- ██╗   ██╗████████╗██╗██╗     ███████╗
-- ██║   ██║╚══██╔══╝██║██║     ██╔════╝
-- ██║   ██║   ██║   ██║██║     ███████╗
-- ██║   ██║   ██║   ██║██║     ╚════██║
-- ╚██████╔╝   ██║   ██║███████╗███████║
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
-- ------------------------------------------------------------

type TypeDecl = (String, Scheme, Expr)

-- we need no dep on lens, that's good
type Lens s t a b = Functor f => (a -> f b) -> s -> f t

infixl 8 ^.
(^.) :: s -> Lens s t a b -> a
s^.l = getConst (l Const s)

tid :: Lens TypeDecl TypeDecl TID TID
tid f (t, s, as) = (\(TID t') -> (t', s, as)) <$> f (TID t)

scheme :: Lens TypeDecl TypeDecl Scheme Scheme
scheme f (t, s, as) = (\s' -> (t, s', as)) <$> f s

expr :: Lens TypeDecl TypeDecl Expr Expr
expr f (t, s, as) = (\as' -> (t, s, as')) <$> f as

-- ------------------------------------------------------------
-- ██████╗ ██████╗  ██████╗  ██████╗ ██████╗  █████╗ ███╗   ███╗███████╗
-- ██╔══██╗██╔══██╗██╔═══██╗██╔════╝ ██╔══██╗██╔══██╗████╗ ████║██╔════╝
-- ██████╔╝██████╔╝██║   ██║██║  ███╗██████╔╝███████║██╔████╔██║███████╗
-- ██╔═══╝ ██╔══██╗██║   ██║██║   ██║██╔══██╗██╔══██║██║╚██╔╝██║╚════██║
-- ██║     ██║  ██║╚██████╔╝╚██████╔╝██║  ██║██║  ██║██║ ╚═╝ ██║███████║
-- ╚═╝     ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝
-- ------------------------------------------------------------

constBG :: TypeDecl
constBG =
    ( "const"
    , Scheme [TyID "a", TyID "b"] (mkTyFun (TVar $ Tyvar $ TyID "a") (mkTyFun (TVar $ Tyvar $ TyID "b") (TVar $ Tyvar $ TyID "a")))
    , EAbs (TyID "x") (EAbs (TyID "y") (EVar (TyID "x")))
    )




