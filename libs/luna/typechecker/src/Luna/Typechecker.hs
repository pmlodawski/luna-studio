{-# LANGUAGE RankNTypes #-}
module Luna.Typechecker where


--module Luna.Typechecker (
--  ) where
--import Luna.Typechecker.TypecheckClass ()

import Luna.Typechecker.AST            (Expr(..))
import Luna.Typechecker.Substitution   ()
import Luna.Typechecker.TIMonad        ()
import Luna.Typechecker.IDs            (TyID(..),VarID(..))
import Luna.Typechecker.Type           (Scheme(..),Type(..),Tyvar(..),mkTyFun)
import Luna.Typechecker.TypeEnv        ()
import Luna.Typechecker.TypecheckClass ()

import Logger ()

import Control.Applicative



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

tid :: Lens TypeDecl TypeDecl TyID TyID
tid f (t, s, as) = (\(TyID t') -> (t', s, as)) <$> f (TyID t)

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
    , EAbs (VarID "x") (EAbs (VarID "y") (EVar (VarID "x")))
    )




