---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Luna.ASTNew.AST (
    module Luna.ASTNew.AST
) where

import GHC.Generics        (Generic)

import Flowbox.Prelude
import Luna.ASTNew.Expr   (Expr)
import Luna.ASTNew.Lit    (Lit)
import Luna.ASTNew.Module (Module)
import Luna.ASTNew.Decl   (Decl)
import Luna.ASTNew.Pat    (Pat)
import Luna.ASTNew.Type   (Type)


type ID = Int

data AST a e v = Module { fromModule :: Module a e }
               | Decl   { fromDecl   :: Decl   a e }
               | Expr   { fromExpr   :: Expr   a v }
               | Lit    { fromLit    :: Lit        }
               | Pat    { fromPat    :: Pat    a   }
               | Type   { fromType   :: Type   a   }
               deriving (Show, Eq, Generic, Read)


class Wrapper a b where
    wrap :: a -> b


instance (a~a', e~e') => Wrapper (Module a e) (AST a' e' v ) where wrap = Module
instance (a~a', e~e') => Wrapper (Decl   a e) (AST a' e' v ) where wrap = Decl
instance (a~a', v~v') => Wrapper (Expr   a v) (AST a' e  v') where wrap = Expr
instance                 Wrapper (Lit       ) (AST a  e  v ) where wrap = Lit
instance (a~a')       => Wrapper (Pat    a  ) (AST a' e  v ) where wrap = Pat
instance (a~a')       => Wrapper (Type   a  ) (AST a' e  v ) where wrap = Type

