---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Luna.Syntax.AST (
    module Luna.Syntax.AST
) where

import           GHC.Generics       (Generic)

import           Flowbox.Prelude
import           Luna.Syntax.Decl   (Decl)
import           Luna.Syntax.Expr   (Expr)
import           Luna.Syntax.Lit    (Lit)
import           Luna.Syntax.Module (Module)
import           Luna.Syntax.Pat    (Pat)
import           Luna.Syntax.Type   (Type)


type ID = Int

data AST a e v = Module { fromModule :: Module a e }
               | Decl   { fromDecl :: Decl   a e }
               | Expr   { fromExpr :: Expr   a v }
               | Lit    { fromLit :: Lit        }
               | Pat    { fromPat :: Pat    a   }
               | Type   { fromType :: Type   a   }
               deriving (Show, Generic)


class ASTWrapper a b where
    astWrap :: a -> b


instance (a~a', e~e') => ASTWrapper (Module a e) (AST a' e' v ) where astWrap = Module
instance (a~a', e~e') => ASTWrapper (Decl   a e) (AST a' e' v ) where astWrap = Decl
instance (a~a', v~v') => ASTWrapper (Expr   a v) (AST a' e  v') where astWrap = Expr
instance                 ASTWrapper (Lit       ) (AST a  e  v ) where astWrap = Lit
instance (a~a')       => ASTWrapper (Pat    a  ) (AST a' e  v ) where astWrap = Pat
instance (a~a')       => ASTWrapper (Type   a  ) (AST a' e  v ) where astWrap = Type

