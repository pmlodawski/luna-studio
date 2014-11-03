{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Typechecker.AST.Expr (
    Expr(..)
  ) where

-- luna-core
import Luna.ASTNew.Expr


instance Show (Expr f a) where
  show _ = "<expr>"