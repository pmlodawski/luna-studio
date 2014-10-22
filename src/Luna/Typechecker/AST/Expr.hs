module Luna.Typechecker.AST.Expr (
    Expr(..)
  ) where


import Luna.Typechecker.IDs (VarID)
import Luna.Typechecker.AST.Lit (Lit)


data Expr = EVar VarID
          | ELit Lit
          | EApp Expr  Expr
          | EAbs VarID Expr
          | ELet VarID Expr Expr

instance Show Expr where
  show _ = "<expr>"