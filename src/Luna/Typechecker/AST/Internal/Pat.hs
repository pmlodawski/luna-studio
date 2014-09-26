module Luna.Typechecker.AST.Internal.Pat (Pat(..)) where


import Luna.Typechecker.Assumptions      (Assump(..))

import Luna.Typechecker.AST.Lit          (Lit)
import Luna.Typechecker.AST.TID          (TID)

import Text.Printf


data Pat = PVar TID
         | PWildcard
         | PAs TID Pat
         | PLit Lit
         | PCon Assump [Pat]


instance Show Pat where
  show (PVar tid)            = printf "pvar %s" (show tid)
  show PWildcard             = "_"
  show (PAs tid pat)         = printf "(%s PAs %s)" (show tid) (show pat)
  show (PLit lit)            = show lit
  show (PCon (i:>:sch) pats) = printf "(%s::%s) %s" (show i) (show sch) (show pats)

