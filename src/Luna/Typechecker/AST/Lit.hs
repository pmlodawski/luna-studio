module Luna.Typechecker.AST.Lit (
    Lit(..)
  ) where


import Text.Printf


data Lit = LitChar   Char
         | LitDouble Double
         | LitInt    Integer
         | LitStr    String


instance Show Lit where
  show (LitChar   c) = printf "'%c'" c
  show (LitDouble f) = printf "%.3f" f
  show (LitInt    i) = show i
  show (LitStr    s) = show s