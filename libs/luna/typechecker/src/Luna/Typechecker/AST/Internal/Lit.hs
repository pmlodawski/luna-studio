{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Typechecker.AST.Internal.Lit (
    Lit(..)
  ) where


import Text.Printf                                (printf)


data Lit = LitChar     Char
         | LitFloat    Float
         | LitInt      Integer
         | LitIntegral Int
         | LitStr      String


instance Show Lit where
  show (LitChar    c)  = printf "'%c'" c
  show (LitFloat   f)  = printf "%.3f" f
  show (LitInt     i)  = show i
  show (LitIntegral i) = show i
  show (LitStr     s)  = show s
