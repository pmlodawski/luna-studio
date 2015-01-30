module Luna.Typechecker.Data.Type where


import Luna.Typechecker.Data.TVar



data Type = TV TVar
          | Type `Fun` Type
          deriving (Show,Eq)

