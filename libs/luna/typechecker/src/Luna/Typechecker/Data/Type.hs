module Luna.Typechecker.Data.Type where


import Flowbox.Prelude

import Luna.Typechecker.Data.TVar


type Fieldlabel = String


type Field = (Fieldlabel, Type)


data Type = TV TVar
          | Type `Fun` Type
          | Record [Field]
          deriving (Show,Eq)
