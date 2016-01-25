module Luna.Syntax.AST.Layout where

import Prelude


data Dynamic = Dynamic deriving (Show)
data Static  = Static  deriving (Show)


type family ByLayout layout static dynamic where
    ByLayout Static   static dynamic = static
    ByLayout Dynamic  static dynamic = dynamic

type        SubSemiLayouts l = l ': SubLayouts l
type family SubLayouts     l where SubLayouts Static  = '[]
                                   SubLayouts Dynamic = '[Static]


type family ToStatic  a :: *
type family ToDynamic a :: *

type instance ToStatic Static  = Static
type instance ToStatic Dynamic = Static

type instance ToDynamic Static  = Dynamic
type instance ToDynamic Dynamic = Dynamic