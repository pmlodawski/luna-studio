module Luna.Typechecker.Data.Typo where


import Luna.Syntax.Enum     (ID)

import Luna.Typechecker.Data.TypeScheme


type Typo           = [(ID,TypeScheme)]


empty_typo :: Typo
empty_typo = []


init_typo :: [Typo]
init_typo = [empty_typo]