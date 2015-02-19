---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Core.Expr where

import Flowbox.Prelude

-- pomyslec o uzywaniu Bound tu tez
-- + do czego jest Core?


--data Expr b = Var  b
--            | Lit  Literal
--            | App  (Expr b) (Arg b)
--            | Lam  b (Expr b)
--            | Let  (Bind b) (Expr b)
--            | Case (Expr b) b [Alt b]
--            | Acc  FastString (Expr b)
--            | Call (Expr b)
--            | RecUpdt (Expr b) FastString (Expr b)
--            deriving (Show, Eq, Generic, Read)


--type Arg b = Expr b


--data Bind b = NonRec b (Expr b) 
--            | Rec [(b, (Expr b))]


--data Alt b = Data b [b]
--           | Lit  Literal b
--           | DEFAULT


--data Decl = Data FastString DataParams [Cons]
--          | Alias


--data Cons = Cons FastString 


--0) Call jest zle! bo nie wiadomo ile lambd callowac jezeli lambdy sa jednoargumentowe!

--1) do czego jest core?
--    - core moze byc podobny do hs cora - w pelni otypowany, bez typeclass / interfejsow / funkcji polimorficznych (poly lambdy?)
--    - moze tez byc interfacem na ktorym robimy optymalizacje i on leci do plików prekompilowanych Luny - tylko ze one musza zawierac np. interfejsy
--      moze nie powinien byc uzywany w plikach prekompilowanych? a gdzie optymalizacje w takich plikach?

--2) Co zawiera core?
--    - rozwiazane patterny
--    - brak smaczkow jak groupped lub type wrappers
--    - 

--3) moze warto wyczyscic AST? pozwolic na unifikacje nazw? Moze nazwy zbindowac z variablami i typami?
--   moze warto wyrzucic grouped i w klasach uporzadkowac metody etc?
--   moze unifikacja nazw jest niepotrzebna?


--Txt ->  Core  -> Backends
--Viz     Opt
--TC      files

--    --data Decl a e
--    --= Data   (DataDecl a e)
--    --| Func   (FuncDecl a e [e])
--    --| Imp    { _modPath :: Path    , _rename   :: Maybe TNameP , _targets :: [ImpTgt]                                 }
--    --| TpAls  { _dstType :: LType a , _srcType  :: LType a                                                             }
--    --| TpWrp  { _dstType :: LType a , _srcType  :: LType a                                                             }
--    --| Foreign (Foreign (ForeignDecl a e))
--    --deriving (Show, Eq, Generic,




----data Name = Name { _sort :: NameSort 
----                 , _occ  :: OccName
----                 -- , _loc 
----                 }


----data OccName = OccName { _nameSpace  :: !NameSpace
----                       , _repr       :: !FastString
----                       }


----data NameSpace = VarName
----               | DataName


----data NameSort = External Module
----              | Internal
----              | System


--data Module = Module { _pkgKey :: PackageKey 
--                     , _name   :: ModuleName
--                     , 
--                     }

--data QualifiedName = QualifiedName { _path :: [FastString] 
--                                   , _name :: FastString
--                                   }

--type ModuleName = QualifiedName

----newtype PackageKey = PackageKey FastString


----data Var = Var { _name :: !Name 
----               , _cls  :: Type
----               }



----data Type = Type -- ...

--data PackageName = PackageName QualifiedName


--Map ModuleKey 


--data Package = Package { _name    :: PackageName
--                       , _version :: Version
--                       , _modules :: [ModuleKey] 
--                       ,  
--                       }


--data PackageManager = PackageManager { _pkgs    :: [Package]
--                                     , _pkgDefs :: Map PackageKey ModuleDef
--                                     }


--data ModuleDef = ModuleDef [Import] [Decls] [Bind Var]


--data Import = Import PackageName (Maybe Version)