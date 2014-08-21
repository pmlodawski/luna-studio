module Flowbox.Luna.Typechecker.Internal.AST.Module () where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
--import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID(..))

--import Flowbox.Luna.Typechecker.Internal.AST.Common (ID)
--import Flowbox.Luna.Typechecker.Internal.AST.Type   (Type)
--import Flowbox.Luna.Typechecker.Internal.AST.Expr   (Expr)

-- # data Module = Module {
-- #                   _id          :: ID
-- #                 , _cls         :: Type
-- #                 , _imports     :: [Expr]
-- #                 , _classes     :: [Expr]
-- #                 , _typeAliases :: [Expr]
-- #                 , _typeDefs    :: [Expr]
-- #                 , _fields      :: [Expr]
-- #                 , _methods     :: [Expr]
-- #                 , _modules     :: [Module]
-- #             }
-- #             deriving (Show)
