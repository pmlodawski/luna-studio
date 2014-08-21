module Flowbox.Luna.Typechecker.Internal.HasKind (HasKind(..)) where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

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


-- TODO [kgdk] 21 sie 2014: zmienić nazwę HaskKind -> ClassHasKind

class HasKind t where
    kind :: t -> Knd.Kind -- ^ Determine the kind of a type variable, type constant, or type expression.

instance HasKind Ty.Tyvar where
  kind (Ty.Tyvar _ k) = k

instance HasKind Ty.Tycon where
  kind (Ty.Tycon _ k) = k

instance HasKind Ty.Type where
  kind (Ty.TVar u)   = kind u
  kind (Ty.TCon tc)  = kind tc
  kind (Ty.TAp t _) = case kind t of
                         (Knd.Kfun _ k) -> k
                         _              -> error "kind mismatch"
                         -- TODO [kgdk] 21 sie 2014: checking, że t' == k'

-- TODO [kgdk] 14 sie 2014: zmienić typ zwracany kind na Either by obsługiwać errory
