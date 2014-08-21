module Flowbox.Luna.Typechecker.Internal.AST.Scheme (Scheme(..), toScheme, quantify) where

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
import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID(..))

-- TODO [kgdk] 20 sie 2014: przenieść Scheme.hs do AST/Scheme.hs
data Scheme = Forall [Knd.Kind] (Tcl.Qual Ty.Type)
            deriving (Eq,Show)


instance Sub.Types Scheme where
  apply s (Forall ks qt) = Forall ks (Sub.apply s qt)
  tv (Forall ks qt) = Sub.tv qt


quantify :: [Ty.Tyvar] -> Tcl.Qual Ty.Type -> Scheme
quantify vs qt = Forall ks (Sub.apply s qt)
  where vs' = [v | v <- Sub.tv qt, v `elem` vs]
        ks  = map HKd.kind vs'
        s   = zip vs' (map Ty.TGen [0..])

toScheme :: Ty.Type -> Scheme
toScheme t = Forall [] ([] Tcl.:=> t)



