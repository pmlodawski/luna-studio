module Flowbox.Luna.Typechecker.Internal.Scheme (Scheme(..), quantify) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.AST    as AST
import qualified Flowbox.Luna.Typechecker.Internal.AST.Common as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr   as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind   as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit    as Lit
import qualified Flowbox.Luna.Typechecker.Internal.AST.Module as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat    as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID    as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type   as Ty

import           Flowbox.Luna.Data.AST.Common                 (ID(..))
import           Flowbox.Luna.Typechecker.Internal.AST.TID    (TID(..))


data Scheme = Forall [Knd.Kind] (Qual Ty.Type)
            deriving (Eq,Show)


instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall ks qt) = tv qt


quantify :: [Ty.Tyvar] -> Qual Ty.Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [v | v <- tv qt, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map Ty.TGen [0..])

toScheme :: Ty.Type -> Scheme
toScheme t = Forall [] ([] :=> t)



