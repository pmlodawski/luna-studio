module Flowbox.Luna.Typechecker.Internal.Assumptions () where

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



data Assump = TID :>: Scheme
            deriving (Show)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc) = tv sc

-- TODO [kgdk] 19 sie 2014: por. z findM
find :: Monad m => TID -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as) = if i==i'
                              then return sc
                              else find i as
