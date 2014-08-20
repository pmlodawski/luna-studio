module Flowbox.Luna.Typechecker.Internal.AST.Pat (Pat(..)) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.AST    as AST
import qualified Flowbox.Luna.Typechecker.Internal.AST.Common as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr   as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind   as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit    as Lit
import qualified Flowbox.Luna.Typechecker.Internal.AST.Module as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID    as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type   as Ty

import           Flowbox.Luna.Data.AST.Common                 (ID(..))
import           Flowbox.Luna.Typechecker.Internal.AST.TID    (TID(..))

data Pat =
           App             { _id :: ID, _src       :: Pat       , _args      :: [Pat] }
         | Con             { _id :: ID, _name      :: String                          }
         | Lit             { _id :: ID, _value     :: Lit                             }
--          | RecWildcard     { _id :: ID                                                }
--          ^-- zbędne bo można App + zagnieżdżone Wildcard
--          | Tuple           { _id :: ID, _items     :: [Pat]                           }
--          ^-- zbędne bo Con to załatwia
--          | Typed           { _id :: ID, _pat       :: Pat       , _cls       :: Type  }
-- TODO [kgdk] 20 sie 2014: ScopedTypeVariables?
         | Var             { _id :: ID, _name      :: String                          }
         | Wildcard        { _id :: ID                                                }
         deriving (Show, Eq)


tiPat :: Pat -> TI ([Pred], [Assump], Ty.Type)
tiPat (Wildcard _) = do v <- newTVar Knd.Star
                        return ([], [], v) 
tiPat (Var _ varName) = do v <- newTVar Knd.Star
                           return ([], [varName :>: toScheme v], v)