module Flowbox.Luna.Typechecker.Internal.AST.Lit (Lit(..)) where

import qualified Flowbox.Luna.Typechecker.Internal.AST.AST    as AST
import qualified Flowbox.Luna.Typechecker.Internal.AST.Common as Com
import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr   as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind   as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Module as Mod
import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat    as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.TID    as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type   as Ty

import           Flowbox.Luna.Data.AST.Common                 (ID(..))
import           Flowbox.Luna.Typechecker.Internal.AST.TID    (TID(..))

data Lit = Char    { _id :: ID, _char :: Prelude.Char   }
         | Float   { _id :: ID, _str  :: Prelude.String }
         | Integer { _id :: ID, _str  :: Prelude.String }
         | String  { _id :: ID, _str  :: Prelude.String }
         deriving (Show, Eq)


tiLit :: Lit -> TI ([Pred], Ty.Type)
tiLit (Char    _ _) = return ( [], Ty.tChar )
tiLit (Float   _ _) = return ( [], Ty.tFloat ) -- 'tFloat' of 'Fractional'?
tiLit (Integer _ _) = return ( [], Ty.tInteger )
--tiLit (Integer _ _) = do v <- newTVar Knd.Star
--                         return ([IsIn "Num" v], v)
tiLit (String  _ _) = return ( [], Ty.tString )