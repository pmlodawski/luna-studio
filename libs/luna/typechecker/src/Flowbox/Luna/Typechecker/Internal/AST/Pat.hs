module Flowbox.Luna.Typechecker.Internal.AST.Pat (tiPat, Pat, tiPats) where

--import qualified Flowbox.Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Flowbox.Luna.Typechecker.Internal.AST.AST          as AST
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Common       as Com
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Flowbox.Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Flowbox.Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Flowbox.Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Flowbox.Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Flowbox.Luna.Typechecker.Internal.AST.TID          as TID
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID(..))

data Pat = Con             { _id :: ID, _name :: String, _scheme :: Sch.Scheme, _args :: [Pat] }
         | Var             { _id :: ID, _name :: String                                        }
         | Wildcard        { _id :: ID                                                         }
         | Lit             { _id :: ID, _value     :: Lit.Lit                                  }
         deriving (Show, Eq)
-- TODO [kgdk] 20 sie 2014: ScopedTypeVariables?
-- TODO [kgdk] 20 sie 2014: odpowiednik 'PAs' tj. named pattern?


tiPat :: Pat -> TIM.TI ([Tcl.Pred], [Ass.Assump], Ty.Type)
tiPat (Con _ i sc pats) = do (ps,as,ts) <- tiPats pats
                             t' <- TIM.newTVar Knd.Star
                             (qs Tcl.:=> t) <- TIM.freshInst sc
                             TIM.unify t (foldr Ty.fn t' ts)
                             return (ps++qs, as, t')
tiPat (Wildcard _) = do v <- TIM.newTVar Knd.Star
                        return ([], [], v) 
tiPat (Var _ varName) = do v <- TIM.newTVar Knd.Star
                           return ([], [varName Ass.:>: Sch.toScheme v], v)
tiPat (Lit _ l) = do (ps, t) <- Lit.tiLit l
                     return (ps, [], t)


-- TODO [kgdk] 20 sie 2014: które lepsze/wydajniejsze?
--import           Control.Lens                                 ((&), (^..), traverse, _1, _2, _3)
--tiPats pats = do psasts <- mapM tiPat pats
--                 let ps = concatMap (^. _1) psasts
--                     as = concatMap (^. _2) psasts
--                     ts =       map (^. _3) psasts
--                 return (ps, as, ts)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return $ (concatMap (^. _1) psasts, concatMap (^. _2) psasts, psasts^..traverse._3)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return (psasts^..traverse._1 & concat, psasts^..traverse._2 & concat, psasts^..traverse._3)
--tiPats pats = do psasts <- mapM tiPat pats
--                 return $ _1%~concat $ _2%~concat $ foldr (\(a,b,c) (x,y,z) -> (a:x, b:y, c:z)) ([],[],[]) psasts
tiPats :: [Pat] -> TIM.TI ([Tcl.Pred], [Ass.Assump], [Ty.Type])
tiPats pats = do psasts <- mapM tiPat pats
                 let ps = concat [ps' | (ps', _,  _) <- psasts]
                     as = concat [as' | ( _, as', _) <- psasts]
                     ts =        [t   | ( _,  _,  t) <- psasts]
                 return (ps, as, ts)


