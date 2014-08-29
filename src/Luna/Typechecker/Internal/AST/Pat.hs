module Luna.Typechecker.Internal.AST.Pat (tiPat, Pat(..), tiPats) where

import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl

import           Luna.Typechecker.Internal.AST.Common       (ID)
import           Text.Printf                                (printf)
import           Data.List                                  (intercalate)


--data Pat = Con             { _id :: ID, _name      :: String                          }  -- to jest tylko konstruktor
--         | Var             { _id :: ID, _name      :: String                          }
--         | Wildcard        { _id :: ID                                                }
--         | Lit             { _id :: ID, _value     :: Lit                             }
         
--         | Tuple           { _id :: ID, _items     :: [Pat]                           }
--         | App             { _id :: ID, _src       :: Pat       , _args      :: [Pat] }
--         | Typed           { _id :: ID, _pat       :: Pat       , _cls       :: Type  }
--         | RecWildcard     { _id :: ID                                                }
--         deriving (Show, Eq, Generic, Read)





-- TODO [kgdk] 21 sie 2014: _scheme w Con musi być osobno
data Pat = Con             { _id :: ID, _name :: String, _scheme :: Sch.Scheme, _args :: [Pat] }
         | Var             { _id :: ID, _name :: String                                        }
         | Wildcard        { _id :: ID                                                         }
         | Lit             { _id :: ID, _value     :: Lit.Lit                                  }
         deriving (Eq)
-- TODO [kgdk] 20 sie 2014: ScopedTypeVariables?
-- TODO [kgdk] 20 sie 2014: odpowiednik 'PAs' tj. named pattern?

instance Show Pat where
    show (Con _ n _ a) = printf "%s %s" n (show a)
    show (Var _ n)     = n
    show (Wildcard _)  = "_"
    show (Lit _ l)     = show l

    showList cs s = printf "%s%s" s (intercalate " " $ map show cs)

--instance  Show Char  where
    --showsPrec _ '\'' = showString "'\\''"
    --showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    --showList cs = showChar '"' . showLitString cs . showChar '"'


tiPat :: Pat -> TIM.TI ([Tcl.Pred], [Ass.Assump], Ty.Type)
tiPat (Con _ _ sc pats) = do (ps,as,ts) <- tiPats pats
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


