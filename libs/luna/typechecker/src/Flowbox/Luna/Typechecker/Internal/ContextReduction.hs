module Flowbox.Luna.Typechecker.Internal.ContextReduction (reduce) where

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
import qualified Flowbox.Luna.Typechecker.Internal.AST.Type         as Ty

--import qualified Flowbox.Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Flowbox.Luna.Typechecker.Internal.Assumptions      as Ass
--import qualified Flowbox.Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Flowbox.Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Flowbox.Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Flowbox.Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Flowbox.Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Flowbox.Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Flowbox.Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Flowbox.Luna.Typechecker.Internal.Unification      as Unf

--import           Flowbox.Luna.Data.AST.Common                       (ID)
--import           Flowbox.Luna.Typechecker.Internal.AST.TID          (TID(..))



inHnf :: Tcl.Pred -> Bool
inHnf (Tcl.IsIn c t) = hnf t
  where hnf (Ty.TVar v ) = True
        hnf (Ty.TCon tc) = False  -- TODO [kgdk] 18 sie 2014: well, why actually can't this be true?
        hnv (Ty.TAp t _) = hnf t



toHnfs :: Monad m => Tcl.ClassEnv -> [Tcl.Pred] -> m [Tcl.Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)
-- TODO [kgdk] 18 sie 2014: sprawdzić, czy to poniżej działa jako zamiennik
-- toHnfs ce ps = liftM concat $ mapM (toHnf ce)

-- TODO [kg]: doprowadzić do pojawienia się błędu "context reduction", zbadać przypadek
toHnf :: Monad m => Tcl.ClassEnv -> Tcl.Pred -> m [Tcl.Pred]
toHnf ce p | inHnf p = return [p]
           | otherwise = case Tcl.byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify :: Tcl.ClassEnv -> [Tcl.Pred] -> [Tcl.Pred]
simplify ce = loop []
  where loop rs []                              = rs
        loop rs (p:ps) | Tcl.entail ce (rs ++ ps) p = loop rs ps
                       | otherwise              = loop (p:rs) ps
-- TODO [kgdk] 18 sie 2014: sprawdzić, czy to poniżej działa jako zamiennik
-- simplify ce = foldl eliminate []
--  where eliminate rs (p:ps) | entail ce (rs ++ ps) p = rs
--                            | otherwise              = (p:rs)

reduce :: Monad m => Tcl.ClassEnv -> [Tcl.Pred] -> m [Tcl.Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)
-- TODO [kgdk] 18 sie 2014: sprawdzić, czy to poniżej działa jako zamiennik
--reduce ce = liftM (simplify ce) $ toHnfs ce
-- TODO [kgdk] 18 sie 2014: w 'reduce' mamy zagwarantowane, że wszystko jest w HNF, więc
-- można by skorzystać z następującego (okrojonego) entail
--scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
--scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
