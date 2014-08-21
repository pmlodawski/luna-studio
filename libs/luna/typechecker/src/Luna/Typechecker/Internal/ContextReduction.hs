module Luna.Typechecker.Internal.ContextReduction (reduce) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl




inHnf :: Tcl.Pred -> Bool
inHnf (Tcl.IsIn _ t) = hnf t
  where hnf (Ty.TVar   _ ) = True
        hnf (Ty.TCon   _ ) = False  -- TODO [kgdk] 18 sie 2014: well, why actually can't this be true?
        hnf (Ty.TAp t1 _ ) = hnf t1
        hnf (Ty.TGen   _ ) = error "ContextReduction.hs:inHnf got TGen!" -- TODO [kgdk] 21 sie 2014: czy może tylko False?



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
