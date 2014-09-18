module Luna.Typechecker.ContextReduction (
    reduce, inHnf, toHnf
  ) where

import Luna.Typechecker.AST.Type         (Type(..))

import Luna.Typechecker.Typeclasses      (Pred(..),ClassEnv(..),byInst,entail)




inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where hnf (TVar   _ ) = True
        hnf (TCon   _ ) = False
        hnf (TAp t1 _ ) = hnf t1
        hnf (TGen   _ ) = error "ContextReduction.hs:inHnf got TGen!"



toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p | inHnf p = return [p]
           | otherwise = case byInst ce p of
                           Nothing -> fail "context reduction"
                           Just ps -> toHnfs ce ps

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where loop rs []                              = rs
        loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
                       | otherwise              = loop (p:rs) ps

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  return (simplify ce qs)
