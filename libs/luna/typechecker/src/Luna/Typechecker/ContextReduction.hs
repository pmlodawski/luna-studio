module Luna.Typechecker.ContextReduction (
    reduce, inHnf, toHnf
  ) where


import Luna.Typechecker.Typeclasses     (Pred(..),ClassEnv(..),byInst,entail)

import Luna.Typechecker.AST.Type        (Type(..))

import Luna.Typechecker.Internal.Logger


inHnf :: (Monad m) => Pred -> TCLoggerT m Bool
inHnf (IsIn _ t) = hnf t
  where hnf (TVar   _ ) = return True
        hnf (TCon   _ ) = return False
        hnf (TAp t1 _ ) = hnf t1
        hnf (TGen   _ ) = throwError "ContextReduction.hs:inHnf got TGen!"

toHnfs :: (Monad m) => ClassEnv -> [Pred] -> TCLoggerT m [Pred]
toHnfs ce ps = do pss <- mapM (toHnf ce) ps
                  return (concat pss)

toHnf :: (Monad m) => ClassEnv -> Pred -> TCLoggerT m [Pred]
toHnf ce p = do
  alreadyHNF <- inHnf p
  if alreadyHNF
     then return [p]
     else do ps <- byInst ce p `catchError` (\_ -> throwError "context reduction")
             toHnfs ce ps

simplify :: (Monad m) => ClassEnv -> [Pred] -> TCLoggerT m [Pred]
simplify ce = loop []
  where loop rs []     = return rs
        loop rs (p:ps) = do isEntailed <- entail ce (rs ++ ps) p
                            if isEntailed
                              then loop rs ps
                              else loop (p:rs) ps

reduce :: (Monad m) => ClassEnv -> [Pred] -> TCLoggerT m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  simplify ce qs
