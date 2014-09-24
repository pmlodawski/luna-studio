module Luna.Typechecker.ContextReduction (
    reduce, inHnf, toHnf
  ) where


import Luna.Typechecker.Typeclasses     (Pred(..),ClassEnv(..),byInst,entail)

import Luna.Typechecker.AST.Type        (Type(..))

import Luna.Typechecker.Internal.Logger

import Control.Monad                    (foldM)



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
  ttt <- inHnf p
  if ttt
    then return [p]
    else do ps <- byInst ce p
            toHnfs ce ps
--toHnf ce p | inHnf p = return [p]
--           | otherwise = case byInst ce p of
--                           Nothing -> fail "context reduction"
--                           Just ps -> toHnfs ce ps

simplify :: (Monad m) => ClassEnv -> [Pred] -> TCLoggerT m [Pred]
              -- ([Pred] -> b -> m [Pred]) -> [Pred] -> [b] -> m [Pred]
simplify ce = loop []
  where loop rs [] = return rs
        loop rs (p:ps) = do
          ttt <- entail ce (rs ++ ps) p
          if ttt
            then loop rs ps
            else loop (p:rs) ps
  --where eliminate :: [Pred] -> Pred -> LoggerT String Identity [Pred]
  --      eliminate rs (p:ps) = do
  --        ttt <- entail ce (rs++ps) p
  --        if ttt
  --          then return rs
  --          else return (p:rs)



--simplify ce = loop []
--  where loop rs []                              = rs
--        loop rs (p:ps) | entail ce (rs ++ ps) p = loop rs ps
--                       | otherwise              = loop (p:rs) ps

reduce :: (Monad m) => ClassEnv -> [Pred] -> TCLoggerT m [Pred]
reduce ce ps = do qs <- toHnfs ce ps
                  simplify ce qs
