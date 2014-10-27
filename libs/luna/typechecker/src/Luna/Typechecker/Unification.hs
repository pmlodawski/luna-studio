module Luna.Typechecker.Unification (
    mgu
  ) where

import Luna.Typechecker.IDs
import Luna.Typechecker.Substitution
import Luna.Typechecker.TIMonad
import Luna.Typechecker.Type

import Data.Monoid


mgu :: (Monad m) => Type -> Type -> LoggerT String m Subst
mgu (TAp t1 t2) (TAp s1 s2) = do ts1 <- mgu t1 s1
                                 ts2 <- mgu (apply ts1 t2) (apply ts1 s2)
                                 return (ts2 `mappend` ts1)
mgu (TConst a) (TConst b) | a == b = return mempty
mgu (Tvar u) (Tvar v)              = unionConstraints u v
mgu (TVar u)   t                   = varBindRow u t
mgu t var@(TVar u)                 = mgu var t
mgu (TRecord row1) (TRecord row2)  = mgu row1 row2
mgu (TVariant row1) (TVariant ro2) = mgu row1 row2 
mgu (TRow (Row { fName, fType, rowTail1 })) (TRow (row2@Row {})) = do
    (fType2, rowTail2, rowInserter) <- rowInserter row2 fName
-- TODO add check for recursive rows    if occursCheck (getVariable rowTail1) rowSub then err "row occurs-check"
    if (rowInserter /= mempty && (getVariable rowTail1 == getVariable row2))
    then err "row occurs-check"
-- TODO now this is checked during varBindRow
--    else if occursCheck (fst rowInserter) fType then
--      err "row/label occurs-check"
    else do
      let rowSub = Subst [rowInserter]
      fieldSub <- mgu (apply rowSub fType) (apply rowSub fType2)
      let sub =  fieldSub `mappend` rowSub
      subRest <- mgu (apply sub rowTail1) (apply sub rowTail2)
      return $ subRest `mappend` sub
mgu t1 t2 = err "mgu not implemented" ("mgu " ++ show t1 ++ " <> " ++ show t2)

occursCheck u t = u `elem` ftv t

rowInserter EmptyRow label = err $ "label " ++ label ++ " connot be inserted"
rowInserter (Row fLabel fType row ) desc@(label, fType)
    | fLabel == label = return (fType, row, mempty)
    | TVar alpha <- row = do
          newRowVar <- newTyVarWith (lacks label) 'r'
          sub <- varBindRow alpha $ Row label fType newRowVar
          return (fType, apply sub $ Row fLabel fType newRowVar, sub)
    | otherwise = do
          (fieldTy, rowTail, sub) = rowInserter row desc
          return (fieldTy, Row label fieldTy rowTail, sub)

-- | in order to unify two type variables, we must union any lacks constraints
unionConstraints u v
  | u == v    = return nullSubst
  | otherwise =
         let c = (tyvarConstraint u) `S.union` (tyvarConstraint v)
         r <- newTyVarWith c 'r'
         return $ Subst [ (u, r), (v, r) ]

-- TODO we've got only record types
varBind :: (Monad m) => Tyvar -> Type -> LoggerT String m Subst
varBind u t | t == TVar u        = return mempty
            | occursCheck u t    = err "infinite type"
            | otherwise          = varBindRow u t

varBindRow u t | t == TVar u     = return mempty
               | occursCheck u t = err "row occursCheck"
               | otherwise       =
    case S.toList (ls `S.intersection` ls') of -- check whether t has labels for which u has lack constraints
        [] | Nothing <- mv -> return s1
      -- merge lack constraints
           | Just r1 <- mv -> do
             let c = ls `S.union` (constraint r1)
             r2 <- newTyVarWith c 'r'
             let s2 = Subst [(r1, r2)]
             return $ s2 `mappend` s1
        labels             -> throwError $ "repeated label(s): " ++ show labels
    where
        ls        = constraint u
        (ls', mv) = first getLabels $ typeToTypeList t
        s1        = Subst [(u, t)]
        getLabels = S.fromList . map (\(fLabel, fType) -> fLabel)

--
-- varBindRow var newTy = if not $ checkConstraints var newTy
--                         then err "repeated labels in constraints"
--                          else mergeRowTypes var newTy

lacks = S.singleton
-- mergeRowTypes var newTy = do
--     d

getRowVariable tTy = let (_, value) = typeToTypeList tTy in value
typeToTypeList (TVar v) = ([], Just v)
typeToTypeList RowEmpty = ([], Nothing)
typeToTypeList (Row l t r) = ((l, t):ls, mv) where
  (ls, mv) = typeToTypeList r
