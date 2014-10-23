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
                                 return (ts1 `mappend` ts2)
mgu (TConst a) (TConst b) | a == b = return mempty
mgu (TConst _) (TConst _) = err "cantunify" "blahblah"
mgu (TVar u)   t          = varBind u t
mgu t var@(TVar u)            = mgu var t
mgu (TRow (Row { fName, fType, rowTail1 })) (TRow (row2@Row {})) = do
    (fType2, rowTail2, rowSub) <- matchRow row2 fName
    if occursCheck (getVariable rowTail1) rowSub then err "row occur-check"
    else do
      fieldSub <- unify (apply rowSub fType) (apply rowSub fType2)
      let sub = rowSub `mappend` fieldSub
      subRest <- unify (apply sub rowTail1) (apply sub rowTail2)
      return $ subRest `mappend` sub

mgu t1 t2 = err "mgunotimplemented" ("mgu " ++ show t1 ++ " <> " ++ show t2)

-- | unnecessary when proper unification algorithm will be used
occursCheck a b = false 

varBind :: (Monad m) => Tyvar -> Type -> LoggerT String m Subst
varBind u t | t == TVar u        = return mempty
            | u `elem` ftv t     = err "infinitetype" "blehbleh"
            | otherwise          = return (Subst [(u, t)])

matchRow EmptyRow label = err $ "label " ++ label ++ " connot be inserted"
matchRow (Row fLabel fType row ) label 
    | fLabel == label = return (fType, row, mempty)
    | TVar alpha <- row = do
          newRowVar <- newTyVarWith (lacks label) 'r'
          tyVar <- newTyVar 'a'
          sub <- varBindRow alpha $ Row label tyVar newRowVar
          return (tyVar, apply sub $ Row fLabel fType newRowVar, sub)
    | otherwise = do
          (fieldTy, rowTail, sub) = matchRow row label
          return (fieldTy, Row label fieldTy rowTail, sub)

varBindRow var newTy = if not $ checkConstraints var newTy
                         then err "repeated labels in constraints"
                         else mergeRowTypes var newTy

lacks = S.singleton
mergeRowTypes var newTy = do
    d

getRowVariable tTy = let (_, value) = typeToTypeList tTy in value
typeToTypeList (TVar v) = ([], Just v)
typeToTypeList RowEmpty = ([], Nothing)
typeToTypeList (Row l t r) = ((l, t):ls, mv) where
  (ls, mv) = typeToTypeList r
