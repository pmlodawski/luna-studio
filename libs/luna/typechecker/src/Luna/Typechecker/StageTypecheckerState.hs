{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Luna.Typechecker.StageTypecheckerState (
    module StageTypecheckerStateClass,
    report_error,
    withTypo, withClonedTypo, withClonedTypo0,
    insertNewMonoTypeVariable,
    getTypeById, setTypeById, getTypeSchemeById, setTypeSchemeById,
    getEnv, add_constraint, newtvar, rename,
    debugPush,
    getTargetIDString, getTargetID
  ) where


import            Control.Applicative
import            Control.Lens
import            Control.Monad.State.Class                     (MonadState(..))
import            Data.Maybe
import            Data.Monoid

import qualified  Luna.Data.StructInfo                          as SI

import qualified  Luna.Pass                                     as Pass

import            Luna.Syntax.Enum                              (ID)
import qualified  Luna.Syntax.Enum                              as Enum

import            Luna.Typechecker.Data                         (
                      TVar, Var,
                      Subst, Typo,
                      Type(..), Constraint(..), TypeScheme(..)
                  )
import            Luna.Typechecker.Inference.Class              (StageTypecheckerCtx, StageTypecheckerPass)
import            Luna.Typechecker.StageTypecheckerState.Class  as StageTypecheckerStateClass


report_error :: (Monad m) => String -> a -> StageTypecheckerPass m a
report_error msg x = do
    st <- get
    let msgRes = "LUNA TC ERROR: " ++ msg ++ "\nState:\n\n" ++ show st
    Pass.fail msgRes


withTypo :: (Monad m) => Typo -> a -> (a -> StageTypecheckerPass m b) -> StageTypecheckerPass m b
withTypo typeEnv astElem action = push *> action astElem <* pop
  where
    push        = typo %= (typeEnv:)
    pop         = typo %= safeTail
    safeTail xs = fromMaybe xs (xs ^? _tail)


withClonedTypo :: (Monad m) => a -> (a -> StageTypecheckerPass m b) -> StageTypecheckerPass m b
withClonedTypo x action = do
  typo0 <- typo . _head & use
  withTypo typo0 x action


withClonedTypo0 :: (Monad m) => StageTypecheckerPass m a -> StageTypecheckerPass m a
withClonedTypo0 = withClonedTypo () . const


insertNewMonoTypeVariable :: (Monad m) => ID -> StageTypecheckerPass m Type
insertNewMonoTypeVariable labID = do
    tvarID <- newtvar
    let typeEnvElem = (labID, Mono (TV tvarID))
    typo . _head %= flip insert typeEnvElem
    return $ TV tvarID

insert :: Typo -> (Var, TypeScheme) -> Typo
insert a (x,t) = (x,t):a

getTypeById :: Monad m => ID -> StageTypecheckerPass m Type
getTypeById idV = do
    typeResult <- typeMap . at idV & use
    maybe (report_error "Can't find type using id." . TV =<< newtvar) return typeResult


setTypeById :: (Monad m) => ID -> Type -> StageTypecheckerPass m ()
setTypeById id2 typeV = do
    debugPush $ "save " ++ show id2 ++ " ?= " ++ show typeV
    typeMap . at id2 ?= typeV
    mm <- typeMap & use
    debugPush $ show mm


getTypeSchemeById :: Monad m => ID -> StageTypecheckerPass m TypeScheme
getTypeSchemeById idV = do
    typeScheme <- typeSchemeMap . at idV & use
    maybe (report_error "Can't find type scheme using an id." . Mono . TV =<< newtvar) return typeScheme


setTypeSchemeById :: (Monad m) => ID -> TypeScheme -> StageTypecheckerPass m ()
setTypeSchemeById idV typeScheme =
    typeSchemeMap . at idV ?= typeScheme


debugPush :: (Monad m) => String -> StageTypecheckerPass m ()
debugPush s = s `seq` debugLog %= (s:)



getTargetIDString :: (StageTypecheckerCtx lab m) => lab -> StageTypecheckerPass m String
getTargetIDString lab = do
    labtID <- getTargetID lab
    return $ "|" ++ show labID ++ "⊳" ++ show labtID ++ "⊲"
  where
    labID = Enum.id lab


getTargetID :: (StageTypecheckerCtx lab m) => lab -> StageTypecheckerPass m ID
getTargetID lab =
    sa . SI.alias . ix labID . SI.target & preuse >>= \case
        Nothing     -> return labID
        Just labtID -> return labtID
  where
    labID = Enum.id lab

getEnv :: (Monad m) => StageTypecheckerPass m Typo
getEnv =
    typo & use >>= \case
        []    -> return []
        (x:_) -> return x



add_constraint :: (Monad m) => Constraint -> StageTypecheckerPass m ()
add_constraint c1 =
    constr %= (`mappend` c1)


newtvar :: (Monad m) => StageTypecheckerPass m TVar
newtvar = use nextTVar <* (nextTVar += 1)


rename :: (Monad m) => StageTypecheckerPass m Subst -> TVar -> StageTypecheckerPass m Subst
rename s x = do
    newtv <- newtvar
    s' <- s
    return ((x, TV newtv):s')



