{-# LANGUAGE KindSignatures #-}

module Luna.Typechecker.AlphaEquiv.AlphaEquivMonad (
      AlphaEquivMonad(..),
      notAlphaEquivalent, cantAlphaTranslate,
      ttInsert, ttDelete,
      ttEquiv,
      ttTranslate, ttBtoA,
      fork, ttWithInserted
  ) where


import            Flowbox.Prelude
import            Control.Monad               (ap)
import            Data.Map.Strict             (Map)
import qualified  Data.Map.Strict             as M
import Data.List                        (sort,nub,sortBy,intercalate)
import Data.Ord                         (comparing)
import Data.Tuple                       (swap)
import Data.Maybe

import            Luna.Typechecker.Data.TVar



type TypesTranslate  = Map TVar TVar

newtype AlphaEquivMonad a
  = AlphaEquivMonad { runAlphaEq  :: TypesTranslate
                                  -> TypesTranslate
                                  -> [ (a, TypesTranslate, TypesTranslate) ] }


instance Monad AlphaEquivMonad where
    fail _ = AlphaEquivMonad aux
      where aux ttab ttba = []

    return x = AlphaEquivMonad aux
      where aux ttab ttba = [(x,ttab,ttba)]

    m >>= ak = AlphaEquivMonad aux
      where aux ttab ttba = go `concatMap` runAlphaEq m ttab ttba
            go (x, ttab, ttba) = runAlphaEq (ak x) ttab ttba

instance Functor AlphaEquivMonad where
    fmap f m = AlphaEquivMonad aux
      where aux ttab ttba = go <$> runAlphaEq m ttab ttba
            go (x, ttab, ttba) = (f x, ttab, ttba)
instance Applicative AlphaEquivMonad where
    pure = return
    (<*>) = ap


notAlphaEquivalent :: AlphaEquivMonad a
notAlphaEquivalent = fail "not ⍺-equivalent"

cantAlphaTranslate :: AlphaEquivMonad a
cantAlphaTranslate = fail "can't ⍺-translate (B to A)"


-- this shall *never* happen, as they shall be equivalent
-- by `fromList . map swap . toList`.
-- In other words, we maintain a bijection. This indicates
-- that it's not. And it's bad.
errorInvalidAlphaInvariant :: AlphaEquivMonad a
errorInvalidAlphaInvariant = fail "internal ⍺-equivalent invariant is dissatisfied"


bittMap :: (TypesTranslate -> TypesTranslate) -> (TypesTranslate -> TypesTranslate) -> AlphaEquivMonad ()
bittMap fl fr = AlphaEquivMonad aux
  where aux ttab ttba = [((), fl ttab, fr ttba)]


-- Type operations

ttInsert :: TVar -> TVar -> AlphaEquivMonad ()
ttInsert a b = bittMap (M.insert a b) (M.insert b a)

ttDelete :: TVar -> TVar -> AlphaEquivMonad ()
ttDelete a b = bittMap (M.delete a) (M.delete b)


ttLookup = ttEquiv

ttEquiv :: TVar -> TVar -> AlphaEquivMonad Bool
ttEquiv a b = AlphaEquivMonad aux
  where aux ttab ttba
          | M.lookup a ttab == Just b  && M.lookup b ttba == Just a = [(True,  ttab,ttba)]
          | M.notMember a ttab         && M.notMember b ttba        = [(False, ttab,ttba)]
          | otherwise                                               = []


ttTranslate :: TVar -> TVar -> AlphaEquivMonad (Maybe TVar, Maybe TVar)
ttTranslate a b = AlphaEquivMonad aux
  where aux ttab ttba = [((M.lookup a ttab, M.lookup b ttba), ttab, ttba)]

ttBtoA :: TVar -> AlphaEquivMonad TVar
ttBtoA a = maybe cantAlphaTranslate return =<< (fst <$> ttTranslate a undefined)


fork :: [AlphaEquivMonad ()] -> AlphaEquivMonad ()
fork fs = AlphaEquivMonad aux
  where aux ttab ttba = concat [runAlphaEq f ttab ttba | f <- fs]

aroundAction :: (Monad m) => m b -> (b -> m ()) -> m a -> m a
aroundAction preAct postAct act = do
    tmp <- preAct
    res <- act
    postAct tmp
    return res

ttWithInserted :: [TVar] -> [TVar] -> AlphaEquivMonad a -> AlphaEquivMonad a
ttWithInserted lsta lstb = aroundAction poll pushOld
  where
    -- store old values (if they existed)
    poll = AlphaEquivMonad $ \ttab ttba ->
            let savettAB = map (\k -> (k, M.lookup k ttab)) lsta
                savettBA = map (\k -> (k, M.lookup k ttba)) lstb
             in [((savettAB, savettBA), ttab, ttba)]
    -- restore old values. If the key didn't exist, remove the entry.
    pushOld = mapM_ (uncurry updateOrDelete) . uncurry zip
    
    updateOrDelete (tvA, Nothing)   (tvB, Nothing)                                = ttDelete tvA tvB
    updateOrDelete (tvA, Just tvAB) (tvB, Just tvBA) | tvAB == tvB && tvBA == tvA = ttInsert tvA tvB
    updateOrDelete _ _                                                            = errorInvalidAlphaInvariant
