{-# LANGUAGE TupleSections #-}

module Luna.Typechecker.Data.Type where

import Flowbox.Prelude
import Control.Monad hiding (mapM)
import Data.Function (on)
import Data.List
import Data.Ord

import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.TVar

import Data.Map.IntConvertibleSet (IntConvertibleSet)
import qualified Data.Map.IntConvertibleSet as S



type Fieldlabel = String
type Field = (Fieldlabel, Type)


data Type = TV TVar
          | Type `Fun` Type
          | Record [Field]
          deriving (Show,Eq,Ord)


instance AlphaEquiv Type where
    equiv (TV a) (TV b) = equiv a b
    equiv (p1 `Fun` p2) (q1 `Fun` q2) = equiv p1 q1 >> equiv p2 q2
    equiv (Record flds1) (Record flds2)
      | length flds1 == length flds2
        = zipWithM_ merge (sorted flds1) (sorted flds2)
      where
        sorted = sortBy (comparing fst)
        merge (fld1lab, fld1ty) (fld2lab, fld2ty) | fld1lab == fld2lab  = equiv fld1ty fld2ty
                                                  | otherwise           = notAlphaEquivalent
    equiv _ _ = notAlphaEquivalent

    translateBtoA (TV b)       = TV     <$> translateBtoA b
    translateBtoA (p `Fun` q)  = Fun    <$> translateBtoA p <*> translateBtoA q
    translateBtoA (Record fls) = Record <$> mapM aux fls
      where aux (lab,ty) = translateBtoA ty >>= return . (lab,)

    freevars (TV a) = freevars a
    freevars (p `Fun` q) = freevars p <> freevars q
    freevars (Record flds) = mconcat (freevars.snd <$> flds)