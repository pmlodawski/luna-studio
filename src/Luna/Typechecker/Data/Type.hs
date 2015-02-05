module Luna.Typechecker.Data.Type where

import Flowbox.Prelude
import Control.Monad

import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.TVar



type Fieldlabel = String
type Field = (Fieldlabel, Type)


data Type = TV TVar
          | Type `Fun` Type
          | Record [Field]
          deriving (Show,Eq)


instance AlphaEquiv Type where
    equiv (TV a) (TV b) = ttLookup a b >>= \case
                              True  -> return ()
                              False -> ttInsert a b
    equiv (p1 `Fun` p2) (q1 `Fun` q2) = equiv p1 q1 >> equiv p2 q2
    equiv (Record flds1) (Record flds2)
      | length flds1 == length flds2 = zipWithM_ merge flds1 flds2
      where 
        merge (fld1lab, fld1ty) (fld2lab, fld2ty) = ffLookup fld1lab fld2lab >>= \case
                                                        True  -> return ()
                                                        False -> ffInsert fld1lab fld2lab >> equiv fld1ty fld2ty
    equiv _ _ = fail "no"