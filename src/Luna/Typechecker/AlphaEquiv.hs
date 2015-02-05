module Luna.Typechecker.AlphaEquiv where


import Flowbox.Prelude
import Luna.Typechecker.Data.Type
import Luna.Typechecker.Data.TVar
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as M


type TypesTranslate  = Map TVar TVar
type FieldsTranslate = Map String String


runEquiv :: (AlphaEquiv a) => a -> a -> Bool
runEquiv a b = (\(x,fmab,fmba,ttab,ttba) -> x) $ equiv M.empty M.empty M.empty M.empty a b


-- TODO [kgdk] 5 lut 2015: najbrzydszy kod EVER. Powinna być monada State, etc. Przepraszam za to :<
class AlphaEquiv a where
    equiv :: FieldsTranslate -> FieldsTranslate -> TypesTranslate -> TypesTranslate -> a -> a -> (Bool, FieldsTranslate, FieldsTranslate, TypesTranslate, TypesTranslate)


instance AlphaEquiv Type where
    equiv fmab fmba ttab ttba (TV a) (TV b)
        | M.lookup a ttab == Just b  && M.lookup b ttba == Just a  = (True, fmab, fmba, ttab, ttba)
        | M.notMember a ttab         && M.notMember b ttba         = (True, fmab, fmba, M.insert a b ttab, M.insert b a ttba)
    equiv fmab fmba ttab ttba (p1 `Fun` p2) (q1 `Fun` q2)
        = let (c1,fmab1,fmba1,ttab1,ttba1) = equiv fmab  fmba ttab  ttba  p1 q1
              (c2,fmab2,fmba2,ttab2,ttba2) = equiv fmab1 fmba1 ttab1 ttba1 p2 q2
           in (c1&&c2, fmab2, fmba2, ttab2, ttba2)
    equiv gfmab gfmba gttab gttba (Record flds1) (Record flds2) | length flds1 == length flds2
        = foldr merge (True, gfmab, gfmba, gttab, gttba) (zip flds1 flds2)
      where
        merge _                                      (False, fmab, fmba, ttab, ttba) = (False, fmab, fmba, ttab, ttba)
        merge ((fld1lab, fld1ty), (fld2lab, fld2ty)) (True,  fmab, fmba, ttab, ttba) | M.lookup fld1lab fmab == Just fld2lab && M.lookup fld2lab fmba == Just fld1lab 
                                                                                        = equiv fmab fmba ttab ttba fld1ty fld2ty
                                                                                     | M.notMember fld1lab fmab              && M.notMember fld2lab fmba
                                                                                        = let fmab1 = M.insert fld1lab fld2lab fmab
                                                                                              fmba1 = M.insert fld1lab fld1lab fmab1
                                                                                           in equiv fmab1 fmba1 ttab ttba fld1ty fld2ty
        merge ((fld1lab, fld1ty), (fld2lab, fld2ty)) (True,  fmab, fmba, ttab, ttba) = (False, fmab, fmba, ttab, ttba)
    equiv fmab fmba ttab ttba _ _ = (False, fmab, fmba, ttab, ttba)
