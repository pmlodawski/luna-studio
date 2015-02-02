module Luna.Typechecker.Debug.PrettyData (
    prettyComma, prettyNullable,
    prettyTVar, prettyID, prettySubst, prettyTypo,
    prettyType, prettyPred, prettyConstr, prettyTypeScheme, prettyTypeMap
  ) where


import qualified  Data.IntMap.Strict      as SM
import qualified  Data.Map as Map
import            Text.PrettyPrint        (
                      Doc, ($+$),(<+>), (<>),
                      brackets, char, empty, hsep, int, parens, punctuate, text
                  )

import            Luna.Syntax.Enum        (ID)

import            Luna.Typechecker.Data (
                      TVar(..), Subst(..),
                      Typo(..), Type(..), Predicate(..), Constraint(..), TypeScheme(..),
                      TypeMap
                  )



vcatNoOverlap :: [Doc] -> Doc
vcatNoOverlap = foldl ($+$) empty

prettyComma :: [Doc] -> Doc
prettyComma = vcatNoOverlap . punctuate (char ',')

prettyNullable :: [Doc] -> Doc
prettyNullable [] = char '∅'
prettyNullable xs = vcatNoOverlap xs

prettyNullableComma :: [Doc] -> Doc
prettyNullableComma [] = char '∅'
prettyNullableComma xs = prettyComma xs


prettyTVar :: TVar -> Doc
prettyTVar tv = text "τ_" <> int (fromTVar tv)

prettyType :: Type -> Doc
prettyType (TV tv)       = prettyTVar tv
prettyType (t1 `Fun` t2) = parens $ prettyType t1
                                    <+> char '→'
                                    <+> prettyType t2

prettyID :: ID -> Doc
prettyID = int

prettyPred :: Predicate -> Doc
prettyPred TRUE                 = char '⊤'
prettyPred (ty1 `Subsume` ty2)  = prettyType  ty1       <> char '≼' <> prettyType ty2

prettyConstr :: Constraint -> Doc
prettyConstr (C ps)         = hsep $ punctuate (char ',') $ map prettyPred ps
prettyConstr (Proj tvs ps)  =   char '∃'
                            <+> (prettyComma (map prettyTVar tvs) <> char '.')
                            <+> prettyComma (map prettyPred ps)

prettyTypeScheme :: TypeScheme -> Doc
prettyTypeScheme (Mono ty)        = prettyType ty
prettyTypeScheme (Poly tvs cs ty) = char '∀'
                                  <+> (brackets (prettyComma (map prettyTVar tvs)) <> char '.')
                                  <+> prettyConstr cs
                                  <+> char '⇒'
                                  <+> prettyType ty

prettySubst :: Subst -> Doc
prettySubst = prettyNullableComma . substs . fromSubst
  where prettySubst1 (tv, ty) = parens $  prettyTVar tv <+> char '↣'
                                      $+$ prettyType ty
        substs = map prettySubst1 . Map.toList

prettyTypo :: Typo -> Doc
prettyTypo = prettyNullable . map prettyTypo1 . fromTypo
  where prettyTypo1 (v,ts)  = prettyID v
                            <+> text "::"
                            <+> prettyTypeScheme ts

prettyTypeMap :: TypeMap -> Doc
prettyTypeMap tm = prettyNullableComma $ SM.elems $ SM.mapWithKey join tm
  where join key ty = prettyID key <+> text "⇒" <+> prettyType ty

