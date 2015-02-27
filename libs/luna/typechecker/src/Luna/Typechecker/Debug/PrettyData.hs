module Luna.Typechecker.Debug.PrettyData (
    prettyComma, prettyNullable,
    prettyTVar, prettyID, prettySubst, prettyTypo,
    prettyType, prettyPred, prettyConstr, prettyTypeScheme, prettyTypeMap,
    toSubscript, toSuperscript
  ) where


import            Flowbox.Prelude             hiding ((<>), empty)

import qualified  Data.IntMap.Strict          as SM
import qualified  Data.Foldable               as Fold
import            Text.PrettyPrint            (
                      Doc, ($+$),(<+>), (<>),
                      brackets, braces, char, empty, hsep, int, parens, punctuate, text
                  )

import            Luna.Syntax.Enum            (ID)

import            Luna.Typechecker.Data       (
                      TVar(..), Subst(..),
                      Typo(..), Type(..), Predicate(..), Constraint(..), TypeScheme(..),
                      TypeMap
                  )
import            Luna.Typechecker.Data.Type  (Field)


toSubscript :: Show a => a -> String
toSubscript = map aux . show
  where aux '0' = '₀'
        aux '1' = '₁'
        aux '2' = '₂'
        aux '3' = '₃'
        aux '4' = '₄'
        aux '5' = '₅'
        aux '6' = '₆'
        aux '7' = '₇'
        aux '8' = '₈'
        aux '9' = '₉' 
        aux  x  =  x

toSuperscript :: Show a => a -> String
toSuperscript = map aux . show
  where aux '0' = '⁰'
        aux '1' = '¹'
        aux '2' = '²'
        aux '3' = '³'
        aux '4' = '⁴'
        aux '5' = '⁵'
        aux '6' = '⁶'
        aux '7' = '⁷'
        aux '8' = '⁸'
        aux '9' = '⁹' 
        aux  x  =  x

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
prettyType (TV tv)          = prettyTVar tv
prettyType (t1 `Fun` t2)    = parens $ prettyType t1
                                       <+> char '→'
                                       <+> prettyType t2
prettyType (Record fields)  = (braces . prettyNullableComma . map prettyField) fields

prettyField :: Field -> Doc
prettyField (label, ty) = text label <> char '∷' <> prettyType ty

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
                                  <+> ((brackets . prettyComma . map prettyTVar) tvs <> char '.')
                                  <+> prettyConstr cs
                                  <+> char '⇒'
                                  <+> prettyType ty

prettySubst :: Subst -> Doc
prettySubst = prettyNullableComma . substs . fromSubst
  where prettySubst1 (tv, ty) = parens $  prettyTVar tv <+> char '↣'
                                      $+$ prettyType ty
        substs = map prettySubst1 . Fold.toList

prettyTypo :: Typo -> Doc
prettyTypo = prettyNullable . map prettyTypo1 . fromTypo
  where prettyTypo1 (v,ts)  = prettyID v
                            <+> text "∷"
                            <+> prettyTypeScheme ts

prettyTypeMap :: TypeMap -> Doc
prettyTypeMap tm = prettyNullableComma $ SM.elems $ SM.mapWithKey join tm
  where join key ty = prettyID key <+> text "⇒" <+> prettyType ty

