module Luna.Typechecker.Debug.PrettyData (
    prettyComma, prettyNullable,
    prettyTVar, prettyVar,
    prettyFieldlabel, prettyField,
    prettySubst, prettyTypo,
    prettyType, prettyPred, prettyConstr, prettyTypeScheme, prettyTypeMap
  ) where


import qualified  Data.Map.Strict         as SM
import            Text.PrettyPrint        (Doc, ($+$),(<+>), (<>), braces, brackets, char, empty, hsep, int, parens, punctuate, text)

import            Luna.Typechecker.Data   (TVar, Var, Fieldlabel, Field, Subst, Typo, Type(..), Predicate(..), Constraint(..), TypeScheme(..), TypeMap)



prettyComma :: [Doc] -> Doc
prettyComma = hsep . punctuate (char ',')

prettyNullable :: [Doc] -> Doc
prettyNullable [] = char '∅'
prettyNullable xs = foldl ($+$) empty xs


prettyTVar :: TVar -> Doc
prettyTVar tv = text "τ_" <> int tv

prettyVar :: Var -> Doc
prettyVar = int

prettyFieldlabel :: Fieldlabel -> Doc
prettyFieldlabel = int

prettyField :: Field -> Doc
prettyField (fl, ty) = prettyFieldlabel fl <> char ':' <> prettyType ty

prettyType :: Type -> Doc
prettyType (TV tv)       = prettyTVar tv
prettyType (t1 `Fun` t2) = parens $ prettyType t1
                                    <+> char '→'
                                    <+> prettyType t2
prettyType (Record fs)  = braces
                        $ hsep
                        $ punctuate (char ',')
                        $ map prettyField fs

prettyPred :: Predicate -> Doc
prettyPred TRUE                 = char '⊤'
prettyPred (ty1 `Subsume` ty2)  = prettyType ty1        <> char '≼' <> prettyType ty2
prettyPred (Reckind rty fl fty) = prettyField (fl, fty) <> char 'ϵ' <> prettyType rty

prettyConstr :: Constraint -> Doc
prettyConstr (C ps)         = hsep
                            $ punctuate (char ',')
                            $ map prettyPred ps
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
prettySubst s | null substs = prettyNullable []
              | otherwise   = prettyComma substs
  where prettySubst1 (tv, ty) = parens
                              $ prettyTVar tv
                              <+> char '↣'
                              <+> prettyType ty
        substs                = map prettySubst1 s

prettyTypo :: Typo -> Doc
prettyTypo = prettyNullable . map prettyTypo1
  where prettyTypo1 (v,ts)  = prettyVar v
                            <+> text " :: "
                            <+> prettyTypeScheme ts

prettyTypeMap :: TypeMap -> Doc
prettyTypeMap = SM.foldrWithKey join empty
  where join key ty rest = int key <+> text "=>" <+> prettyType ty <+> rest

