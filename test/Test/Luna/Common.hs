{-# LANGUAGE RankNTypes #-}

module Test.Luna.Common where


import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Lit       (Lit(..))
import Luna.Typechecker.AST.Pat       (Pat(..))
import Luna.Typechecker.AST.Scheme    (Scheme(..),toScheme)
import Luna.Typechecker.AST.TID       (TID)
import Luna.Typechecker.AST.Type      (Type(..),Tyvar(..),fn,tInteger,list,tBool,pair,tChar,tDouble,tFloat,tInt,tUnit,tList,tArrow,tTuple2)

import Luna.Typechecker.Assumptions   (Assump(..))
import Luna.Typechecker.BindingGroups (Alt,Expr(..),Impl)
import Luna.Typechecker.Typeclasses   (Qual(..),Pred(..),EnvTransformer,(<:>),addClass,addInst)


import Control.Applicative ((<$>),Const(..))



-- ------------------------------------------------------------
-- ██╗   ██╗████████╗██╗██╗     ███████╗
-- ██║   ██║╚══██╔══╝██║██║     ██╔════╝
-- ██║   ██║   ██║   ██║██║     ███████╗
-- ██║   ██║   ██║   ██║██║     ╚════██║
-- ╚██████╔╝   ██║   ██║███████╗███████║
--  ╚═════╝    ╚═╝   ╚═╝╚══════╝╚══════╝
-- ------------------------------------------------------------

ap :: [Expr] -> Expr
ap = foldl1 Ap

type TypeDecl = (TID, Scheme, [Alt])

-- we need no dep on lens, that's good
type Lens s t a b = Functor f => (a -> f b) -> s -> f t

(^.) :: s -> Lens s t a b -> a
infixl 8 ^.
s ^. l = getConst (l Const s)

tid :: Lens TypeDecl TypeDecl TID TID
tid f (t, s, as) = (\t' -> (t', s, as)) <$> f t

scheme :: Lens TypeDecl TypeDecl Scheme Scheme
scheme f (t, s, as) = (\s' -> (t, s', as)) <$> f s

alts :: Lens TypeDecl TypeDecl [Alt] [Alt]
alts f (t, s, as) = (\as' -> (t, s, as')) <$> f as

--impl :: Lens TypeDecl Impl TypeDecl Impl
impl :: Lens TypeDecl (a, Scheme, c) Impl (a,c)
impl f (t, s, as) = (\(t',as') -> (t',s,as')) <$> f (t, as)

asmp :: Lens (TID, Scheme, [Alt]) (TID, Scheme, [Alt]) Assump Assump
asmp f (t, s, as) = (\(t' :>: s') -> (t', s', as)) <$> f (t :>: s)


-- ------------------------------------------------------------
-- ████████╗██╗   ██╗██████╗ ███████╗ ██████╗██╗      █████╗ ███████╗███████╗███████╗███████╗
-- ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝██║     ██╔══██╗██╔════╝██╔════╝██╔════╝██╔════╝
--    ██║    ╚████╔╝ ██████╔╝█████╗  ██║     ██║     ███████║███████╗███████╗█████╗  ███████╗
--    ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ██║     ██║     ██╔══██║╚════██║╚════██║██╔══╝  ╚════██║
--    ██║      ██║   ██║     ███████╗╚██████╗███████╗██║  ██║███████║███████║███████╗███████║
--    ╚═╝      ╚═╝   ╚═╝     ╚══════╝ ╚═════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝
-- ------------------------------------------------------------

standardET :: EnvTransformer
standardET =  addClass "Eq" []
              <:> addInst [] (IsIn "Eq" tUnit)
              <:> addInst [] (IsIn "Eq" tBool)
              <:> addInst [] (IsIn "Eq" tChar)
              <:> addInst [] (IsIn "Eq" tInt)
              <:> addInst [] (IsIn "Eq" tInteger)
              <:> addInst [] (IsIn "Eq" tFloat)
              <:> addInst [] (IsIn "Eq" tDouble)
              <:> addInst [IsIn "Eq" tvarA] (IsIn "Eq" (list tvarA))
              <:> addInst [IsIn "Eq" tvarA, IsIn "Eq" tvarB] (IsIn "Eq" (tvarA `pair` tvarB))
          <:> addClass "Ord" ["Eq"]
              <:> addInst [] (IsIn "Ord" tUnit)
              <:> addInst [] (IsIn "Ord" tBool)
              <:> addInst [] (IsIn "Ord" tChar)
              <:> addInst [] (IsIn "Ord" tInt)
              <:> addInst [] (IsIn "Ord" tInteger)
              <:> addInst [] (IsIn "Ord" tFloat)
              <:> addInst [] (IsIn "Ord" tDouble)
              <:> addInst [IsIn "Ord" tvarA] (IsIn "Ord" (list tvarA))
              <:> addInst [IsIn "Ord" tvarA, IsIn "Ord" tvarB] (IsIn "Ord" (tvarA `pair` tvarB))
          <:> addClass "Enum" []
              <:> addInst [] (IsIn "Enum" tUnit)
              <:> addInst [] (IsIn "Enum" tBool)
              <:> addInst [] (IsIn "Enum" tChar)
              <:> addInst [] (IsIn "Enum" tInt)
              <:> addInst [] (IsIn "Enum" tInteger)
              <:> addInst [] (IsIn "Enum" tFloat)
              <:> addInst [] (IsIn "Enum" tDouble)
          <:> addClass "Num" []
              <:> addInst [] (IsIn "Num" tInt)
              <:> addInst [] (IsIn "Num" tInteger)
              <:> addInst [] (IsIn "Num" tFloat)
              <:> addInst [] (IsIn "Num" tDouble)
          <:> addClass "Real" ["Num", "Ord"]
              <:> addInst [] (IsIn "Real" tInt)
              <:> addInst [] (IsIn "Real" tInteger)
              <:> addInst [] (IsIn "Real" tFloat)
              <:> addInst [] (IsIn "Real" tDouble)
          <:> addClass "Integral" ["Real", "Enum"]
              <:> addInst [] (IsIn "Integral" tInt)
              <:> addInst [] (IsIn "Integral" tInteger)
              <:> addInst [] (IsIn "Integral" tFloat)
              <:> addInst [] (IsIn "Integral" tDouble)
          <:> addClass "Fractional" ["Num"]
              <:> addInst [] (IsIn "Fractional" tFloat)
              <:> addInst [] (IsIn "Fractional" tDouble)
          <:> addClass "Floating" ["Fractional"]
              <:> addInst [] (IsIn "Floating" tFloat)
              <:> addInst [] (IsIn "Floating" tDouble)
          <:> addClass "Functor" []
              <:> addInst [] (IsIn "Functor" tList)
              <:> addInst [] (IsIn "Functor" (TAp tArrow tvarA))
              <:> addInst [] (IsIn "Functor" (TAp tTuple2 tvarA))
  where tvarA = TVar $ Tyvar "a" Star
        tvarB = TVar $ Tyvar "b" Star


-- ------------------------------------------------------------
-- ██████╗ ██████╗ ███████╗██╗     ██╗   ██╗██████╗ ███████╗
-- ██╔══██╗██╔══██╗██╔════╝██║     ██║   ██║██╔══██╗██╔════╝
-- ██████╔╝██████╔╝█████╗  ██║     ██║   ██║██║  ██║█████╗
-- ██╔═══╝ ██╔══██╗██╔══╝  ██║     ██║   ██║██║  ██║██╔══╝
-- ██║     ██║  ██║███████╗███████╗╚██████╔╝██████╔╝███████╗
-- ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚══════╝
-- ------------------------------------------------------------


andBG, consBG, constBG, eqBG, fromIntegralBG, foldrBG, gcdBG, integralAddBG, landBG, leqBG, lorBG, modBG, nilBG, foldlBG, fractionalDivBG, iterateBG, negateBG, sumBG, takeBG, zipWithBG :: TypeDecl

andBG =
  ( "and"
  , Forall [] ([] :=> (list tBool `fn` tBool))
  , [ ( []
      , ap [Var "foldr", Var "(&&)", EConst ("True" :>: toScheme tBool)]
      )
    ]
  )

consBG =
  ( "(:)"
  , Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
  , [ ( [ error "no pat for cons" ] 
      , error "no body for cons"
      )
    ]
  )


constBG =
  ( "const"
  , Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 0))
  , [ ( [ PVar "x", PVar "y" ] 
      , Var "x"
      )
    ]
  )


eqBG =
  ( "(==)"
  , Forall [Star] ([IsIn "Eq" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
  , [ ( [ error "no pat for eq" ]
      , error "no body for eq"
      )
    ]
  )


fromIntegralBG =
  ( "fromIntegral"
  , Forall [Star, Star] ([IsIn "Integral" (TGen 0), IsIn "Num" (TGen 1)] :=> (TGen 0 `fn` TGen 1))
  , [ ( [ error "no pat for fromIntegral" ]
      , error "no body for fromIntegral"
      )
    ]
  )


foldlBG =
  ( "foldl"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 1) `fn` TGen 0))
  , [ (  [PWildcard, PVar "a", PCon (nilBG^.asmp) []]
      ,  Var "a"
     ),( [PVar "f", PVar "a", PAs "xxs" (PCon (consBG ^. asmp) [PVar "x", PVar "xs"] )]
      ,  ap [Var "foldl", Var "f", ap [Var "f", Var "a", Var "x"], Var "xs"]
      )
    ]
  )


foldrBG =
  ( "foldr"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
  , [ (  [PWildcard, PVar "a", PCon (nilBG^.asmp) []]
      ,  Var "a"
     ),( [PVar "f", PVar "a", PAs "xxs" (PCon (consBG ^. asmp) [PVar "x", PVar "xs"] )]
      ,  ap [Var "f", Var "x", ap [Var "foldr", Var "f", Var "a", Var "xs"]]
      )
    ]
  )


fractionalDivBG =
  ( "(/)"
  , Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
  , [ (  [ error "no pat for fractionalDiv" ]
      ,  error "no body for fractionalDiv"
      )
    ]
  )


gcdBG =
  ( "gcd"
  , toScheme (tInteger `fn` tInteger `fn` tInteger)
  , [ (  [PVar "a", PLit (LitInt 0)]
      ,  Var "a"
     ),( [PVar "a", PVar "b"]
      ,  ap [Var "gcd", Var "b", ap [Var "mod", Var "a", Var "b"]]
      )
    ]
  )


integralAddBG =
  ( "(+)"
  , Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
  , [ (  [ error "no pat for integralAdd" ]
      ,  error "no body for integralAdd"
      )
    ]
  )


iterateBG =
  ( "iterate"
  , Forall [Star] ([] :=> ((TGen 0 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 0)))
  , [ (  [ error "no pat for iterate" ]
      ,  error "no body for iterate"
      )
    ]
  )


landBG =
  ( "(&&)"
  , toScheme (tBool `fn` tBool `fn` tBool)
  , [ (  [ error "no pat for (&&)" ]
      ,  error "no body for (&&)"
      )
    ]
  )


leqBG =
  ( "(<=)"
  , Forall [Star] ([IsIn "Ord" (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
  , [ (  [ error "no pat for (<=)" ]
      ,  error "no body for (<=)"
      )
    ]
  )


lorBG =
  ( "(||)"
  , toScheme (tBool `fn` tBool `fn` tBool)
  , [ (  [ error "no pat for (||)" ]
      ,  error "no body for (||)"
      )
    ]
  )


modBG =
  ( "mod"
  , toScheme (tInteger `fn` tInteger `fn` tInteger)
  , [ (  [ error "no pat for mod" ]
      ,  error "no body for mod"
      )
    ]
  )


negateBG =
  ( "negate"
  , Forall [Star] ([IsIn "Num" (TGen 0)] :=> (TGen 0 `fn` TGen 0))
  , [ (  [ error "no pat for negate" ]
      ,  error "no body for negate"
      )
    ]
  )


nilBG =
  ( "[]"
  , Forall [Star] ([] :=> list (TGen 0))
  , [ (  [ error "no pat for []" ]
      ,  error "no body for []"
      )
    ]
  )


sumBG =
  ( "sum"
  , Forall [Star] ([IsIn "Num" (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
  , [ (  [ error "no pat for sum" ]
      ,  error "no body for sum"
      )
    ]
  )

takeBG =
  ( "take"
  , Forall [Star] ([] :=> (tInt `fn` list (TGen 0) `fn` list (TGen 0)))
  , [ (  [ error "no pat for take" ]
      ,  error "no body for take"
      )
    ]
  )


zipWithBG =
  ( "zipWith"
  , Forall [Star,Star,Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` list (TGen 0) `fn` list (TGen 1) `fn` list (TGen 2)))
  , [ (  [ error "no pat for zipWith" ]
      ,  error "no body for zipWith"
      )
    ]
  )




