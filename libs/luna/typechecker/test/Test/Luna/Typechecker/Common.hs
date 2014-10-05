{-# LANGUAGE RankNTypes #-}

module Test.Luna.Typechecker.Common where


import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Lit       (Lit(..))
import Luna.Typechecker.AST.Pat       (Pat(..))
import Luna.Typechecker.AST.Scheme    (Scheme(..),toScheme)
import Luna.Typechecker.AST.TID       (TID(..))
import Luna.Typechecker.AST.Type      (Type(..),Tyvar(..),fn,tInteger,list,tBool,pair,tChar,tDouble,tFloat,tInt,tUnit,tList,tArrow,tTuple2)

import Luna.Typechecker.Assumptions   (Assump(..))
import Luna.Typechecker.BindingGroups (Alt,Expr(..),Impl,Expl)
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

type TypeDecl = (String, Scheme, [Alt])

-- we need no dep on lens, that's good
type Lens s t a b = Functor f => (a -> f b) -> s -> f t

(^.) :: s -> Lens s t a b -> a
infixl 8 ^.
s ^. l = getConst (l Const s)

tid :: Lens TypeDecl TypeDecl TID TID
tid f (t, s, as) = (\(TID t') -> (t', s, as)) <$> f (TID t)

scheme :: Lens TypeDecl TypeDecl Scheme Scheme
scheme f (t, s, as) = (\s' -> (t, s', as)) <$> f s

alts :: Lens TypeDecl TypeDecl [Alt] [Alt]
alts f (t, s, as) = (\as' -> (t, s, as')) <$> f as

--expl :: Lens TypeDecl TypeDecl 
expl :: Lens TypeDecl TypeDecl Expl Expl
expl f (t, s, as) = (\(TID ts',s',as') -> (ts',s',as')) <$> f (TID t, s, as)

--impl :: Lens TypeDecl Impl TypeDecl Impl
impl :: Lens TypeDecl (String, Scheme, c) Impl (TID,c)
impl f (t, s, as) = (\(TID t',as') -> (t',s,as')) <$> f (TID t, as)

asmp :: Lens (String, Scheme, [Alt]) (String, Scheme, [Alt]) Assump Assump
asmp f (t, s, as) = (\(TID t' :>: s') -> (t', s', as)) <$> f (TID t :>: s)


-- ------------------------------------------------------------
-- ████████╗██╗   ██╗██████╗ ███████╗ ██████╗██╗      █████╗ ███████╗███████╗███████╗███████╗
-- ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝██║     ██╔══██╗██╔════╝██╔════╝██╔════╝██╔════╝
--    ██║    ╚████╔╝ ██████╔╝█████╗  ██║     ██║     ███████║███████╗███████╗█████╗  ███████╗
--    ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ██║     ██║     ██╔══██║╚════██║╚════██║██╔══╝  ╚════██║
--    ██║      ██║   ██║     ███████╗╚██████╗███████╗██║  ██║███████║███████║███████╗███████║
--    ╚═╝      ╚═╝   ╚═╝     ╚══════╝ ╚═════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝
-- ------------------------------------------------------------

standardET :: (Monad m) => EnvTransformer m
standardET =  addClass (TID "Eq") (TID <$> [])
              <:> addInst [] (IsIn (TID "Eq") tUnit)
              <:> addInst [] (IsIn (TID "Eq") tBool)
              <:> addInst [] (IsIn (TID "Eq") tChar)
              <:> addInst [] (IsIn (TID "Eq") tInt)
              <:> addInst [] (IsIn (TID "Eq") tInteger)
              <:> addInst [] (IsIn (TID "Eq") tFloat)
              <:> addInst [] (IsIn (TID "Eq") tDouble)
              <:> addInst [IsIn (TID "Eq") tvarA] (IsIn (TID "Eq") (list tvarA))
              <:> addInst [IsIn (TID "Eq") tvarA, IsIn (TID "Eq") tvarB] (IsIn (TID "Eq") (tvarA `pair` tvarB))
          <:> addClass (TID "Ord") (TID <$> ["Eq"])
              <:> addInst [] (IsIn (TID "Ord") tUnit)
              <:> addInst [] (IsIn (TID "Ord") tBool)
              <:> addInst [] (IsIn (TID "Ord") tChar)
              <:> addInst [] (IsIn (TID "Ord") tInt)
              <:> addInst [] (IsIn (TID "Ord") tInteger)
              <:> addInst [] (IsIn (TID "Ord") tFloat)
              <:> addInst [] (IsIn (TID "Ord") tDouble)
              <:> addInst [IsIn (TID "Ord") tvarA] (IsIn (TID "Ord") (list tvarA))
              <:> addInst [IsIn (TID "Ord") tvarA, IsIn (TID "Ord") tvarB] (IsIn (TID "Ord") (tvarA `pair` tvarB))
          <:> addClass (TID "Enum") (TID <$> [])
              <:> addInst [] (IsIn (TID "Enum") tUnit)
              <:> addInst [] (IsIn (TID "Enum") tBool)
              <:> addInst [] (IsIn (TID "Enum") tChar)
              <:> addInst [] (IsIn (TID "Enum") tInt)
              <:> addInst [] (IsIn (TID "Enum") tInteger)
              <:> addInst [] (IsIn (TID "Enum") tFloat)
              <:> addInst [] (IsIn (TID "Enum") tDouble)
          <:> addClass (TID "Num") (TID <$> [])
              <:> addInst [] (IsIn (TID "Num") tInt)
              <:> addInst [] (IsIn (TID "Num") tInteger)
              <:> addInst [] (IsIn (TID "Num") tFloat)
              <:> addInst [] (IsIn (TID "Num") tDouble)
          <:> addClass (TID "Real") (TID <$> ["Num", "Ord"])
              <:> addInst [] (IsIn (TID "Real") tInt)
              <:> addInst [] (IsIn (TID "Real") tInteger)
              <:> addInst [] (IsIn (TID "Real") tFloat)
              <:> addInst [] (IsIn (TID "Real") tDouble)
          <:> addClass (TID "Integral") (TID <$> ["Real", "Enum"])
              <:> addInst [] (IsIn (TID "Integral") tInt)
              <:> addInst [] (IsIn (TID "Integral") tInteger)
              <:> addInst [] (IsIn (TID "Integral") tFloat)
              <:> addInst [] (IsIn (TID "Integral") tDouble)
          <:> addClass (TID "Fractional") (TID <$> ["Num"])
              <:> addInst [] (IsIn (TID "Fractional") tFloat)
              <:> addInst [] (IsIn (TID "Fractional") tDouble)
          <:> addClass (TID "Floating") (TID <$> ["Fractional"])
              <:> addInst [] (IsIn (TID "Floating") tFloat)
              <:> addInst [] (IsIn (TID "Floating") tDouble)
          <:> addClass (TID "Functor") (TID <$> [])
              <:> addInst [] (IsIn (TID "Functor") tList)
              <:> addInst [] (IsIn (TID "Functor") (TAp tArrow tvarA))
              <:> addInst [] (IsIn (TID "Functor") (TAp tTuple2 tvarA))
  where tvarA = TVar $ Tyvar (TID "a") Star
        tvarB = TVar $ Tyvar (TID "b") Star


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
      , ap [Var (TID "foldr"), Var (TID "(&&)"), EConst (TID "True" :>: toScheme tBool)]
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
  , [ ( [ PVar (TID "x"), PVar (TID "y") ]
      , Var (TID "x")
      )
    ]
  )


eqBG =
  ( "(==)"
  , Forall [Star] ([IsIn (TID "Eq") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
  , [ ( [ error "no pat for eq" ]
      , error "no body for eq"
      )
    ]
  )


fromIntegralBG =
  ( "fromIntegral"
  , Forall [Star, Star] ([IsIn (TID "Integral") (TGen 0), IsIn (TID "Num") (TGen 1)] :=> (TGen 0 `fn` TGen 1))
  , [ ( [ error "no pat for fromIntegral" ]
      , error "no body for fromIntegral"
      )
    ]
  )


foldlBG =
  ( "foldl"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 1) `fn` TGen 0))
  , [ (  [PWildcard, PVar (TID "a"), PCon (nilBG^.asmp) []]
      ,  Var (TID "a")
     ),( [PVar (TID "f"), PVar (TID "a"), PAs (TID "xxs") (PCon (consBG ^. asmp) [PVar (TID "x"), PVar (TID "xs")] )]
      ,  ap [Var (TID "foldl"), Var (TID "f"), ap [Var (TID "f"), Var (TID "a"), Var (TID "x")], Var (TID "xs")]
      )
    ]
  )


foldrBG =
  ( "foldr"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
  , [ (  [PWildcard, PVar (TID "a"), PCon (nilBG^.asmp) []]
      ,  Var (TID "a")
     ),( [PVar (TID "f"), PVar (TID "a"), PAs (TID "xxs") (PCon (consBG ^. asmp) [PVar (TID "x"), PVar (TID "xs")] )]
      ,  ap [Var (TID "f"), Var (TID "x"), ap [Var (TID "foldr"), Var (TID "f"), Var (TID "a"), Var (TID "xs")]]
      )
    ]
  )


fractionalDivBG =
  ( "(/)"
  , Forall [Star] ([IsIn (TID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
  , [ (  [ error "no pat for fractionalDiv" ]
      ,  error "no body for fractionalDiv"
      )
    ]
  )


gcdBG =
  ( "gcd"
  , toScheme (tInteger `fn` tInteger `fn` tInteger)
  , [ (  [PVar (TID "a"), PLit (LitInt 0)]
      ,  Var (TID "a")
     ),( [PVar (TID "a"), PVar (TID "b")]
      ,  ap [Var (TID "gcd"), Var (TID "b"), ap [Var (TID "mod"), Var (TID "a"), Var (TID "b")]]
      )
    ]
  )


integralAddBG =
  ( "(+)"
  , Forall [Star] ([IsIn (TID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
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
  , Forall [Star] ([IsIn (TID "Ord") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
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
  , Forall [Star] ([IsIn (TID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0))
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
  , Forall [Star] ([IsIn (TID "Num") (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
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




