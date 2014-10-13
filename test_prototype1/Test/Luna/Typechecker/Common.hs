{-# LANGUAGE RankNTypes #-}

module Test.Luna.Typechecker.Common where


import Luna.Typechecker.AST.ClassID   (ClassID(..))
import Luna.Typechecker.AST.Kind      (Kind(..))
import Luna.Typechecker.AST.Lit       (Lit(..))
import Luna.Typechecker.AST.Pat       (Pat(..))
import Luna.Typechecker.AST.Scheme    (Scheme(..),toScheme)
import Luna.Typechecker.AST.TID       (TID(..))
import Luna.Typechecker.AST.Type      (Type(..),Tyvar(..),fn,tInteger,list,tBool,pair,tChar,tDouble,tFloat,tInt,tUnit,tList,tArrow,tTuple2)
import Luna.Typechecker.AST.VarID     (VarID(..))

import Luna.Typechecker.Assumptions   (Assump(..))
import Luna.Typechecker.BindingGroups (BindGroup,Alt,Expr(..),Impl,Expl)
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
s^.l = getConst (l Const s)

tid :: Lens TypeDecl TypeDecl TID TID
tid f (t, s, as) = (\(TID t') -> (t', s, as)) <$> f (TID t)

scheme :: Lens TypeDecl TypeDecl Scheme Scheme
scheme f (t, s, as) = (\s' -> (t, s', as)) <$> f s

alts :: Lens TypeDecl TypeDecl [Alt] [Alt]
alts f (t, s, as) = (\as' -> (t, s, as')) <$> f as

--expl :: Lens TypeDecl TypeDecl 
expl :: Lens TypeDecl TypeDecl Expl Expl
expl f (t, s, as) = (\(VarID ts',s',as') -> (ts',s',as')) <$> f (VarID t, s, as)

bndgrpexpl :: Lens TypeDecl TypeDecl BindGroup BindGroup
bndgrpexpl f (t, s, as) = reconstruct <$> f ([(VarID t, s, as)], [])
  where reconstruct ((VarID t',s',as'):_ , _) = (t', s', as')
        reconstruct _                         = (t, s, as)

bndgrpimpl :: Lens TypeDecl TypeDecl BindGroup BindGroup
bndgrpimpl f (t, s, as) = reconstruct <$> f ([], [[(VarID t, as)]])
  where reconstruct (_ , [ (VarID t',as'):_ ]) = (t', s, as')
        reconstruct _                          = (t, s, as)

--impl :: Lens TypeDecl Impl TypeDecl Impl
impl :: Lens TypeDecl (String, Scheme, c) Impl (VarID,c)
impl f (t, s, as) = (\(VarID t',as') -> (t',s,as')) <$> f (VarID t, as)

asmp :: Lens (String, Scheme, [Alt]) (String, Scheme, [Alt]) Assump Assump
asmp f (t, s, as) = (\(VarID t' :>: s') -> (t', s', as)) <$> f (VarID t :>: s)


-- ------------------------------------------------------------
-- ████████╗██╗   ██╗██████╗ ███████╗ ██████╗██╗      █████╗ ███████╗███████╗███████╗███████╗
-- ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝██╔════╝██║     ██╔══██╗██╔════╝██╔════╝██╔════╝██╔════╝
--    ██║    ╚████╔╝ ██████╔╝█████╗  ██║     ██║     ███████║███████╗███████╗█████╗  ███████╗
--    ██║     ╚██╔╝  ██╔═══╝ ██╔══╝  ██║     ██║     ██╔══██║╚════██║╚════██║██╔══╝  ╚════██║
--    ██║      ██║   ██║     ███████╗╚██████╗███████╗██║  ██║███████║███████║███████╗███████║
--    ╚═╝      ╚═╝   ╚═╝     ╚══════╝ ╚═════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝
-- ------------------------------------------------------------

standardET :: (Monad m) => EnvTransformer m
standardET =  addClass (ClassID "Eq") (ClassID <$> [])
              <:> addInst [] (IsIn (ClassID "Eq") tUnit)
              <:> addInst [] (IsIn (ClassID "Eq") tBool)
              <:> addInst [] (IsIn (ClassID "Eq") tChar)
              <:> addInst [] (IsIn (ClassID "Eq") tInt)
              <:> addInst [] (IsIn (ClassID "Eq") tInteger)
              <:> addInst [] (IsIn (ClassID "Eq") tFloat)
              <:> addInst [] (IsIn (ClassID "Eq") tDouble)
              <:> addInst [IsIn (ClassID "Eq") tvarA] (IsIn (ClassID "Eq") (list tvarA))
              <:> addInst [IsIn (ClassID "Eq") tvarA, IsIn (ClassID "Eq") tvarB] (IsIn (ClassID "Eq") (tvarA `pair` tvarB))
          <:> addClass (ClassID "Ord") (ClassID <$> ["Eq"])
              <:> addInst [] (IsIn (ClassID "Ord") tUnit)
              <:> addInst [] (IsIn (ClassID "Ord") tBool)
              <:> addInst [] (IsIn (ClassID "Ord") tChar)
              <:> addInst [] (IsIn (ClassID "Ord") tInt)
              <:> addInst [] (IsIn (ClassID "Ord") tInteger)
              <:> addInst [] (IsIn (ClassID "Ord") tFloat)
              <:> addInst [] (IsIn (ClassID "Ord") tDouble)
              <:> addInst [IsIn (ClassID "Ord") tvarA] (IsIn (ClassID "Ord") (list tvarA))
              <:> addInst [IsIn (ClassID "Ord") tvarA, IsIn (ClassID "Ord") tvarB] (IsIn (ClassID "Ord") (tvarA `pair` tvarB))
          <:> addClass (ClassID "Enum") (ClassID <$> [])
              <:> addInst [] (IsIn (ClassID "Enum") tUnit)
              <:> addInst [] (IsIn (ClassID "Enum") tBool)
              <:> addInst [] (IsIn (ClassID "Enum") tChar)
              <:> addInst [] (IsIn (ClassID "Enum") tInt)
              <:> addInst [] (IsIn (ClassID "Enum") tInteger)
              <:> addInst [] (IsIn (ClassID "Enum") tFloat)
              <:> addInst [] (IsIn (ClassID "Enum") tDouble)
          <:> addClass (ClassID "Num") (ClassID <$> [])
              <:> addInst [] (IsIn (ClassID "Num") tInt)
              <:> addInst [] (IsIn (ClassID "Num") tInteger)
              <:> addInst [] (IsIn (ClassID "Num") tFloat)
              <:> addInst [] (IsIn (ClassID "Num") tDouble)
          <:> addClass (ClassID "Real") (ClassID <$> ["Num", "Ord"])
              <:> addInst [] (IsIn (ClassID "Real") tInt)
              <:> addInst [] (IsIn (ClassID "Real") tInteger)
              <:> addInst [] (IsIn (ClassID "Real") tFloat)
              <:> addInst [] (IsIn (ClassID "Real") tDouble)
          <:> addClass (ClassID "Integral") (ClassID <$> ["Real", "Enum"])
              <:> addInst [] (IsIn (ClassID "Integral") tInt)
              <:> addInst [] (IsIn (ClassID "Integral") tInteger)
              <:> addInst [] (IsIn (ClassID "Integral") tFloat)
              <:> addInst [] (IsIn (ClassID "Integral") tDouble)
          <:> addClass (ClassID "Fractional") (ClassID <$> ["Num"])
              <:> addInst [] (IsIn (ClassID "Fractional") tFloat)
              <:> addInst [] (IsIn (ClassID "Fractional") tDouble)
          <:> addClass (ClassID "Floating") (ClassID <$> ["Fractional"])
              <:> addInst [] (IsIn (ClassID "Floating") tFloat)
              <:> addInst [] (IsIn (ClassID "Floating") tDouble)
          <:> addClass (ClassID "Functor") (ClassID <$> [])
              <:> addInst [] (IsIn (ClassID "Functor") tList)
              <:> addInst [] (IsIn (ClassID "Functor") (TAp tArrow tvarA))
              <:> addInst [] (IsIn (ClassID "Functor") (TAp tTuple2 tvarA))
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
      , ap [Var (VarID "foldr"), Var (VarID "(&&)"), EConst (VarID "True" :>: toScheme tBool)]
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
  , [ ( [ PVar (VarID "x"), PVar (VarID "y") ]
      , Var (VarID "x")
      )
    ]
  )


eqBG =
  ( "(==)"
  , Forall [Star] ([IsIn (ClassID "Eq") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
  , [ ( [ error "no pat for eq" ]
      , error "no body for eq"
      )
    ]
  )


fromIntegralBG =
  ( "fromIntegral"
  , Forall [Star, Star] ([IsIn (ClassID "Integral") (TGen 0), IsIn (ClassID "Num") (TGen 1)] :=> (TGen 0 `fn` TGen 1))
  , [ ( [ error "no pat for fromIntegral" ]
      , error "no body for fromIntegral"
      )
    ]
  )


foldlBG =
  ( "foldl"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 0 `fn` list (TGen 1) `fn` TGen 0))
  , [ (  [PWildcard, PVar (VarID "a"), PCon (nilBG^.asmp) []]
      ,  Var (VarID "a")
     ),( [PVar (VarID "f"), PVar (VarID "a"), PAs (VarID "xxs") (PCon (consBG^.asmp) [PVar (VarID "x"), PVar (VarID "xs")] )]
      ,  ap [Var (VarID "foldl"), Var (VarID "f"), ap [Var (VarID "f"), Var (VarID "a"), Var (VarID "x")], Var (VarID "xs")]
      )
    ]
  )


foldrBG =
  ( "foldr"
  , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
  , [ (  [PWildcard, PVar (VarID "a"), PCon (nilBG^.asmp) []]
      ,  Var (VarID "a")
     ),( [PVar (VarID "f"), PVar (VarID "a"), PAs (VarID "xxs") (PCon (consBG^.asmp) [PVar (VarID "x"), PVar (VarID "xs")] )]
      ,  ap [Var (VarID "f"), Var (VarID "x"), ap [Var (VarID "foldr"), Var (VarID "f"), Var (VarID "a"), Var (VarID "xs")]]
      )
    ]
  )


fractionalDivBG =
  ( "(/)"
  , Forall [Star] ([IsIn (ClassID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
  , [ (  [ error "no pat for fractionalDiv" ]
      ,  error "no body for fractionalDiv"
      )
    ]
  )


gcdBG =
  ( "gcd"
  , toScheme (tInteger `fn` tInteger `fn` tInteger)
  , [ (  [PVar (VarID "a"), PLit (LitInt 0)]
      ,  Var (VarID "a")
     ),( [PVar (VarID "a"), PVar (VarID "b")]
      ,  ap [Var (VarID "gcd"), Var (VarID "b"), ap [Var (VarID "mod"), Var (VarID "a"), Var (VarID "b")]]
      )
    ]
  )


integralAddBG =
  ( "(+)"
  , Forall [Star] ([IsIn (ClassID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` TGen 0))
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
  , Forall [Star] ([IsIn (ClassID "Ord") (TGen 0)] :=> (TGen 0 `fn` TGen 0 `fn` tBool))
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
  , Forall [Star] ([IsIn (ClassID "Num") (TGen 0)] :=> (TGen 0 `fn` TGen 0))
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
  , Forall [Star] ([IsIn (ClassID "Num") (TGen 0)] :=> (list (TGen 0) `fn` TGen 0))
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




