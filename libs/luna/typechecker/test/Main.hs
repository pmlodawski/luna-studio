
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}


module Main where

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#), assert)
import Type.Container

import Luna.Syntax.AST.Layout (ByLayout, SubLayouts, SubSemiLayouts)
import qualified Luna.Syntax.AST.Layout as Layout
--import Data.Bits.Mask         (Mask)
import GHC.Prim            (Any, unsafeCoerce#)
import Data.Int            (Int64)

import Unsafe.Coerce (unsafeCoerce)
import Data.Relation.Binary
import qualified Data.Relation.Binary as Rel
import Data.Result

import Data.Bits (Bits, FiniteBits, setBit, testBit, zeroBits, finiteBitSize)

import Type.Map

import Data.Foldable (foldl', foldr')
import Control.Monad.State hiding (when, withState)

import Data.Maybe (isNothing, catMaybes)
import Data.Base
import Type.Bool
import Type.List
import System.Environment (getArgs)
import Data.List.Split (chunksOf)

-----------------------------------------------------------------

withState f = do
    s <- get
    put (f s)
{-# INLINE withState #-}

class                                  KnownNats (nats :: [Nat]) where natVals :: Proxy nats -> [Integer]
instance                               KnownNats '[]             where natVals _ = []                                                      ; {-# INLINE natVals #-}
instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns)       where natVals _ = natVal (Proxy :: Proxy n) : natVals (Proxy :: Proxy ns) ; {-# INLINE natVals #-}


-----------------------------------------------------------------

-- === Variant definitions === --

newtype Arg a = Arg a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data    Star         = Star                 deriving (Show, Eq, Ord)
data    Str          = Str String           deriving (Show, Eq, Ord)
data    Number       = Number Int           deriving (Show, Eq, Ord)

data    Cons     n t = Cons     n   [t]     deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Arrow      t = Arrow      t t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Acc      n t = Acc      n t         deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    App        t = App        t [Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype Var      n   = Var      n           deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Unify      t = Unify      t t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Blank        = Blank                deriving (Show, Eq, Ord)


-- === Group definitions === --

newtype Lit                 (t :: * -> *) = Lit   (Unlayered (Lit          t))
newtype Val   (layout :: *) (t :: * -> *) = Val   (Unlayered (Val   layout t))
newtype Thunk (layout :: *) (t :: * -> *) = Thunk (Unlayered (Thunk layout t))
newtype Term  (layout :: *) (t :: * -> *) = Term  (Unlayered (Term  layout t))
newtype Draft (layout :: *) (t :: * -> *) = Draft (Unlayered (Draft layout t))

type LitElems       = Star
                   ': Str
                   ': Number
                   ': '[]

type ValElems   n t = Cons       n t
                   ': Arrow        t
                   ': '[]

type ThunkElems n t = Acc        n t
                   ': App          t
                   ': ValElems   n t

type TermElems  n t = Var        n
                   ': Unify        t
                   ': ThunkElems n t

type DraftElems n t = Blank
                   ': TermElems  n t



type LitVariants       = LitElems
type ValVariants   l t = ValElems   (ByLayout' l (t (Val   l t))) (t (Val   l t))
type ThunkVariants l t = ThunkElems (ByLayout' l (t (Thunk l t))) (t (Thunk l t))
type TermVariants  l t = TermElems  (ByLayout' l (t (Term  l t))) (t (Term  l t))
type DraftVariants l t = DraftElems (ByLayout' l (t (Draft l t))) (t (Draft l t))

type instance Unlayered (Lit     t) = ASTRecord '[]                                                  LitVariants        t Data
type instance Unlayered (Val   l t) = ASTRecord (Lit t ': SubGroups l t '[Val]                    ) (ValVariants   l t) t Data
type instance Unlayered (Thunk l t) = ASTRecord (Lit t ': SubGroups l t '[Val, Thunk]             ) (ThunkVariants l t) t Data
type instance Unlayered (Term  l t) = ASTRecord (Lit t ': SubGroups l t '[Val, Thunk, Term]       ) (TermVariants  l t) t Data
type instance Unlayered (Draft l t) = ASTRecord (Lit t ': SubGroups l t '[Val, Thunk, Term, Draft]) (DraftVariants l t) t Data


-- === Instances ===

-- Bases

type instance Base Star        = Proxy Star
type instance Base Str         = Proxy Str
type instance Base Number      = Proxy Number

type instance Base (Arrow   t) = Proxy Arrow
type instance Base (Cons  n t) = Proxy Cons
type instance Base (Acc   n t) = Proxy Acc
type instance Base (App     t) = Proxy App
type instance Base (Var   n  ) = Proxy Var
type instance Base (Unify   t) = Proxy Unify
type instance Base Blank       = Proxy Blank

-- Wrappers & Layers

instance      Layered   (Lit t)
instance      Rewrapped (Lit t) (Lit t')
instance      Wrapped   (Lit t) where
    type      Unwrapped (Lit t) = Unlayered (Lit t)
    _Wrapped' = iso (\(Lit a) -> a) Lit ;{-# INLINE _Wrapped' #-}

instance      Layered   (Val l t)
instance      Rewrapped (Val l t) (Val l' t')
instance      Wrapped   (Val l t) where
    type      Unwrapped (Val l t) = Unlayered (Val l t)
    _Wrapped' = iso (\(Val a) -> a) Val ;{-# INLINE _Wrapped' #-}

instance      Layered   (Thunk l t)
instance      Rewrapped (Thunk l t) (Thunk l' t')
instance      Wrapped   (Thunk l t) where
    type      Unwrapped (Thunk l t) = Unlayered (Thunk l t)
    _Wrapped' = iso (\(Thunk a) -> a) Thunk ;{-# INLINE _Wrapped' #-}

instance      Layered   (Term l t)
instance      Rewrapped (Term l t) (Term l' t')
instance      Wrapped   (Term l t) where
    type      Unwrapped (Term l t) = Unlayered (Term l t)
    _Wrapped' = iso (\(Term a) -> a) Term ;{-# INLINE _Wrapped' #-}

instance      Layered   (Draft l t)
instance      Rewrapped (Draft l t) (Draft l' t')
instance      Wrapped   (Draft l t) where
    type      Unwrapped (Draft l t) = Unlayered (Draft l t)
    _Wrapped' = iso (\(Draft a) -> a) Draft ;{-# INLINE _Wrapped' #-}


-- Show

deriving instance Show (Lit     t)
deriving instance Show (Val   l t)
deriving instance Show (Thunk l t)
deriving instance Show (Term  l t)
deriving instance Show (Draft l t)

-- Record instances

type instance RecordOf (Lit     t) = RecordOf (Unlayered (Lit     t))
type instance RecordOf (Val   l t) = RecordOf (Unlayered (Val   l t))
type instance RecordOf (Thunk l t) = RecordOf (Unlayered (Thunk l t))
type instance RecordOf (Term  l t) = RecordOf (Unlayered (Term  l t))
type instance RecordOf (Draft l t) = RecordOf (Unlayered (Draft l t))

instance IsRecord (Lit     t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   l t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk l t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Term  l t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft l t) where asRecord = wrapped' ∘ asRecord

-----------------------
-----------------------
-----------------------

-- === Data Layouts ===

type family Static  (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Static  a = a Layout.Static
type family Dynamic (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Dynamic a = a Layout.Dynamic

type ByLayout' l d = ByLayout l Str d

type family ApplyLayouts ls a (t :: * -> *) where ApplyLayouts '[]       a t = '[]
                                                  ApplyLayouts (l ': ls) a t = a l t ': ApplyLayouts ls a t

type ApplySubLayouts     l a (t :: * -> *) = ApplyLayouts (SubLayouts     l) a t
type ApplySubSemiLayouts l a (t :: * -> *) = ApplyLayouts (SubSemiLayouts l) a t


type family SubGroups layout (t :: * -> *) gs where
  SubGroups l t '[]       = '[]
  SubGroups l t '[g]      = ApplySubLayouts     l g t
  SubGroups l t (g ': gs) = ApplySubSemiLayouts l g t <> SubGroups l t gs



--------------------------------------
-- === Data related type caches === --
--------------------------------------

type family GroupList (t :: * -> *) where
            GroupList (t :: * -> *) = '[ Lit           t
                                       , Static  Val   t
                                       , Dynamic Val   t 
                                       , Static  Thunk t
                                       , Dynamic Thunk t 
                                       , Static  Term  t
                                       , Dynamic Term  t 
                                       , Static  Draft t
                                       , Dynamic Draft t 
                                       ] -- [!] Implemented as TF because of #11375

type family VariantList (t :: * -> *) :: [*]
--type family LayoutRelMap (v :: *) (t :: * -> *) :: Map * (Birelation Nat)

--type Layout = (Map * (Birelation Nat) :: Box)
--type family Layout a :: Map * (Birelation Nat)

type instance VariantList t = 
        '[ Star
         , Str
         , Number
         , Cons  Str (t (Static  Val   t))
         , Arrow     (t (Static  Val   t))
         , Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))
         , Arrow     (t (Dynamic Val   t))
         , Acc   Str (t (Static  Thunk t))
         , App       (t (Static  Thunk t))
         , Cons  Str (t (Static  Thunk t))
         , Arrow     (t (Static  Thunk t))
         , Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t))
         , App       (t (Dynamic Thunk t))
         , Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t))
         , Arrow     (t (Dynamic Thunk t))
         , Var   Str
         , Unify     (t (Static  Term t))
         , Acc   Str (t (Static  Term t))
         , App       (t (Static  Term t))
         , Cons  Str (t (Static  Term t))
         , Arrow     (t (Static  Term t))
         , Var       (t (Dynamic Term t))
         , Unify     (t (Dynamic Term t))
         , Acc       (t (Dynamic Term t)) (t (Dynamic Term t))
         , App       (t (Dynamic Term t))
         , Cons      (t (Dynamic Term t)) (t (Dynamic Term t))
         , Arrow     (t (Dynamic Term t))
         , Blank
         , Unify     (t (Static  Draft t))
         , Acc   Str (t (Static  Draft t))
         , App       (t (Static  Draft t))
         , Cons  Str (t (Static  Draft t))
         , Arrow     (t (Static  Draft t))
         , Var       (t (Dynamic Draft t))
         , Unify     (t (Dynamic Draft t))
         , Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t))
         , App       (t (Dynamic Draft t))
         , Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t))
         , Arrow     (t (Dynamic Draft t))
         ]

type MyLayout2 t = 
    [ {-  0 -} Lit           t
    , {-  1 -} Static  Val   t
    , {-  2 -} Dynamic Val   t
    , {-  3 -} Static  Thunk t
    , {-  4 -} Dynamic Thunk t
    , {-  5 -} Static  Term  t
    , {-  6 -} Dynamic Term  t
    , {-  7 -} Static  Draft t
    , {-  8 -} Dynamic Draft t

    , {-  9 -} Star
    , {- 10 -} Str
    , {- 11 -} Number
    , {- 12 -} Cons  Str (t (Static  Val   t))
    , {- 13 -} Arrow     (t (Static  Val   t))
    , {- 14 -} Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))
    , {- 15 -} Arrow     (t (Dynamic Val   t))
    , {- 16 -} Acc   Str (t (Static  Thunk t))
    , {- 17 -} App       (t (Static  Thunk t))
    , {- 18 -} Cons  Str (t (Static  Thunk t))
    , {- 19 -} Arrow     (t (Static  Thunk t))
    , {- 20 -} Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t))
    , {- 21 -} App       (t (Dynamic Thunk t))
    , {- 22 -} Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t))
    , {- 23 -} Arrow     (t (Dynamic Thunk t))
    , {- 24 -} Var   Str
    , {- 25 -} Unify     (t (Static  Term t))
    , {- 26 -} Acc   Str (t (Static  Term t))
    , {- 27 -} App       (t (Static  Term t))
    , {- 28 -} Cons  Str (t (Static  Term t))
    , {- 29 -} Arrow     (t (Static  Term t))
    , {- 30 -} Var       (t (Dynamic Term t))
    , {- 31 -} Unify     (t (Dynamic Term t))
    , {- 32 -} Acc       (t (Dynamic Term t)) (t (Dynamic Term t))
    , {- 33 -} App       (t (Dynamic Term t))
    , {- 34 -} Cons      (t (Dynamic Term t)) (t (Dynamic Term t))
    , {- 35 -} Arrow     (t (Dynamic Term t))
    , {- 36 -} Blank
    , {- 37 -} Unify     (t (Static  Draft t))
    , {- 38 -} Acc   Str (t (Static  Draft t))
    , {- 39 -} App       (t (Static  Draft t))
    , {- 40 -} Cons  Str (t (Static  Draft t))
    , {- 41 -} Arrow     (t (Static  Draft t))
    , {- 42 -} Var       (t (Dynamic Draft t))
    , {- 43 -} Unify     (t (Dynamic Draft t))
    , {- 44 -} Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t))
    , {- 45 -} App       (t (Dynamic Draft t))
    , {- 46 -} Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t))
    , {- 47 -} Arrow     (t (Dynamic Draft t))
    ]

type DerivingMap t = 
    'Map [ {-  9 -} '( Star                                                      , '[ 9  , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Str                                                       , '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Number                                                    , '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons  Str (t (Static  Val   t))                         , '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                         , '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))   , '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                         , '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                         , '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( App       (t (Static  Thunk t))                         , '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( Cons  Str (t (Static  Thunk t))                         , '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Arrow     (t (Static  Thunk t))                         , '[ 19 , 3,4,5,6,7,8       ] )
         , {- 20 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 20 , 4,6,8             ] )
         , {- 21 -} '( App       (t (Dynamic Thunk t))                         , '[ 21 , 4,6,8             ] )
         , {- 22 -} '( Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 22 , 4,6,8             ] )
         , {- 23 -} '( Arrow     (t (Dynamic Thunk t))                         , '[ 23 , 4,6,8             ] )
         , {- 24 -} '( Var   Str                                                 , '[ 24 , 5,7               ] )
         , {- 25 -} '( Unify     (t (Static  Term t))                          , '[ 25 , 5,7               ] )
         , {- 26 -} '( Acc   Str (t (Static  Term t))                          , '[ 26 , 5,7               ] )
         , {- 27 -} '( App       (t (Static  Term t))                          , '[ 27 , 5,7               ] )
         , {- 28 -} '( Cons  Str (t (Static  Term t))                          , '[ 28 , 5,7               ] )
         , {- 29 -} '( Arrow     (t (Static  Term t))                          , '[ 29 , 5,7               ] )
         , {- 30 -} '( Var       (t (Dynamic Term t))                          , '[ 30 , 6,8               ] )
         , {- 31 -} '( Unify     (t (Dynamic Term t))                          , '[ 31 , 6,8               ] )
         , {- 32 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 32 , 6,8               ] )
         , {- 33 -} '( App       (t (Dynamic Term t))                          , '[ 33 , 6,8               ] )
         , {- 34 -} '( Cons      (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 34 , 6,8               ] )
         , {- 35 -} '( Arrow     (t (Dynamic Term t))                          , '[ 35 , 6,8               ] )
         , {- 36 -} '( Blank                                                     , '[ 36 , 7,8               ] )
         , {- 37 -} '( Unify     (t (Static  Draft t))                         , '[ 37 , 7                 ] )
         , {- 38 -} '( Acc   Str (t (Static  Draft t))                         , '[ 38 , 7                 ] )
         , {- 39 -} '( App       (t (Static  Draft t))                         , '[ 39 , 7                 ] )
         , {- 40 -} '( Cons  Str (t (Static  Draft t))                         , '[ 40 , 7                 ] )
         , {- 41 -} '( Arrow     (t (Static  Draft t))                         , '[ 41 , 7                 ] )
         , {- 42 -} '( Var       (t (Dynamic Draft t))                         , '[ 42 , 8                 ] )
         , {- 43 -} '( Unify     (t (Dynamic Draft t))                         , '[ 43 , 8                 ] )
         , {- 44 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 44 , 8                 ] )
         , {- 45 -} '( App       (t (Dynamic Draft t))                         , '[ 45 , 8                 ] )
         , {- 46 -} '( Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 46 , 8                 ] )
         , {- 47 -} '( Arrow     (t (Dynamic Draft t))                         , '[ 47 , 8                 ] )
         ]

type RequestMap t = 
    'Map [ {-  9 -} '( Star                                                      , '[ 9  ] )
         , {- 10 -} '( Str                                                       , '[ 10 ] )
         , {- 11 -} '( Number                                                    , '[ 11 ] )
         , {- 12 -} '( Cons  Str (t (Static  Val   t))                         , '[ 12 ] )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                         , '[ 13 ] )
         , {- 14 -} '( Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))   , '[ 14 ] )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                         , '[ 15 ] )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                         , '[ 16 ] )
         , {- 17 -} '( App       (t (Static  Thunk t))                         , '[ 17 ] )
         , {- 18 -} '( Cons  Str (t (Static  Thunk t))                         , '[ 18 ] )
         , {- 19 -} '( Arrow     (t (Static  Thunk t))                         , '[ 19 ] )
         , {- 20 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 20 ] )
         , {- 21 -} '( App       (t (Dynamic Thunk t))                         , '[ 21 ] )
         , {- 22 -} '( Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 22 ] )
         , {- 23 -} '( Arrow     (t (Dynamic Thunk t))                         , '[ 23 ] )
         , {- 24 -} '( Var   Str                                                 , '[ 24 ] )
         , {- 25 -} '( Unify     (t (Static  Term t))                          , '[ 25 ] )
         , {- 26 -} '( Acc   Str (t (Static  Term t))                          , '[ 26 ] )
         , {- 27 -} '( App       (t (Static  Term t))                          , '[ 27 ] )
         , {- 28 -} '( Cons  Str (t (Static  Term t))                          , '[ 28 ] )
         , {- 29 -} '( Arrow     (t (Static  Term t))                          , '[ 29 ] )
         , {- 30 -} '( Var       (t (Dynamic Term t))                          , '[ 30 ] )
         , {- 31 -} '( Unify     (t (Dynamic Term t))                          , '[ 31 ] )
         , {- 32 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 32 ] )
         , {- 33 -} '( App       (t (Dynamic Term t))                          , '[ 33 ] )
         , {- 34 -} '( Cons      (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 34 ] )
         , {- 35 -} '( Arrow     (t (Dynamic Term t))                          , '[ 35 ] )
         , {- 36 -} '( Blank                                                     , '[ 36 ] )
         , {- 37 -} '( Unify     (t (Static  Draft t))                         , '[ 37 ] )
         , {- 38 -} '( Acc   Str (t (Static  Draft t))                         , '[ 38 ] )
         , {- 39 -} '( App       (t (Static  Draft t))                         , '[ 39 ] )
         , {- 40 -} '( Cons  Str (t (Static  Draft t))                         , '[ 40 ] )
         , {- 41 -} '( Arrow     (t (Static  Draft t))                         , '[ 41 ] )
         , {- 42 -} '( Var       (t (Dynamic Draft t))                         , '[ 42 ] )
         , {- 43 -} '( Unify     (t (Dynamic Draft t))                         , '[ 43 ] )
         , {- 44 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 44 ] )
         , {- 45 -} '( App       (t (Dynamic Draft t))                         , '[ 45 ] )
         , {- 46 -} '( Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 46 ] )
         , {- 47 -} '( Arrow     (t (Dynamic Draft t))                         , '[ 47 ] )
         ]

type MyLayout t = 
    'Map [-- {-  0 -} '( Lit           t                                         , 'OneToMany 0  '[ 0                      ] )
         -- , {-  1 -} '( Static  Val   t                                         , 'OneToMany 1  '[ 1                      ] )
         -- , {-  2 -} '( Dynamic Val   t                                         , 'OneToMany 2  '[ 2                      ] )
         -- , {-  3 -} '( Static  Thunk t                                         , 'OneToMany 3  '[ 3                      ] )
         -- , {-  4 -} '( Dynamic Thunk t                                         , 'OneToMany 4  '[ 4                      ] )
         -- , {-  5 -} '( Static  Term  t                                         , 'OneToMany 5  '[ 5                      ] )
         -- , {-  6 -} '( Dynamic Term  t                                         , 'OneToMany 6  '[ 6                      ] )
         -- , {-  7 -} '( Static  Draft t                                         , 'OneToMany 7  '[ 7                      ] )
         -- , {-  8 -} '( Dynamic Draft t                                         , 'OneToMany 8  '[ 8                      ] )
           {-  9 -} '( Star                                                      , 'OneToMany 9  '[ 9  , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Str                                                       , 'OneToMany 10 '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Number                                                    , 'OneToMany 11 '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons  Str (t (Static  Val   t))                         , 'OneToMany 12 '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                         , 'OneToMany 13 '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))   , 'OneToMany 14 '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                         , 'OneToMany 15 '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                         , 'OneToMany 16 '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( App       (t (Static  Thunk t))                         , 'OneToMany 17 '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( Cons  Str (t (Static  Thunk t))                         , 'OneToMany 18 '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Arrow     (t (Static  Thunk t))                         , 'OneToMany 19 '[ 19 , 3,4,5,6,7,8       ] )
         , {- 20 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , 'OneToMany 20 '[ 20 , 4,6,8             ] )
         , {- 21 -} '( App       (t (Dynamic Thunk t))                         , 'OneToMany 21 '[ 21 , 4,6,8             ] )
         , {- 22 -} '( Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , 'OneToMany 22 '[ 22 , 4,6,8             ] )
         , {- 23 -} '( Arrow     (t (Dynamic Thunk t))                         , 'OneToMany 23 '[ 23 , 4,6,8             ] )
         , {- 24 -} '( Var   Str                                                 , 'OneToMany 24 '[ 24 , 5,7               ] )
         , {- 25 -} '( Unify     (t (Static  Term t))                          , 'OneToMany 25 '[ 25 , 5,7               ] )
         , {- 26 -} '( Acc   Str (t (Static  Term t))                          , 'OneToMany 26 '[ 26 , 5,7               ] )
         , {- 27 -} '( App       (t (Static  Term t))                          , 'OneToMany 27 '[ 27 , 5,7               ] )
         , {- 28 -} '( Cons  Str (t (Static  Term t))                          , 'OneToMany 28 '[ 28 , 5,7               ] )
         , {- 29 -} '( Arrow     (t (Static  Term t))                          , 'OneToMany 29 '[ 29 , 5,7               ] )
         , {- 30 -} '( Var       (t (Dynamic Term t))                          , 'OneToMany 30 '[ 30 , 6,8               ] )
         , {- 31 -} '( Unify     (t (Dynamic Term t))                          , 'OneToMany 31 '[ 31 , 6,8               ] )
         , {- 32 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , 'OneToMany 32 '[ 32 , 6,8               ] )
         , {- 33 -} '( App       (t (Dynamic Term t))                          , 'OneToMany 33 '[ 33 , 6,8               ] )
         , {- 34 -} '( Cons      (t (Dynamic Term t)) (t (Dynamic Term t))   , 'OneToMany 34 '[ 34 , 6,8               ] )
         , {- 35 -} '( Arrow     (t (Dynamic Term t))                          , 'OneToMany 35 '[ 35 , 6,8               ] )
         , {- 36 -} '( Blank                                                     , 'OneToMany 36 '[ 36 , 7,8               ] )
         , {- 37 -} '( Unify     (t (Static  Draft t))                         , 'OneToMany 37 '[ 37 , 7                 ] )
         , {- 38 -} '( Acc   Str (t (Static  Draft t))                         , 'OneToMany 38 '[ 38 , 7                 ] )
         , {- 39 -} '( App       (t (Static  Draft t))                         , 'OneToMany 39 '[ 39 , 7                 ] )
         , {- 40 -} '( Cons  Str (t (Static  Draft t))                         , 'OneToMany 40 '[ 40 , 7                 ] )
         , {- 41 -} '( Arrow     (t (Static  Draft t))                         , 'OneToMany 41 '[ 41 , 7                 ] )
         , {- 42 -} '( Var       (t (Dynamic Draft t))                         , 'OneToMany 42 '[ 42 , 8                 ] )
         , {- 43 -} '( Unify     (t (Dynamic Draft t))                         , 'OneToMany 43 '[ 43 , 8                 ] )
         , {- 44 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , 'OneToMany 44 '[ 44 , 8                 ] )
         , {- 45 -} '( App       (t (Dynamic Draft t))                         , 'OneToMany 45 '[ 45 , 8                 ] )
         , {- 46 -} '( Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t)) , 'OneToMany 46 '[ 46 , 8                 ] )
         , {- 47 -} '( Arrow     (t (Dynamic Draft t))                         , 'OneToMany 47 '[ 47 , 8                 ] )
         ]


type LayoutProxy l = Proxy (l :: Map * (Birelation Nat))


-----------------
-- === NOP === --
-----------------

data NOP = NOP deriving (Show)


------------------------
-- === Properties === --
------------------------
data Value = Value deriving (Show)


type family ValueOf a
class HasValue a where value :: Lens' a (ValueOf a)


    
-------------------
-- === Store === --
-------------------

-- TODO [WD]: rewrite to use memory block (FFI or Primitive)
newtype Store = Store Any
instance Show Store where show _ = "Store"

unsafeStore :: a -> Store
unsafeStore = Store ∘ unsafeCoerce
{-# INLINE unsafeStore #-}

unsafeRestore :: Store -> a
unsafeRestore = unsafeCoerce ∘ unwrap'
{-# INLINE unsafeRestore #-}

instance Rewrapped Store Store
instance Wrapped   Store where
    type Unwrapped Store = Any
    _Wrapped' = iso (\(Store s) -> s) Store 



------------------
-- === Mods === --
------------------

-- === Types === --

---- TODO [WD]: generalize m to PolyKinds
--newtype Modal (m :: ValueMod) a = Modal a deriving (Functor, Traversable, Foldable)

--instance Show (Modal m a) where show _ = "Modal" -- FIXME[WD]: add show when possible


---- === Basic mods === --

--data ValueMod = WithValue
--              | WithoutValue
--              deriving (Show)

--class                                   ValueReader a m             v where readValue :: a -> Modal m v
--instance                                ValueReader a 'WithoutValue v where readValue _ = Modal $ unsafeCoerce NOP ; {-# INLINE readValue #-}
--instance (HasValue a, v ~ ValueOf a) => ValueReader a 'WithValue    v where readValue   = Modal ∘ view value       ; {-# INLINE readValue #-}



----------------------
-- === Modifier === --
----------------------

data Attached t d a = Attached !d !a deriving (Show, Functor, Traversable, Foldable)

attached :: t -> d -> a -> Attached t d a
attached _ = Attached
{-# INLINE attached #-}

(+>) :: a -> d -> Attached Value d a
(+>) = flip $ attached Value
{-# INLINE (+>) #-}

-- Instances

type instance ValueOf   (Attached t d a) = If (t == Value) d (ValueOf a)
type instance Unlayered (Attached t d a) = a
instance      Layered   (Attached t d a) where layered = lens (\(Attached _ a) -> a) (\(Attached d _) a -> Attached d a)

instance {-# OVERLAPPABLE #-} (ValueOf a ~ ValueOf (Attached t d a), HasValue a)
                           => HasValue (Attached t     d a) where value = layered ∘ value                                                 ; {-# INLINE value #-}
instance {-# OVERLAPPABLE #-} HasValue (Attached Value d a) where value = lens (\(Attached d _) -> d) (\(Attached _ a) d -> Attached d a) ; {-# INLINE value #-}



--------------------
-- === Record === --
--------------------

class IsRecord a where asRecord :: forall gs vs. Iso' a (RecordOf a)

type family RecordOf a :: *
type family Variants a :: [*]
type family Groups   a :: [*]
type family Layout   a :: Map * (Birelation Nat)




---------------------------------
-- === AST Data definition === --
---------------------------------

newtype Mask = Mask Int64 deriving (Eq, Num, Bits, FiniteBits)

instance Show Mask where 
    show m = show (catMaybes (testBit' m <$> [0 .. finiteBitSize m - 1])) where
        testBit' m b = if testBit m b then Just b else Nothing


-- === Types === --

data Data = Data { _mask    :: !Mask
                 , _variant :: !Store
                 } deriving (Show)

newtype ASTRecord (groups :: [*]) (variants :: [*]) (t :: * -> *) d = ASTRecord (Unlayered (ASTRecord groups variants t d)) deriving (Show)
type instance Unlayered (ASTRecord gs vs t d) = d

-- === Instances === --

type instance Variants (ASTRecord gs vs t d) = vs
type instance Groups   (ASTRecord gs vs t d) = gs
type instance Layout   (ASTRecord gs vs t d) = MyLayout t

type instance RecordOf (ASTRecord gs vs t d) = ASTRecord gs vs t d
instance      IsRecord (ASTRecord gs vs t d) where asRecord = id ; {-# INLINE asRecord #-}


-- Wrappers

instance Layered   (ASTRecord gs vs t d)
instance Rewrapped (ASTRecord gs vs t d) (ASTRecord gs' vs' t' d')
instance Wrapped   (ASTRecord gs vs t d) where
    type Unwrapped (ASTRecord gs vs t d) = d
    _Wrapped' = iso (\(ASTRecord a) -> a) ASTRecord
    {-# INLINE _Wrapped' #-}


-- === AST Data encoder === --

instance ( rel    ~ MapLookup v layout
         , bits   ~ Rel.Targets rel
         , layout ~ Layout (rec Data)
         , KnownNats bits
         , Wrapped (rec Data)
         , Unwrapped (rec Data) ~ Data
         ) => VariantEncoder v Ok (rec Data) where 
    encodeVariant v = Ok $ wrap' $ Data mask $ unsafeStore v where
        bits    = fromIntegral <$> natVals (Proxy :: Proxy bits)
        mask    = foldl' setBit zeroBits bits
    {-# INLINE encodeVariant #-}

instance ( MaskRebuilder layout layout'
         , layout  ~ Layout (rec  Data)
         , layout' ~ Layout (rec' Data)
         , Unwrapped (rec  Data) ~ Data
         , Unwrapped (rec' Data) ~ Data
         , Wrapped   (rec  Data)
         , Wrapped   (rec' Data)
         ) => RecordRecoder (rec Data) Ok (rec' Data) where 
    recodeRecord (unwrap' -> (Data mask var)) = Ok $ wrap' $ Data mask' var where
        mask'             = rebuildMask (Proxy :: Proxy layout) (Proxy :: Proxy layout') mask
    {-# INLINE recodeRecord #-}


class    MaskRebuilder oldLayout newLayout where rebuildMask :: LayoutProxy oldLayout -> LayoutProxy newLayout -> Mask -> Mask
instance MaskRebuilder layout    layout    where rebuildMask _ _ = id ; {-# INLINE rebuildMask #-}




--------------------------
-- === Construction === --
--------------------------

-- === VariantCons === --
data InvalidVariant v
data InvalidGroup   g

type InvalidVariant' v = Error (InvalidVariant v)
type InvalidGroup'   v = Error (InvalidGroup   v)


class VariantCons     v m rec |    v rec -> m where variantCons  ::                       v -> m rec
class VariantCons' ok v m rec | ok v rec -> m where variantCons' :: Proxy (ok :: Bool) -> v -> m rec

instance ( rec ~ RecordOf r
         , vs  ~ Variants rec
         , ok  ~ (v `In` vs)
         , VariantCons' ok v m (RecordOf r)
         , IsRecord r
         , Functor m )         => VariantCons         v m  r where variantCons    = view (from asRecord) <∘> variantCons' (Proxy :: Proxy ok) ; {-# INLINE variantCons  #-}
instance                          VariantCons         I IM r where variantCons    = impossible                                                ; {-# INLINE variantCons  #-}
instance                          VariantCons         v IM I where variantCons    = impossible                                                ; {-# INLINE variantCons  #-}
instance VariantEncoder v m r  => VariantCons' 'True  v m  r where variantCons' _ = encodeVariant                                             ; {-# INLINE variantCons' #-}
instance m ~ InvalidVariant' v => VariantCons' 'False v m  r where variantCons' _ = const Error                                               ; {-# INLINE variantCons' #-}


-- === GroupCons === --

class GroupCons     g m rec |    g rec -> m where groupCons  ::                       g -> m rec
class GroupCons' ok g m rec | ok g rec -> m where groupCons' :: Proxy (ok :: Bool) -> g -> m rec

instance ( rec ~ RecordOf r
         , gs  ~ Groups rec
         , ok  ~ (g `In` gs)
         , GroupCons' ok g m (RecordOf r)
         , IsRecord r
         , Functor m )        => GroupCons         g m  r where groupCons    = view (from asRecord) <∘> groupCons' (Proxy :: Proxy ok) ; {-# INLINE groupCons  #-}
instance                         GroupCons         I IM r where groupCons    = impossible                                              ; {-# INLINE groupCons  #-}
instance                         GroupCons         v IM I where groupCons    = impossible                                              ; {-# INLINE groupCons  #-}
instance RecordRecoder' v m r => GroupCons' 'True  v m  r where groupCons' _ = encodeGroup                                             ; {-# INLINE groupCons' #-}
instance m ~ InvalidGroup' v  => GroupCons' 'False v m  r where groupCons' _ = const Error                                             ; {-# INLINE groupCons' #-}


encodeGroup = recodeRecord ∘ view asRecord
{-# INLINE encodeGroup #-}

-- === Constructions encoders === --

class VariantEncoder v m rec | v rec -> m where encodeVariant :: v -> m rec
class RecordRecoder  g m rec | g rec -> m where recodeRecord  :: g -> m rec

type RecordRecoder' g m rec = (IsRecord g, RecordRecoder (RecordOf g) m rec)

-- - zrobic uncheckedRec i checkedRec
-- - zrobic resolution typow przy konstruktorach i pattern matchach
-- - zrobic konstruktory typow zlozonych
-- - zrobic ladne typy pattern matchingu



------------------------------
-- === Pattern matching === --
------------------------------

class LayoutMatch v rec where layoutMatch :: forall a. (v -> a) -> rec -> Maybe a

instance ( rel    ~ MapLookup a layout
         , nat    ~ Rel.Source rel
         , layout ~ Layout (rec Data)
         , Unwrapped (rec Data) ~ Data
         , Wrapped   (rec Data)
         , KnownNat nat
         ) => LayoutMatch a (rec Data) where
    layoutMatch f (unwrap' -> (Data mask v)) = if match then Just (f val) else Nothing where
        bit   = fromIntegral $ natVal (Proxy :: Proxy nat)
        match = testBit mask bit
        val   = unsafeRestore v :: a
    {-# INLINE layoutMatch #-}


runMatch :: (LayoutMatch v (RecordOf r), IsRecord r) => (v -> a) -> r -> Maybe a
runMatch f (rec :: rec) = layoutMatch f $ view asRecord rec
{-# INLINE runMatch #-}

match f = withState (runMatch f:)
{-# INLINE match #-}

-- TODO [WD]: Add TH case' interface
__case__ lib file loc t s = case catMaybes $ ($ t) <$> execState s [] of
    (r : _) -> r
    []      -> error $ lib <> ": " <> file <> ":" <> show loc <> ": Non-exhaustive patterns in case"
{-# INLINE __case__ #-}




-------------------------------------------------------------------------------------------------------------------
-- TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST --
-------------------------------------------------------------------------------------------------------------------

newtype IDT a = IDT a deriving (Show, Functor, Traversable, Foldable)

--star' :: ASTRecord '[] '[] IDT
--star' = variantCons $ Star +> 5

--star :: Lit t
--star = variantCons Star


caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}

data Test a b = Test !a !b  deriving (Show)

main = do
    --let v  = star :: Lit Int IDT
    let Ok v  = variantCons Star :: Ok (Lit IDT)
        --Ok v' = groupCons v :: Ok (Static Draft IDT)
    --let v  = variantCons Star :: Ok (Static Draft IDT)
    --let v  = variantCons (1 :: Int) :: Ok (Lit IDT)
        --l  = variantCons v :: Static Thunk Int IDT
        --l2 = variantCons l  :: Dynamic Val Int IDT
        --l2 = variantCons l  :: Dynamic Thunk Int IDT

    print v
    --print v'

    --print l
    --print l2

    --print $ caseTest v $ do
    --    match (\Star -> (1 :: Int))

    return ()




-------------------------
-- === Benchmarks === ---
-------------------------


--data Bench a = Bench1 a
--             | Bench2
--             deriving (Show)

--main = do


--    args <- getArgs
--    let mode   = read (args !! 0) :: Int
--        argnum = read (args !! 1) :: Int
--        nums = [0..argnum]


--    case mode of
--        0 -> do
--            let ls = const star . show <$> nums
--                pattest l = caseTest l $ do
--                    match (\Star -> (1 :: Int))
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--        1 -> do
--            let ls = const Bench2 . show <$> nums
--                pattest l = case l of
--                    Bench2 -> (1 :: Int)
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
