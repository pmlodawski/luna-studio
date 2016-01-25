{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.AST.Term where

import Prologue hiding (Cons)

import Data.Base
import Data.Record
import Luna.Syntax.AST.Layout (SubLayouts, SubSemiLayouts, ToStatic, ToDynamic)
import Type.Container
import Type.Cache.TH
import Type.Map

import qualified Luna.Syntax.AST.Layout as Layout


-- | Options in this section should be used only for development purpose and should never be enabled in production ready code.
-- | Their behaviour bases often on manually cached code, which could accidentaly get obsolete.
-- | We could probably throw it away in the future, but the following GHC bugs have to be resolved first:
-- |    - https://ghc.haskell.org/trac/ghc/ticket/8095
-- |    - https://ghc.haskell.org/trac/ghc/ticket/11375

#ifndef RELEASE
#ifdef  FastCompilation
#define CachedTypeFamilies
#endif
#endif


-- Cache related pragmas
#define CACHE(n)       cacheHelper ''n Nothing              ; cacheType ''n Nothing
#define CACHE_AS(n,cn) cacheHelper ''n (Just Quote cn Quote); cacheType ''n (Just Quote cn Quote)
#define CHECK_EQ(s,t)  assertTypesEq (Proxy :: Proxy (s)) (Proxy :: Proxy (t))
#define Quote "


-----------------------------
-- === Component types === --
-----------------------------

newtype Arg a = Arg a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data    Star         = Star                     deriving (Show, Eq, Ord)
data    Str          = Str String               deriving (Show, Eq, Ord)
data    Number       = Number Int               deriving (Show, Eq, Ord)


-- LEGEND
--   N   - Name
--   S   - Source
--   A/P - Args / Params

-- Layout                         N  S  A/P
data    Cons     n t = Cons      !n    ![t]     deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Arrow      t = Arrow        !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Acc      n t = Acc       !n !t          deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    App        t = App          !t ![Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype Var      n   = Var        n             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Unify      t = Unify        !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Blank        = Blank                    deriving (Show, Eq, Ord)


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
type ValVariants   l t = ValElems   (ByLayout l (t (Val   l t))) (t (Val   l t))
type ThunkVariants l t = ThunkElems (ByLayout l (t (Thunk l t))) (t (Thunk l t))
type TermVariants  l t = TermElems  (ByLayout l (t (Term  l t))) (t (Term  l t))
type DraftVariants l t = DraftElems (ByLayout l (t (Draft l t))) (t (Draft l t))

type instance Unlayered (Lit     t) = ASTRecord '[]                                                        LitVariants        t Data
type instance Unlayered (Val   l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val]                    ) (ValVariants   l t) t Data
type instance Unlayered (Thunk l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk]             ) (ThunkVariants l t) t Data
type instance Unlayered (Term  l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk, Term]       ) (TermVariants  l t) t Data
type instance Unlayered (Draft l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk, Term, Draft]) (DraftVariants l t) t Data


-- === Syntax Layouts === --

type family Static  (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Static  a = a Layout.Static
type family Dynamic (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Dynamic a = a Layout.Dynamic

type ByLayout l d = Layout.ByLayout l Str d

type ApplySubLayouts     l  a (t :: * -> *) = ApplyLayouts (SubLayouts     l) a t
type ApplySubSemiLayouts l  a (t :: * -> *) = ApplyLayouts (SubSemiLayouts l) a t
type family ApplyLayouts ls a (t :: * -> *) where ApplyLayouts '[]       a t = '[]
                                                  ApplyLayouts (l ': ls) a t = a l t ': ApplyLayouts ls a t

type family SubLayoutGroups layout (t :: * -> *) gs where
  SubLayoutGroups l t '[]       = '[]
  SubLayoutGroups l t '[g]      = ApplySubLayouts     l g t
  SubLayoutGroups l t (g ': gs) = ApplySubSemiLayouts l g t <> SubLayoutGroups l t gs


-- === Instances === --

-- Show

deriving instance Show (Lit     t)
deriving instance Show (Val   l t)
deriving instance Show (Thunk l t)
deriving instance Show (Term  l t)
deriving instance Show (Draft l t)

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

-- 

type instance Base (Lit     t) = Proxy Lit
type instance Base (Val   l t) = Proxy Val
type instance Base (Thunk l t) = Proxy Thunk
type instance Base (Term  l t) = Proxy Term
type instance Base (Draft l t) = Proxy Draft

-- Wrappers & Layers

instance      Layered   (Lit t)
instance      Rewrapped (Lit t) (Lit t')
instance      Wrapped   (Lit t) where
    type      Unwrapped (Lit t) = Unlayered (Lit t)
    _Wrapped' = iso (\(Lit a) -> a) Lit ; {-# INLINE _Wrapped' #-}

instance      Layered   (Val l t)
instance      Rewrapped (Val l t) (Val l' t')
instance      Wrapped   (Val l t) where
    type      Unwrapped (Val l t) = Unlayered (Val l t)
    _Wrapped' = iso (\(Val a) -> a) Val ; {-# INLINE _Wrapped' #-}

instance      Layered   (Thunk l t)
instance      Rewrapped (Thunk l t) (Thunk l' t')
instance      Wrapped   (Thunk l t) where
    type      Unwrapped (Thunk l t) = Unlayered (Thunk l t)
    _Wrapped' = iso (\(Thunk a) -> a) Thunk ; {-# INLINE _Wrapped' #-}

instance      Layered   (Term l t)
instance      Rewrapped (Term l t) (Term l' t')
instance      Wrapped   (Term l t) where
    type      Unwrapped (Term l t) = Unlayered (Term l t)
    _Wrapped' = iso (\(Term a) -> a) Term ; {-# INLINE _Wrapped' #-}

instance      Layered   (Draft l t)
instance      Rewrapped (Draft l t) (Draft l' t')
instance      Wrapped   (Draft l t) where
    type      Unwrapped (Draft l t) = Unlayered (Draft l t)
    _Wrapped' = iso (\(Draft a) -> a) Draft ; {-# INLINE _Wrapped' #-}

-- Record instances

type instance RecordOf (Lit     t) = RecordOf (Unlayered (Lit     t))
type instance RecordOf (Val   l t) = RecordOf (Unlayered (Val   l t))
type instance RecordOf (Thunk l t) = RecordOf (Unlayered (Thunk l t))
type instance RecordOf (Term  l t) = RecordOf (Unlayered (Term  l t))
type instance RecordOf (Draft l t) = RecordOf (Unlayered (Draft l t))

instance      IsRecord (Lit     t) where asRecord = wrapped' ∘ asRecord
instance      IsRecord (Val   l t) where asRecord = wrapped' ∘ asRecord
instance      IsRecord (Thunk l t) where asRecord = wrapped' ∘ asRecord
instance      IsRecord (Term  l t) where asRecord = wrapped' ∘ asRecord
instance      IsRecord (Draft l t) where asRecord = wrapped' ∘ asRecord

-- Layouts

type instance ToStatic (Lit     t) = Lit                t
type instance ToStatic (Val   l t) = Val   (ToStatic l) t
type instance ToStatic (Thunk l t) = Thunk (ToStatic l) t
type instance ToStatic (Term  l t) = Term  (ToStatic l) t
type instance ToStatic (Draft l t) = Draft (ToStatic l) t

type instance ToDynamic (Lit     t) = Lit                 t
type instance ToDynamic (Val   l t) = Val   (ToDynamic l) t
type instance ToDynamic (Thunk l t) = Thunk (ToDynamic l) t
type instance ToDynamic (Term  l t) = Term  (ToDynamic l) t
type instance ToDynamic (Draft l t) = Draft (ToDynamic l) t

-- Properties

type instance Props p (Lit     t) = Props p (RecordOf (Lit     t))
type instance Props p (Val   l t) = Props p (RecordOf (Val   l t))
type instance Props p (Thunk l t) = Props p (RecordOf (Thunk l t))
type instance Props p (Term  l t) = Props p (RecordOf (Term  l t))
type instance Props p (Draft l t) = Props p (RecordOf (Draft l t))



------------------------------------
-- === AST Layout type caches === --
------------------------------------

-- The following code is result of type-families expressions and is cached in order to speed-up the compilation process.
-- related GHC bug:        https://ghc.haskell.org/trac/ghc/ticket/8095#no1
-- related IRC discussion: http://pastebin.com/9PH7TPB9

-- | All possible groups and variants stored as single 64-bit mask:
-- |   - 9  bits for groups
-- |   - 39 bits for variants
-- |   - 16 bits free for further extensions  

-- === VariantList === --

type  GroupList t =              '[ {-  0 -} Lit           t
                                  , {-  1 -} Static  Val   t
                                  , {-  2 -} Dynamic Val   t 
                                  , {-  3 -} Static  Thunk t
                                  , {-  4 -} Dynamic Thunk t 
                                  , {-  5 -} Static  Term  t
                                  , {-  6 -} Dynamic Term  t 
                                  , {-  7 -} Static  Draft t
                                  , {-  8 -} Dynamic Draft t 
                                  ]
type VariantList_MANUAL_CACHE t = [ {-  9 -} Star
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

#ifndef CachedTypeFamilies

type VariantList_RULE t = Unique (GatherProps Variant (GroupList t))
CACHE_AS(VariantList_RULE, VariantList_GEN_CACHE)
CHECK_EQ(VariantList_GEN_CACHE IM, VariantList_MANUAL_CACHE IM)
type VariantList_CACHE t = VariantList_GEN_CACHE t

#else

type VariantList_CACHE t = VariantList_MANUAL_CACHE t

#endif

type VariantList t = VariantList_CACHE t

-- Layout

type Layout_RULE t = GroupList t <> VariantList t
CACHE_AS(Layout_RULE, Layout_CACHE)

type instance Layout (ASTRecord gs vs t d) = Layout_CACHE t


-- === DecodeMap === --

type DecodeMap_MANUAL_CACHE t =
    'Map [ {-  0 -} '( Lit           t                                       , 0  )
         , {-  1 -} '( Static  Val   t                                       , 1  )
         , {-  2 -} '( Dynamic Val   t                                       , 2  )
         , {-  3 -} '( Static  Thunk t                                       , 3  )
         , {-  4 -} '( Dynamic Thunk t                                       , 4  )
         , {-  5 -} '( Static  Term  t                                       , 5  )
         , {-  6 -} '( Dynamic Term  t                                       , 6  )
         , {-  7 -} '( Static  Draft t                                       , 7  )
         , {-  8 -} '( Dynamic Draft t                                       , 8  )
         , {-  9 -} '( Star                                                  , 9  )
         , {- 10 -} '( Str                                                   , 10 )
         , {- 11 -} '( Number                                                , 11 )
         , {- 12 -} '( Cons  Str (t (Static  Val   t))                       , 12 )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                       , 13 )
         , {- 14 -} '( Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))   , 14 )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                       , 15 )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                       , 16 )
         , {- 17 -} '( App       (t (Static  Thunk t))                       , 17 )
         , {- 18 -} '( Cons  Str (t (Static  Thunk t))                       , 18 )
         , {- 19 -} '( Arrow     (t (Static  Thunk t))                       , 19 )
         , {- 20 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , 20 )
         , {- 21 -} '( App       (t (Dynamic Thunk t))                       , 21 )
         , {- 22 -} '( Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , 22 )
         , {- 23 -} '( Arrow     (t (Dynamic Thunk t))                       , 23 )
         , {- 24 -} '( Var   Str                                             , 24 )
         , {- 25 -} '( Unify     (t (Static  Term t))                        , 25 )
         , {- 26 -} '( Acc   Str (t (Static  Term t))                        , 26 )
         , {- 27 -} '( App       (t (Static  Term t))                        , 27 )
         , {- 28 -} '( Cons  Str (t (Static  Term t))                        , 28 )
         , {- 29 -} '( Arrow     (t (Static  Term t))                        , 29 )
         , {- 30 -} '( Var       (t (Dynamic Term t))                        , 30 )
         , {- 31 -} '( Unify     (t (Dynamic Term t))                        , 31 )
         , {- 32 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , 32 )
         , {- 33 -} '( App       (t (Dynamic Term t))                        , 33 )
         , {- 34 -} '( Cons      (t (Dynamic Term t)) (t (Dynamic Term t))   , 34 )
         , {- 35 -} '( Arrow     (t (Dynamic Term t))                        , 35 )
         , {- 36 -} '( Blank                                                 , 36 )
         , {- 37 -} '( Unify     (t (Static  Draft t))                       , 37 )
         , {- 38 -} '( Acc   Str (t (Static  Draft t))                       , 38 )
         , {- 39 -} '( App       (t (Static  Draft t))                       , 39 )
         , {- 40 -} '( Cons  Str (t (Static  Draft t))                       , 40 )
         , {- 41 -} '( Arrow     (t (Static  Draft t))                       , 41 )
         , {- 42 -} '( Var       (t (Dynamic Draft t))                       , 42 )
         , {- 43 -} '( Unify     (t (Dynamic Draft t))                       , 43 )
         , {- 44 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , 44 )
         , {- 45 -} '( App       (t (Dynamic Draft t))                       , 45 )
         , {- 46 -} '( Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t)) , 46 )
         , {- 47 -} '( Arrow     (t (Dynamic Draft t))                       , 47 )
         ]

#ifndef CachedTypeFamilies

type DecodeMap_RULE t = 'Map $ Zip (Layout_CACHE t) (Enumerate (Size (Layout_CACHE t)))
CACHE_AS(DecodeMap_RULE, DecodeMap_GEN_CACHE)
CHECK_EQ(DecodeMap_GEN_CACHE IM, DecodeMap_MANUAL_CACHE IM)
type DecodeMap_CACHE t = DecodeMap_GEN_CACHE t

#else

type DecodeMap_CACHE t = DecodeMap_MANUAL_CACHE t

#endif

type instance DecodeMap (ASTRecord gs vs t d) = DecodeMap_CACHE t


-- === EncodeMap === --

type EncodeMap_MANUAL_CACHE t =
    'Map [ {-  9 -} '( Star                                                  , '[ 9  , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Str                                                   , '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Number                                                , '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons  Str (t (Static  Val   t))                       , '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                       , '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons      (t (Dynamic Val   t)) (t (Dynamic Val t))   , '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                       , '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                       , '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( App       (t (Static  Thunk t))                       , '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( Cons  Str (t (Static  Thunk t))                       , '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Arrow     (t (Static  Thunk t))                       , '[ 19 , 3,4,5,6,7,8       ] )
         , {- 20 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 20 , 4,6,8             ] )
         , {- 21 -} '( App       (t (Dynamic Thunk t))                       , '[ 21 , 4,6,8             ] )
         , {- 22 -} '( Cons      (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 22 , 4,6,8             ] )
         , {- 23 -} '( Arrow     (t (Dynamic Thunk t))                       , '[ 23 , 4,6,8             ] )
         , {- 24 -} '( Var   Str                                             , '[ 24 , 5,6,7,8           ] )
         , {- 25 -} '( Unify     (t (Static  Term t))                        , '[ 25 , 5,6,7,8           ] )
         , {- 26 -} '( Acc   Str (t (Static  Term t))                        , '[ 26 , 5,6,7,8           ] )
         , {- 27 -} '( App       (t (Static  Term t))                        , '[ 27 , 5,6,7,8           ] )
         , {- 28 -} '( Cons  Str (t (Static  Term t))                        , '[ 28 , 5,6,7,8           ] )
         , {- 29 -} '( Arrow     (t (Static  Term t))                        , '[ 29 , 5,6,7,8           ] )
         , {- 30 -} '( Var       (t (Dynamic Term t))                        , '[ 30 , 6,8               ] )
         , {- 31 -} '( Unify     (t (Dynamic Term t))                        , '[ 31 , 6,8               ] )
         , {- 32 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 32 , 6,8               ] )
         , {- 33 -} '( App       (t (Dynamic Term t))                        , '[ 33 , 6,8               ] )
         , {- 34 -} '( Cons      (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 34 , 6,8               ] )
         , {- 35 -} '( Arrow     (t (Dynamic Term t))                        , '[ 35 , 6,8               ] )
         , {- 36 -} '( Blank                                                 , '[ 36 , 7,8               ] )
         , {- 37 -} '( Unify     (t (Static  Draft t))                       , '[ 37 , 7,8               ] )
         , {- 38 -} '( Acc   Str (t (Static  Draft t))                       , '[ 38 , 7,8               ] )
         , {- 39 -} '( App       (t (Static  Draft t))                       , '[ 39 , 7,8               ] )
         , {- 40 -} '( Cons  Str (t (Static  Draft t))                       , '[ 40 , 7,8               ] )
         , {- 41 -} '( Arrow     (t (Static  Draft t))                       , '[ 41 , 7,8               ] )
         , {- 42 -} '( Var       (t (Dynamic Draft t))                       , '[ 42 , 8                 ] )
         , {- 43 -} '( Unify     (t (Dynamic Draft t))                       , '[ 43 , 8                 ] )
         , {- 44 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 44 , 8                 ] )
         , {- 45 -} '( App       (t (Dynamic Draft t))                       , '[ 45 , 8                 ] )
         , {- 46 -} '( Cons      (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 46 , 8                 ] )
         , {- 47 -} '( Arrow     (t (Dynamic Draft t))                       , '[ 47 , 8                 ] )
         ]

#ifndef CachedTypeFamilies

-- SubGroupRelations

type family MapIndex els (cont :: [*]) where MapIndex '[]       cont = '[]
                                             MapIndex (e ': es) cont = UnsafeIndex e cont ': MapIndex es cont 

type family SubGroups       g  where SubGroups       g         = (UniqueFix (SubGroups' g :: [*]) :: [*])
type family SubGroups'      g  where SubGroups'      g         = GatherSubGroups (Groups g) <> Groups g
type family GatherSubGroups gs where GatherSubGroups '[]       = ('[] :: [*])
                                     GatherSubGroups (g ': gs) = SubGroups' g <> GatherSubGroups gs

type family SubGroupRel    g  where SubGroupRel    g         = '(UnsafeIndex g (Layout_CACHE IM), MapIndex (SubGroups g :: [*]) (Layout_CACHE IM))
type family MapSubGroupRel gs where MapSubGroupRel '[]       = ('[] :: [(Nat, [Nat])])
                                    MapSubGroupRel (g ': gs) = SubGroupRel g ': MapSubGroupRel gs

type SubGroupRelations_RULE = (MapSubGroupRel (GroupList IM) :: [(Nat, [Nat])])
CACHE_AS(SubGroupRelations_RULE, SubGroupRelations)

-- SubGroupInvRelations

type family InverseRel  arg rels where InverseRel arg rels = '(arg, InverseRel' arg rels)
type family InverseRel' (arg :: Nat) (rels :: [(Nat, [Nat])]) where
    InverseRel' a '[]                = '[]
    InverseRel' a ( '(s, ts) ': rs ) = If (a `In` ts) '[s] '[] <> InverseRel' a rs

type family MapInverseRel args rels where
    MapInverseRel '[]       rels = '[]
    MapInverseRel (a ': as) rels = InverseRel a rels ': MapInverseRel as rels

type SubGroupInvRelations_RULE = (MapInverseRel (Enumerate (Size (GroupList IM))) SubGroupRelations :: [(Nat, [Nat])])
CACHE_AS(SubGroupInvRelations_RULE, SubGroupInvRelations)

-- Relation expanders

type        ExpandSubGroupRel  g  rels = g ': ExpandSubGroupRel' g rels
type family ExpandSubGroupRel' g (rels :: [(Nat, [Nat])]) where
    ExpandSubGroupRel' g '[] = '[]
    ExpandSubGroupRel' g ( '(g, rels) ': rs ) = rels <> ExpandSubGroupRel' g rs
    ExpandSubGroupRel' g ( r          ': rs ) =         ExpandSubGroupRel' g rs

type family MapExpandSubGroupRel rels gs where
    MapExpandSubGroupRel rels '[]       = '[]
    MapExpandSubGroupRel rels (g ': gs) = ExpandSubGroupRel g rels <> MapExpandSubGroupRel rels gs

-- SubGroupInvRelations

type family GroupsOf  v    where GroupsOf  v = UnsafeIndex v (Layout_CACHE IM) ': CatMaybes (GroupsOf' v (GroupList IM))
type family GroupsOf' v gs where GroupsOf' v '[]       = '[]
                                 GroupsOf' v (g ': gs) = If (v `In` Variants g) ('Just (UnsafeIndex g (GroupList IM))) 'Nothing ': GroupsOf' v gs

-- EncodeMapRel

type EncodeMapRel a = UniqueFix (MapExpandSubGroupRel SubGroupInvRelations ( (GroupsOf a)))
type family MapEncodeMapRel as where
    MapEncodeMapRel '[] = '[]
    MapEncodeMapRel (a ': as) = EncodeMapRel a ': MapEncodeMapRel as

-- Final rules

type EncodeMap_RULE t = 'Map $ Zip (VariantList t) (MapEncodeMapRel (VariantList IM))
CACHE_AS(EncodeMap_RULE, EncodeMap_GEN_CACHE)
CHECK_EQ(EncodeMap_GEN_CACHE IM, EncodeMap_MANUAL_CACHE IM)
type EncodeMap_CACHE t = EncodeMap_GEN_CACHE t

#else

type EncodeMap_CACHE t = EncodeMap_MANUAL_CACHE t

#endif

type instance EncodeMap (ASTRecord gs vs t d) = EncodeMap_CACHE t




