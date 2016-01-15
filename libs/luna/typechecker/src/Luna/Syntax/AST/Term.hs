{-# LANGUAGE CPP                  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.AST.Term where

import Prologue hiding (Cons)

import Data.Attributes
import Data.Base
import Data.Path
import Data.Relation.Binary
import Data.Variant.Layers
import Data.Variant.Properties
import Data.Variant.Record
import Luna.Syntax.AST.Layout (ByLayout, SubLayouts, SubSemiLayouts)
import Type.Container
import Type.Map
import Type.Regex
import Type.Cache.TH
import Type.Zip
import Type.Sequence          (Range')
import Data.Bits.Mask         (Mask)

import qualified Luna.Syntax.AST.Layout (ByLayout, SubLayouts, SubSemiLayouts)

import qualified Luna.Syntax.AST.Layout as Layout


-- Cache related pragmas
#define CACHE(n)       cacheHelper ''n Nothing    ; cacheType ''n Nothing
#define CACHE_AS(n,cn) cacheHelper ''n (Just Quote cn Quote); cacheType ''n (Just Quote cn Quote)
#define Quote          "


-----------------------
-- === AST Terms === --
-----------------------

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

newtype Lit                 (v :: *) (t :: * -> *) = Lit   (Unlayered (Lit          v t))
newtype Val   (layout :: *) (v :: *) (t :: * -> *) = Val   (Unlayered (Val   layout v t))
newtype Thunk (layout :: *) (v :: *) (t :: * -> *) = Thunk (Unlayered (Thunk layout v t))
newtype Term  (layout :: *) (v :: *) (t :: * -> *) = Term  (Unlayered (Term  layout v t))
newtype Draft (layout :: *) (v :: *) (t :: * -> *) = Draft (Unlayered (Draft layout v t))

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


type LitVariants         = LitElems
type ValVariants   l v t = ValElems   (ByLayout' l (t (Val   l v t))) (t (Val   l v t))
type ThunkVariants l v t = ThunkElems (ByLayout' l (t (Thunk l v t))) (t (Thunk l v t))
type TermVariants  l v t = TermElems  (ByLayout' l (t (Term  l v t))) (t (Term  l v t))
type DraftVariants l v t = DraftElems (ByLayout' l (t (Draft l v t))) (t (Draft l v t))

type instance Unlayered (Lit     v t) = Data 'False '[]                                                 LitVariants          v t
type instance Unlayered (Val   l v t) = Data 'True  (Lit v t ': SubGroups l v t '[Val]               ) (ValVariants   l v t) v t
type instance Unlayered (Thunk l v t) = Data 'False (Lit v t ': SubGroups l v t '[Val, Thunk]        ) (ThunkVariants l v t) v t
type instance Unlayered (Term  l v t) = Data 'False (Lit v t ': SubGroups l v t '[Val, Thunk, Term]  ) (TermVariants  l v t) v t
type instance Unlayered (Draft l v t) = Data 'False (Lit v t ': SubGroups l v t '[Thunk, Term, Draft]) (DraftVariants l v t) v t


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

instance      Layered   (Lit v t)
instance      Rewrapped (Lit v t) (Lit v' t')
instance      Wrapped   (Lit v t) where
    type      Unwrapped (Lit v t) = Unlayered (Lit v t)
    _Wrapped' = iso (\(Lit a) -> a) Lit ;{-# INLINE _Wrapped' #-}

instance      Layered   (Val l v t)
instance      Rewrapped (Val l v t) (Val l' v' t')
instance      Wrapped   (Val l v t) where
    type      Unwrapped (Val l v t) = Unlayered (Val l v t)
    _Wrapped' = iso (\(Val a) -> a) Val ;{-# INLINE _Wrapped' #-}

instance      Layered   (Thunk l v t)
instance      Rewrapped (Thunk l v t) (Thunk l' v' t')
instance      Wrapped   (Thunk l v t) where
    type      Unwrapped (Thunk l v t) = Unlayered (Thunk l v t)
    _Wrapped' = iso (\(Thunk a) -> a) Thunk ;{-# INLINE _Wrapped' #-}

instance      Layered   (Term l v t)
instance      Rewrapped (Term l v t) (Term l' v' t')
instance      Wrapped   (Term l v t) where
    type      Unwrapped (Term l v t) = Unlayered (Term l v t)
    _Wrapped' = iso (\(Term a) -> a) Term ;{-# INLINE _Wrapped' #-}

instance      Layered   (Draft l v t)
instance      Rewrapped (Draft l v t) (Draft l' v' t')
instance      Wrapped   (Draft l v t) where
    type      Unwrapped (Draft l v t) = Unlayered (Draft l v t)
    _Wrapped' = iso (\(Draft a) -> a) Draft ;{-# INLINE _Wrapped' #-}


-- Show

deriving instance Show (Lit     v t)
deriving instance Show (Val   l v t)
deriving instance Show (Thunk l v t)
deriving instance Show (Term  l v t)
deriving instance Show (Draft l v t)

-- Record instances

type instance RecordOf (Lit     v t) = Unwrapped (Lit     v t)
type instance RecordOf (Val   l v t) = Unwrapped (Val   l v t)
type instance RecordOf (Thunk l v t) = Unwrapped (Thunk l v t)
type instance RecordOf (Term  l v t) = Unwrapped (Term  l v t)
type instance RecordOf (Draft l v t) = Unwrapped (Draft l v t)

instance      IsRecord (Lit     v t) where record = wrapped'
instance      IsRecord (Val   l v t) where record = wrapped'
instance      IsRecord (Thunk l v t) where record = wrapped'
instance      IsRecord (Term  l v t) where record = wrapped'
instance      IsRecord (Draft l v t) where record = wrapped'

-- Bases

type instance Base (Lit     v t) = Proxy Lit
type instance Base (Val   l v t) = Proxy Val
type instance Base (Thunk l v t) = Proxy Thunk
type instance Base (Term  l v t) = Proxy Term
type instance Base (Draft l v t) = Proxy Draft

-- Data.Path.Focus

type instance Focused n (Lit     v t) = Focused n (Unwrapped (Lit     v t))
type instance Focused n (Val   l v t) = Focused n (Unwrapped (Val   l v t))
type instance Focused n (Thunk l v t) = Focused n (Unwrapped (Thunk l v t))
type instance Focused n (Term  l v t) = Focused n (Unwrapped (Term  l v t))
type instance Focused n (Draft l v t) = Focused n (Unwrapped (Draft l v t))

instance Focus n (Unwrapped (Lit     v t)) => Focus   n (Lit     v t) where focus = wrapped ∘∘ focus
instance Focus n (Unwrapped (Val   l v t)) => Focus   n (Val   l v t) where focus = wrapped ∘∘ focus
instance Focus n (Unwrapped (Thunk l v t)) => Focus   n (Thunk l v t) where focus = wrapped ∘∘ focus
instance Focus n (Unwrapped (Term  l v t)) => Focus   n (Term  l v t) where focus = wrapped ∘∘ focus
instance Focus n (Unwrapped (Draft l v t)) => Focus   n (Draft l v t) where focus = wrapped ∘∘ focus

-- Successors

type instance Succ (Lit     v t) = Succ (Unwrapped (Lit     v t))
type instance Succ (Val   l v t) = Succ (Unwrapped (Val   l v t))
type instance Succ (Thunk l v t) = Succ (Unwrapped (Thunk l v t))
type instance Succ (Term  l v t) = Succ (Unwrapped (Term  l v t))
type instance Succ (Draft l v t) = Succ (Unwrapped (Draft l v t))

-- === Data Layouts ===

type family Static  (a :: * -> k) :: k where Static  a = a Layout.Static
type family Dynamic (a :: * -> k) :: k where Dynamic a = a Layout.Dynamic

type ByLayout' l d = ByLayout l Str d

type family ApplyLayouts ls a v t where ApplyLayouts '[]       a v t = '[]
                                        ApplyLayouts (l ': ls) a v t = a l v t ': ApplyLayouts ls a v t

type ApplySubLayouts     l a v t = ApplyLayouts (SubLayouts     l) a v t
type ApplySubSemiLayouts l a v t = ApplyLayouts (SubSemiLayouts l) a v t


type family SubGroups layout v t gs where
  SubGroups l v t '[]       = '[]
  SubGroups l v t '[g]      = ApplySubLayouts     l g v t
  SubGroups l v t (g ': gs) = ApplySubSemiLayouts l g v t <> SubGroups l v t gs


------------------------------------
-- === AST data specification === --
------------------------------------

-- [!] Layout is implemented as newtype to hide type complexity and shorten compilation times.
--     It should be considered obsolete when #8095 will be fixed

-- TODO [WD]: In order to complete the abstraction,
--            we should implement the redirection operator +->
--            which would allow us to redirect different tags to the same one:
--                +-> Selection @# Selection # Variant
--                +-> Selection @# Selection # Group

type Data hasVal gs vs v t = Record
                          +> ASTLayout v t                      @: Layout
                          +> gs                                 @@ Group
                          +> vs                                 @@ Variant
                          +> ExistsIf hasVal v                  @# Value
                          +> Store                              @# Variant
                          +> MatchY (LayoutRelMap v t) ElemMask @# Selection

newtype ASTLayout v t = ASTLayout (LayoutDef v t)
type    LayoutDef v t = Record
                     +> OneOf (GroupList   v t) @: Group   -- Implemented NOT as @@ because of compilation times 
                     +> OneOf (VariantList v t) @: Variant -- Implemented NOT as @@ because of compilation times

-- Instances

instance Rewrapped (ASTLayout v t) (ASTLayout v' t')
instance Wrapped   (ASTLayout v t) where
    type Unwrapped (ASTLayout v t) = LayoutDef v t
    _Wrapped' = iso (\(ASTLayout a) -> a) ASTLayout

-- FIXME [WD]: provide path info
--             it is not crutial though, because we use only type information here and we do not store any real values
type instance Succ (ASTLayout v t) = Succ (Unwrapped (ASTLayout v t))



-- === Type Declarations === --

-- | These type families are just type aliases, but because of GHC stage restriction they need to be defined
-- | after the caching TemplateHaskell functions below.

type family ElemMask :: *
type family LayoutRelMap (v :: *) (t :: * -> *) :: Map * (Birelation Nat)
type family VariantList  (v :: *) (t :: * -> *) :: [*]
type family ElemList     (v :: *) (t :: * -> *) where ElemList v t = GroupList v t <> VariantList v t -- [!] Implemented as TF because of #11375
type family GroupList    (v :: *) (t :: * -> *) where
            GroupList    (v :: *) (t :: * -> *) = '[ Lit           v t
                                                   , Static  Val   v t
                                                   , Dynamic Val   v t 
                                                   , Static  Thunk v t
                                                   , Dynamic Thunk v t 
                                                   , Static  Term  v t
                                                   , Dynamic Term  v t 
                                                   , Static  Draft v t
                                                   , Dynamic Draft v t 
                                                   ] -- [!] Implemented as TF because of #11375

type AbstractGroupList   = GroupList   I IM
type AbstractVariantList = VariantList I IM
type AbstractElemList    = ElemList    I IM


--------------------------------------
-- === Data related type caches === --
--------------------------------------
-- The following code is result of type-families expressions and is cached in order to speed-up the compilation process.
-- related GHC bug:        https://ghc.haskell.org/trac/ghc/ticket/8095#no1
-- related IRC discussion: http://pastebin.com/9PH7TPB9

-- | All possible groups and variants stored as single 64-bit mask:
-- |   - 9  bits for groups
-- |   - 39 bits for variants
-- |   - 16 bits free for further extensions                            


#ifndef CachedTypeFamilies

-- === VariantList === --
-- | List of all possible variants.
-- | See the example cache below.

type VariantList_RULE v t = Unique (OneOf (GroupList v t) ##. Variants)
CACHE_AS(VariantList_RULE, VariantList_CACHE)
type instance VariantList v t = VariantList_CACHE v t


-- === LayoutRelMap === --
-- | Record construction relations between input variants and the layout bit-mask
-- | See the example cache below.

type family NatEnumerate a where NatEnumerate a = NatEnumerateFrom 0 a -- Implemented as TF because of #11375
type family NatEnumerateFrom (n :: Nat) (lst :: [k]) where
    NatEnumerateFrom n '[]       = '[]
    NatEnumerateFrom n (l ': ls) = '(l,n) ': NatEnumerateFrom (n + 1) ls

type family EnumeratedSubMatch lst m where EnumeratedSubMatch lst m = (EnumeratedSubMatch' lst m) -- Implemented as TF because of #11375
type family EnumeratedSubMatch' lst m where
    EnumeratedSubMatch' '[]               m = '[]
    EnumeratedSubMatch' ( '(a, n) ': ls ) m =  TaggedLst n (a ##. m) <> EnumeratedSubMatch' ls m 

type family TaggedLst (t :: k) (lst :: [l]) :: [(l,k)] where
    TaggedLst t '[]       = '[]
    TaggedLst t (l ': ls) = '(l,t) ': TaggedLst t ls

type family GatherAssocs (key :: k) (ascs :: [(k, v)]) :: [v] where
    GatherAssocs k '[]              = '[]
    GatherAssocs k ( '(k, v) ': as) = v ': GatherAssocs k as
    GatherAssocs k ( '(l, v) ': as) =      GatherAssocs k as

type CollectAssocs key ascs = '(key, GatherAssocs key ascs)

type family MapCollectAssocs (keys :: [k]) (ascs :: [(k, v)]) :: [(k,[v])] where
    MapCollectAssocs '[]       ascs = '[]
    MapCollectAssocs (k ': ks) ascs = CollectAssocs k ascs ': MapCollectAssocs ks ascs

type family MapGatherAssocs (keys :: [k]) (ascs :: [(k, v)]) :: [[v]] where
    MapGatherAssocs '[]       ascs = '[]
    MapGatherAssocs (k ': ks) ascs = GatherAssocs k ascs ': MapGatherAssocs ks ascs

--

type VariantRelAssocs_RULE   = NatEnumerate AbstractElemList
                            <> EnumeratedSubMatch (NatEnumerate AbstractGroupList) Variants
type LayoutTargets_RULE      = MapGatherAssocs AbstractElemList VariantRelAssocs_RULE
type LayoutSources_RULE      = Range' (Size AbstractElemList)
type LayoutRelMap_RULE   v t = 'Map (Zip (ElemList v t) (ZipWith 'OneToMany LayoutSources_RULE LayoutTargets_RULE))

CACHE_AS(LayoutRelMap_RULE, LayoutRelMap_CACHE)
type instance LayoutRelMap v t = LayoutRelMap_CACHE v t

#else

type instance VariantList v t = '[ Star
                                 , Str
                                 , Number
                                 , Cons  Str (t (Static  Val   v t))
                                 , Arrow     (t (Static  Val   v t))
                                 , Cons      (t (Dynamic Val   v t)) (t (Dynamic Val v t))
                                 , Arrow     (t (Dynamic Val   v t))
                                 , Acc   Str (t (Static  Thunk v t))
                                 , App       (t (Static  Thunk v t))
                                 , Cons  Str (t (Static  Thunk v t))
                                 , Arrow     (t (Static  Thunk v t))
                                 , Acc       (t (Dynamic Thunk v t)) (t (Dynamic Thunk v t))
                                 , App       (t (Dynamic Thunk v t))
                                 , Cons      (t (Dynamic Thunk v t)) (t (Dynamic Thunk v t))
                                 , Arrow     (t (Dynamic Thunk v t))
                                 , Var   Str
                                 , Unify     (t (Static  Term v t))
                                 , Acc   Str (t (Static  Term v t))
                                 , App       (t (Static  Term v t))
                                 , Cons  Str (t (Static  Term v t))
                                 , Arrow     (t (Static  Term v t))
                                 , Var       (t (Dynamic Term v t))
                                 , Unify     (t (Dynamic Term v t))
                                 , Acc       (t (Dynamic Term v t)) (t (Dynamic Term v t))
                                 , App       (t (Dynamic Term v t))
                                 , Cons      (t (Dynamic Term v t)) (t (Dynamic Term v t))
                                 , Arrow     (t (Dynamic Term v t))
                                 , Blank
                                 , Unify     (t (Static  Draft v t))
                                 , Acc   Str (t (Static  Draft v t))
                                 , App       (t (Static  Draft v t))
                                 , Cons  Str (t (Static  Draft v t))
                                 , Arrow     (t (Static  Draft v t))
                                 , Var       (t (Dynamic Draft v t))
                                 , Unify     (t (Dynamic Draft v t))
                                 , Acc       (t (Dynamic Draft v t)) (t (Dynamic Draft v t))
                                 , App       (t (Dynamic Draft v t))
                                 , Cons      (t (Dynamic Draft v t)) (t (Dynamic Draft v t))
                                 , Arrow     (t (Dynamic Draft v t))
                                 ]

type instance LayoutRelMap v t = 'Map [ {-  0 -} '( Lit           v t                                         , 'OneToMany 0  '[0     ] )
                                      , {-  1 -} '( Static  Val   v t                                         , 'OneToMany 1  '[1     ] )
                                      , {-  2 -} '( Dynamic Val   v t                                         , 'OneToMany 2  '[2     ] )
                                      , {-  3 -} '( Static  Thunk v t                                         , 'OneToMany 3  '[3     ] )
                                      , {-  4 -} '( Dynamic Thunk v t                                         , 'OneToMany 4  '[4     ] )
                                      , {-  5 -} '( Static  Term  v t                                         , 'OneToMany 5  '[5     ] )
                                      , {-  6 -} '( Dynamic Term  v t                                         , 'OneToMany 6  '[6     ] )
                                      , {-  7 -} '( Static  Draft v t                                         , 'OneToMany 7  '[7     ] )
                                      , {-  8 -} '( Dynamic Draft v t                                         , 'OneToMany 8  '[8     ] )
                                      , {-  9 -} '( Star                                                      , 'OneToMany 9  '[9 ,0  ] )
                                      , {- 10 -} '( Str                                                       , 'OneToMany 10 '[10,0  ] )
                                      , {- 11 -} '( Number                                                    , 'OneToMany 11 '[11,0  ] )
                                      , {- 12 -} '( Cons  Str (t (Static  Val   v t))                         , 'OneToMany 12 '[12,1  ] )
                                      , {- 13 -} '( Arrow     (t (Static  Val   v t))                         , 'OneToMany 13 '[13,1  ] )
                                      , {- 14 -} '( Cons      (t (Dynamic Val   v t)) (t (Dynamic Val v t))   , 'OneToMany 14 '[14,2  ] )
                                      , {- 15 -} '( Arrow     (t (Dynamic Val   v t))                         , 'OneToMany 15 '[15,2  ] )
                                      , {- 16 -} '( Acc   Str (t (Static  Thunk v t))                         , 'OneToMany 16 '[16,3  ] )
                                      , {- 17 -} '( App       (t (Static  Thunk v t))                         , 'OneToMany 17 '[17,3  ] )
                                      , {- 18 -} '( Cons  Str (t (Static  Thunk v t))                         , 'OneToMany 18 '[18,3  ] )
                                      , {- 19 -} '( Arrow     (t (Static  Thunk v t))                         , 'OneToMany 19 '[19,3  ] )
                                      , {- 20 -} '( Acc       (t (Dynamic Thunk v t)) (t (Dynamic Thunk v t)) , 'OneToMany 20 '[20,4  ] )
                                      , {- 21 -} '( App       (t (Dynamic Thunk v t))                         , 'OneToMany 21 '[21,4  ] )
                                      , {- 22 -} '( Cons      (t (Dynamic Thunk v t)) (t (Dynamic Thunk v t)) , 'OneToMany 22 '[22,4  ] )
                                      , {- 23 -} '( Arrow     (t (Dynamic Thunk v t))                         , 'OneToMany 23 '[23,4  ] )
                                      , {- 24 -} '( Var   Str                                                 , 'OneToMany 24 '[24,5,7] )
                                      , {- 25 -} '( Unify     (t (Static  Term v t))                          , 'OneToMany 25 '[25,5  ] )
                                      , {- 26 -} '( Acc   Str (t (Static  Term v t))                          , 'OneToMany 26 '[26,5  ] )
                                      , {- 27 -} '( App       (t (Static  Term v t))                          , 'OneToMany 27 '[27,5  ] )
                                      , {- 28 -} '( Cons  Str (t (Static  Term v t))                          , 'OneToMany 28 '[28,5  ] )
                                      , {- 29 -} '( Arrow     (t (Static  Term v t))                          , 'OneToMany 29 '[29,5  ] )
                                      , {- 30 -} '( Var       (t (Dynamic Term v t))                          , 'OneToMany 30 '[30,6  ] )
                                      , {- 31 -} '( Unify     (t (Dynamic Term v t))                          , 'OneToMany 31 '[31,6  ] )
                                      , {- 32 -} '( Acc       (t (Dynamic Term v t)) (t (Dynamic Term v t))   , 'OneToMany 32 '[32,6  ] )
                                      , {- 33 -} '( App       (t (Dynamic Term v t))                          , 'OneToMany 33 '[33,6  ] )
                                      , {- 34 -} '( Cons      (t (Dynamic Term v t)) (t (Dynamic Term v t))   , 'OneToMany 34 '[34,6  ] )
                                      , {- 35 -} '( Arrow     (t (Dynamic Term v t))                          , 'OneToMany 35 '[35,6  ] )
                                      , {- 36 -} '( Blank                                                     , 'OneToMany 36 '[36,7,8] )
                                      , {- 37 -} '( Unify     (t (Static  Draft v t))                         , 'OneToMany 37 '[37,7  ] )
                                      , {- 38 -} '( Acc   Str (t (Static  Draft v t))                         , 'OneToMany 38 '[38,7  ] )
                                      , {- 39 -} '( App       (t (Static  Draft v t))                         , 'OneToMany 39 '[39,7  ] )
                                      , {- 40 -} '( Cons  Str (t (Static  Draft v t))                         , 'OneToMany 40 '[40,7  ] )
                                      , {- 41 -} '( Arrow     (t (Static  Draft v t))                         , 'OneToMany 41 '[41,7  ] )
                                      , {- 42 -} '( Var       (t (Dynamic Draft v t))                         , 'OneToMany 42 '[42,8  ] )
                                      , {- 43 -} '( Unify     (t (Dynamic Draft v t))                         , 'OneToMany 43 '[43,8  ] )
                                      , {- 44 -} '( Acc       (t (Dynamic Draft v t)) (t (Dynamic Draft v t)) , 'OneToMany 44 '[44,8  ] )
                                      , {- 45 -} '( App       (t (Dynamic Draft v t))                         , 'OneToMany 45 '[45,8  ] )
                           
                                      , {- 46 -} '( Cons      (t (Dynamic Draft v t)) (t (Dynamic Draft v t)) , 'OneToMany 46 '[46,8  ] )
                                      , {- 47 -} '( Arrow     (t (Dynamic Draft v t))                         , 'OneToMany 47 '[47,8  ] )
                                      ]

#endif

-- ElemMask
type ElemMask_RULE = Mask (Size AbstractElemList)
CACHE_AS(ElemMask_RULE, ElemMask_CACHE)
type instance ElemMask = ElemMask_CACHE




-----------------------------------------------------------------------------------------
-- OLD OLD OLD OLD OLD OLD ODL OLD OLD OLD OLD OLD OLD ODL OLD OLD OLD OLD OLD OLD ODL --
-----------------------------------------------------------------------------------------



---- === Terms ===

---- Component types

---- LEGEND
---- N   - Name
---- S   - Source
---- A/P - Args / Params


---- Layout                     N S A/P
--data    Star       = Star                       deriving (Show, Eq, Ord)
--data    Arrow    t = Arrow   [t] (Map Name t) t deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Cons     t = Cons     t   [t]           deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Accessor t = Accessor t t               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    App      t = App        t [Arg t]       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype Var      t = Var      t                 deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Unify    t = Unify      t t             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Blank      = Blank                      deriving (Show, Eq, Ord)

---- Layout                     N S A/P
--data    Star2         = Star2                 deriving (Show, Eq, Ord)
--data    Arrow2      t = Arrow2      t t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Cons2     n t = Cons2     n   [t]     deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Accessor2 n t = Accessor2 n t         deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    App2        t = App2        t [Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype Var2      n t = Var2      n           deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Unify2      t = Unify2      t t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--data    Blank2        = Blank2                deriving (Show, Eq, Ord)

---- Type sets

--type DraftElems t = Blank
--                 ': TermElems t

--type TermElems  t = Var        t
--                 ': Unify      t
--                 ': ThunkElems t

--type ThunkElems t = Accessor t
--                 ': App      t
--                 ': ValElems t

--type ValElems   t = Star
--                 ': Lit
--                 ': Cons t
--                 ': Arrow t
--                 ': '[]

---- Record types

--newtype Val   t = Val   (VariantRec (Val   t)) deriving (Show)
--newtype Thunk t = Thunk (VariantRec (Thunk t)) deriving (Show)
--newtype Term  t = Term  (VariantRec (Term  t)) deriving (Show)
--newtype Draft t = Draft (VariantRec (Draft t)) deriving (Show)

--type instance Variants (Val   t) =                                     ValElems   t
--type instance Variants (Thunk t) = (Val t) ':                          ThunkElems t
--type instance Variants (Term  t) = (Val t) ': (Thunk t) ':             TermElems  t
--type instance Variants (Draft t) = (Val t) ': (Thunk t) ': (Term t) ': DraftElems t

--type HRecord a (h :: * -> *) = VariantRec (a h)


--newtype Val2   t = Val2   (HRecord Val2   t) -- deriving (Show)
--newtype Thunk2 t = Thunk2 (HRecord Thunk2 t) -- deriving (Show)
--newtype Term2  t = Term2  (HRecord Term2  t) -- deriving (Show)
--newtype Draft2 t = Draft2 (HRecord Draft2 t) -- deriving (Show)

--type instance Variants (Val2   t) =                                        ValElems   (t (Val2   t))
--type instance Variants (Thunk2 t) = (Val2 t) ':                            ThunkElems (t (Thunk2 t))
--type instance Variants (Term2  t) = (Val2 t) ': (Thunk2 t) ':              TermElems  (t (Term2  t))
--type instance Variants (Draft2 t) = (Val2 t) ': (Thunk2 t) ': (Term2 t) ': DraftElems (t (Draft2 t))



----Zamiast Coat mozemy uzyc AST ktory bedzie robil to samo co COAT i bedzie pozwalal ogladac AST. Zalety tego sa dwie
----1) ladniej i jasniej wyglada co to jest
----2) mozna generowac takie AST w passie do generowania warstw, a co za tym idzie od razu dobrze ustalac typu Draft, Term etc, tworzac
----   w przypadku Value wartosc dla literalow!

---- TODO[PM]
--instance Ord (Draft t)
--instance Eq (Draft t)
--instance Enum (Draft a) where
--    fromEnum d = case' d $ do
--        match $ \(Star     {}) -> 1
--        match $ \(Arrow    {}) -> 2
--        match $ \(Cons     {}) -> 3
--        match $ \(Accessor {}) -> 4
--        match $ \(App      {}) -> 5
--        match $ \(Var      {}) -> 6
--        match $ \(Unify    {}) -> 7
--        match $ \(Blank    {}) -> 8
-------------------------

---- Record & variant instances

--instance Record (Val   h) where mkRecord = Val
--instance Record (Thunk h) where mkRecord = Thunk
--instance Record (Term  h) where mkRecord = Term
--instance Record (Draft h) where mkRecord = Draft

--instance HasRecord (Val   t) (Val   t') where record = lens (\(Val   a) -> a) (const Val  )
--instance HasRecord (Thunk t) (Thunk t') where record = lens (\(Thunk a) -> a) (const Thunk)
--instance HasRecord (Term  t) (Term  t') where record = lens (\(Term  a) -> a) (const Term )
--instance HasRecord (Draft t) (Draft t') where record = lens (\(Draft a) -> a) (const Draft)

----

--instance Record (Val2   h) where mkRecord = Val2
--instance Record (Thunk2 h) where mkRecord = Thunk2
--instance Record (Term2  h) where mkRecord = Term2
--instance Record (Draft2 h) where mkRecord = Draft2

--instance HasRecord (Val2   t) (Val2   t') where record = lens (\(Val2   a) -> a) (const Val2  )
--instance HasRecord (Thunk2 t) (Thunk2 t') where record = lens (\(Thunk2 a) -> a) (const Thunk2)
--instance HasRecord (Term2  t) (Term2  t') where record = lens (\(Term2  a) -> a) (const Term2 )
--instance HasRecord (Draft2 t) (Draft2 t') where record = lens (\(Draft2 a) -> a) (const Draft2)


---- Name instances

--class                          m ~ Maybe            => MaybeNamedVariant v     m     a where checkVariantName :: v -> m a
--instance {-# OVERLAPPABLE #-} (a ~ t, MaybeNamed v) => MaybeNamedVariant (v t) Maybe a where checkVariantName = checkName
--instance {-# OVERLAPPABLE #-}                          MaybeNamedVariant v     Maybe a where checkVariantName = const Nothing

--instance MaybeNamed Val   where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record
--instance MaybeNamed Thunk where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record
--instance MaybeNamed Term  where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record
--instance MaybeNamed Draft where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record


----TODO[wd]: makeClassyInstances ''Cons
--instance MaybeNamed Var
--instance MaybeNamed Cons
--instance MaybeNamed Accessor
--instance HasName    Var      where name = lens (\(Var n)        -> n) (\(Var _) n         -> Var n)
--instance HasName    Cons     where name = lens (\(Cons n _)     -> n) (\(Cons _ t1) n     -> Cons n t1)
--instance HasName    Accessor where name = lens (\(Accessor n _) -> n) (\(Accessor _ t1) n -> Accessor n t1)

---- Utils intances

--instance Functor     Val   where fmap = recordMap
--instance Functor     Thunk where fmap = recordMap
--instance Functor     Term  where fmap = recordMap
--instance Functor     Draft where fmap = recordMap

--instance Foldable    Val   where foldr = recordFoldr
--instance Foldable    Thunk where foldr = recordFoldr
--instance Foldable    Term  where foldr = recordFoldr
--instance Foldable    Draft where foldr = recordFoldr

--instance Traversable Val   where traverse = recordTraverse
--instance Traversable Thunk where traverse = recordTraverse
--instance Traversable Term  where traverse = recordTraverse
--instance Traversable Draft where traverse = recordTraverse

---- === Representations ===

--instance             Repr s Blank        where repr = fromString . show
--instance             Repr s Star         where repr _              = "*"
--instance Repr s t => Repr s (Arrow    t) where repr (Arrow  p n r) = "Arrow"    <+> repr p <+> repr (Map.toList n) <+> repr r
--instance Repr s t => Repr s (Var      t) where repr (Var      n  ) = "Var"      <+> repr n
--instance Repr s t => Repr s (Unify    t) where repr (Unify    n t) = "Unify"    <+> repr n <+> repr t
--instance Repr s t => Repr s (Cons     t) where repr (Cons     n t) = "Cons"     <+> repr n <+> repr t
--instance Repr s t => Repr s (Accessor t) where repr (Accessor n t) = "Accessor" <+> repr n <+> repr t
--instance Repr s t => Repr s (App      t) where repr (App      n t) = "App"      <+> repr n <+> repr t

--instance {-# OVERLAPPING #-} VariantReprs s (Val   t) => Repr s (Val   t) where repr (Val   t) = "Val"   <+> repr t
--instance {-# OVERLAPPING #-} VariantReprs s (Thunk t) => Repr s (Thunk t) where repr (Thunk t) = "Thunk" <+> repr t
--instance {-# OVERLAPPING #-} VariantReprs s (Term  t) => Repr s (Term  t) where repr (Term  t) = "Term"  <+> repr t
--instance {-# OVERLAPPING #-} VariantReprs s (Draft t) => Repr s (Draft t) where repr (Draft t) = "Draft" <+> repr t

--instance {-# OVERLAPPING #-} VariantReprs HeaderOnly (Val   t) => Repr HeaderOnly (Val   t) where repr (Val   t) = repr t
--instance {-# OVERLAPPING #-} VariantReprs HeaderOnly (Thunk t) => Repr HeaderOnly (Thunk t) where repr (Thunk t) = repr t
--instance {-# OVERLAPPING #-} VariantReprs HeaderOnly (Term  t) => Repr HeaderOnly (Term  t) where repr (Term  t) = repr t
--instance {-# OVERLAPPING #-} VariantReprs HeaderOnly (Draft t) => Repr HeaderOnly (Draft t) where repr (Draft t) = repr t

---- HeaderOnly

--instance {-# OVERLAPPING #-} Repr HeaderOnly Star         where repr _ = "*"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (Arrow    t) where repr _ = "Arrow"
--instance {-# OVERLAPPING #-} Repr HeaderOnly Blank        where repr _ = "Blank"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (App      t) where repr _ = "App"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (Var      t) where repr a = "Var"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (Unify    t) where repr a = "Unify"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (Cons     t) where repr a = "Cons"
--instance {-# OVERLAPPING #-} Repr HeaderOnly (Accessor t) where repr a = "Accessor"


-- === Inputs ===

--inputs :: Foldable t => t a -> [a]
--inputs = foldr (:) []



-- wszystkie rzeczy nazwane powinny byc w slownikach w jezyku! - czyli datatypy ponizej zostaja,
-- ale mozemy tworzyc np. Var funkcjami, ktore oczekuja konkretnego Stringa!
--data Term h = Var      Name
--            | Cons     Name
--            | Accessor Name (Term h)
--            | App      (Term h) [Arg h]
--            | Lambda
--            | RecUpd
--            | Unify    (Term h) (Term h)
--            | Case
--            | Typed
--            -- | Assignment
--            -- x | Decons
--            | Curry
--            -- | Meta
--            -- | Tuple
--            -- | Grouped
--            -- | Decl
--            | Lit      Lit
--            | Wildcard
--            -- | Tuple
--            -- | List
--            | Unsafe [Name] (Term h)
