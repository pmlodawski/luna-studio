
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

import Prologue hiding (simple, empty, Indexable, Simple, cons, lookup, index, children, Cons, Ixed, Repr, repr, minBound, maxBound, (#))
import Type.Container

import Luna.Syntax.AST.Layout (ByLayout, SubLayouts, SubSemiLayouts)
import qualified Luna.Syntax.AST.Layout as Layout
--import Data.Bits.Mask         (Mask)
import GHC.Prim            (Any, unsafeCoerce#)
import Data.Int            (Int64)

import Unsafe.Coerce (unsafeCoerce)
import Data.Relation.Binary
import qualified Data.Relation.Binary as Rel


import Data.Bits (Bits, FiniteBits, setBit, testBit, zeroBits, finiteBitSize)

import Type.Map

import Data.Foldable (foldl', foldr')
import Control.Monad.State hiding (when, withState)

import Data.Maybe (isNothing, catMaybes)
import Data.Base
import Type.Bool
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

type instance Unlayered (Lit     v t) = ASTRecord '[]                                                      LitVariants          WithoutValue v t
type instance Unlayered (Val   l v t) = ASTRecord (Lit v t ': SubGroups l v t '[Val]                    ) (ValVariants   l v t) WithValue    v t
type instance Unlayered (Thunk l v t) = ASTRecord (Lit v t ': SubGroups l v t '[Val, Thunk]             ) (ThunkVariants l v t) WithoutValue v t
type instance Unlayered (Term  l v t) = ASTRecord (Lit v t ': SubGroups l v t '[Val, Thunk, Term]       ) (TermVariants  l v t) WithoutValue v t
type instance Unlayered (Draft l v t) = ASTRecord (Lit v t ': SubGroups l v t '[Val, Thunk, Term, Draft]) (DraftVariants l v t) WithoutValue v t


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

type instance RecordOf (Lit     v t) = RecordOf (Unlayered (Lit     v t))
type instance RecordOf (Val   l v t) = RecordOf (Unlayered (Val   l v t))
type instance RecordOf (Thunk l v t) = RecordOf (Unlayered (Thunk l v t))
type instance RecordOf (Term  l v t) = RecordOf (Unlayered (Term  l v t))
type instance RecordOf (Draft l v t) = RecordOf (Unlayered (Draft l v t))

instance IsRecord (Lit     v t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   l v t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk l v t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Term  l v t) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft l v t) where asRecord = wrapped' ∘ asRecord

-----------------------
-----------------------
-----------------------

-- === Data Layouts ===

type family Static  (a :: * -> * -> (* -> *) -> *) :: (* -> (* -> *) -> *) where Static  a = a Layout.Static
type family Dynamic (a :: * -> * -> (* -> *) -> *) :: (* -> (* -> *) -> *) where Dynamic a = a Layout.Dynamic

type ByLayout' l d = ByLayout l Str d

type family ApplyLayouts ls a v (t :: * -> *) where ApplyLayouts '[]       a v t = '[]
                                                    ApplyLayouts (l ': ls) a v t = a l v t ': ApplyLayouts ls a v t

type ApplySubLayouts     l a v (t :: * -> *) = ApplyLayouts (SubLayouts     l) a v t
type ApplySubSemiLayouts l a v (t :: * -> *) = ApplyLayouts (SubSemiLayouts l) a v t


type family SubGroups layout v (t :: * -> *) gs where
  SubGroups l v t '[]       = '[]
  SubGroups l v t '[g]      = ApplySubLayouts     l g v t
  SubGroups l v t (g ': gs) = ApplySubSemiLayouts l g v t <> SubGroups l v t gs



--------------------------------------
-- === Data related type caches === --
--------------------------------------

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

type family VariantList (v :: *) (t :: * -> *) :: [*]
--type family LayoutRelMap (v :: *) (t :: * -> *) :: Map * (Birelation Nat)

--type Layout = (Map * (Birelation Nat) :: Box)
--type family Layout a :: Map * (Birelation Nat)

type instance VariantList v t = 
        '[ Star
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

type MyLayout v t = 
    'Map [ {-  0 -} '( Lit           v t                                         , 'OneToMany 0  '[0     ] )
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

-- TODO [WD]: generalize m to PolyKinds
newtype Modal (m :: ValueMod) a = Modal a deriving (Functor, Traversable, Foldable)

instance Show (Modal m a) where show _ = "Modal" -- FIXME[WD]: add show when possible


-- === Basic mods === --

data ValueMod = WithValue
              | WithoutValue
              deriving (Show)

class                                   ValueReader a m             v where readValue :: a -> Modal m v
instance                                ValueReader a 'WithoutValue v where readValue _ = Modal $ unsafeCoerce NOP ; {-# INLINE readValue #-}
instance (HasValue a, v ~ ValueOf a) => ValueReader a 'WithValue    v where readValue   = Modal ∘ view value       ; {-# INLINE readValue #-}



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

newtype Record (layout :: Map * (Birelation Nat)) (groups :: [*]) (variants :: [*]) rec = Record rec deriving (Show, Functor, Traversable, Foldable)

class IsRecord a where asRecord :: forall gs vs. Iso' a (RecordOf a)

type family RecordOf a :: *
type family Variants a :: [*]
type family Groups   a :: [*]


-- === Variant Provider === --

class                                                  VariantProvider a                vnt | a -> vnt where providedVariant :: a -> vnt
instance {-# OVERLAPPABLE #-} a ~ vnt               => VariantProvider a                vnt            where providedVariant = id                        ; {-# INLINE providedVariant #-}
instance {-# OVERLAPPABLE #-} VariantProvider a vnt => VariantProvider (Attached t d a) vnt            where providedVariant = providedVariant ∘ unlayer ; {-# INLINE providedVariant #-}

-- === LayoutMask Provider === --

class                                                   LayoutMaskProvider l a                  where providedMask :: LayoutProxy l -> a -> Mask
instance {-# OVERLAPPABLE #-}                           LayoutMaskProvider l a                  where providedMask _ _ = zeroBits                 ; {-# INLINE providedMask #-}
instance {-# OVERLAPPABLE #-} LayoutMaskProvider l a => LayoutMaskProvider l (Attached t d a)   where providedMask l   = providedMask l ∘ unlayer ; {-# INLINE providedMask #-}


-- === Instances === --

type instance Variants (Record l gs vs rec) = vs
type instance Groups   (Record l gs vs rec) = gs



---------------------------------
-- === AST Data definition === --
---------------------------------

newtype Mask = Mask Int64 deriving (Eq, Num, Bits, FiniteBits)

instance Show Mask where 
    show m = show (catMaybes (testBit' m <$> [0 .. finiteBitSize m - 1])) where
        testBit' m b = if testBit m b then Just b else Nothing


-- === Types === --

data Data (m :: ValueMod) v = Data { _mask    :: !Mask
                                   , _value   :: !(Modal m v)
                                   , _variant :: !Store
                                   } deriving (Show)

newtype ASTRecord (groups :: [*]) (variants :: [*]) (m :: ValueMod) v (t :: * -> *) = ASTRecord (Unwrapped (ASTRecord groups variants m v t)) deriving (Show)
type instance Unlayered (ASTRecord gs vs m v t) = Record (MyLayout v t) gs vs (Data m v)

-- === Instances === --

type instance Variants (ASTRecord gs vs m v t) = Variants (Unwrapped (ASTRecord gs vs m v t))
type instance Groups   (ASTRecord gs vs m v t) = Groups   (Unwrapped (ASTRecord gs vs m v t))
type instance RecordOf (ASTRecord gs vs m v t) = Unwrapped (ASTRecord gs vs m v t)
instance      IsRecord (ASTRecord gs vs m v t) where asRecord = wrapped' ; {-# INLINE asRecord #-}

-- Wrappers

instance Layered   (ASTRecord gs vs m v t)
instance Rewrapped (ASTRecord gs vs m v t) (ASTRecord l' gs' vs' m' v')
instance Wrapped   (ASTRecord gs vs m v t) where
    type Unwrapped (ASTRecord gs vs m v t) = Unlayered (ASTRecord gs vs m v t)
    _Wrapped' = iso (\(ASTRecord a) -> a) ASTRecord
    {-# INLINE _Wrapped' #-}

-- MaskProviders

instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (RecordOf (Lit     v t)) => LayoutMaskProvider l (Lit     v t) where providedMask l = providedMask l ∘ view asRecord
instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (RecordOf (Val   x v t)) => LayoutMaskProvider l (Val   x v t) where providedMask l = providedMask l ∘ view asRecord
instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (RecordOf (Thunk x v t)) => LayoutMaskProvider l (Thunk x v t) where providedMask l = providedMask l ∘ view asRecord
instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (RecordOf (Term  x v t)) => LayoutMaskProvider l (Term  x v t) where providedMask l = providedMask l ∘ view asRecord
instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (RecordOf (Draft x v t)) => LayoutMaskProvider l (Draft x v t) where providedMask l = providedMask l ∘ view asRecord

instance {-# OVERLAPPABLE #-} LayoutMaskProvider l (Record l gs vs (Data m v)) where providedMask _ (Record (Data m _ _)) = m ; {-# INLINE providedMask #-}


-- === AST Data encoder === --

instance ( rel  ~ MapLookup vnt layout
         , bits ~ Rel.Targets rel
         , KnownNats bits
         , VariantProvider a vnt
         , LayoutMaskProvider layout vnt
         , ValueReader     a m v
         ) => RecEncoder a layout gs vs (Data m v) where 
    encodeRec v = Record $ Data mask' val $ unsafeStore v where
        bits    = fromIntegral <$> natVals (Proxy :: Proxy bits)
        mask    = providedMask (Proxy :: Proxy layout) vnt
        mask'   = foldl' setBit mask bits
        val     = readValue       v
        vnt     = providedVariant v
    {-# INLINE encodeRec #-}





--------------------------
-- === Construction === --
--------------------------

-- === Abstraction === --

class RecCons v rec where recCons :: v -> rec
instance ( IsRecord rec
         , RecordOf rec ~ Record layout gs vs d
         , RecEncoder v layout gs vs d
         ) => RecCons v rec where recCons = view (from asRecord) ∘ encodeRec ; {-# INLINE recCons #-}
instance      RecCons I rec where recCons = impossible                       ; {-# INLINE recCons #-}
instance      RecCons v I   where recCons = impossible                       ; {-# INLINE recCons #-}


-- === Constructions encoders === --

class RecEncoder v layout gs vs rec where encodeRec :: v -> Record layout gs vs rec


-- - zrobic uncheckedRec i checkedRec
-- - zrobic resolution typow przy konstruktorach i pattern matchach
-- - zrobic konstruktory typow zlozonych
-- - zrobic ladne typy pattern matchingu


------------------------------
-- === Pattern matching === --
------------------------------

class LayoutMatch layout v gs vs rec where layoutMatch :: forall a. (v -> a) -> Record layout gs vs rec -> Maybe a

instance ( rel ~ MapLookup a layout
         , nat ~ Rel.Source rel
         , KnownNat nat
         ) => LayoutMatch layout a gs vs (Data m v) where
    layoutMatch f (Record (Data mask val v)) = if match then Just (f val) else Nothing where
        bit   = fromIntegral $ natVal (Proxy :: Proxy nat)
        match = testBit mask bit
        val   = unsafeRestore v :: a
    {-# INLINE layoutMatch #-}


runMatch :: (LayoutMatch l v gs vs rec, IsRecord r, RecordOf r ~ Record l gs vs rec) => (v -> a) -> r -> Maybe a
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

star' :: ASTRecord '[] '[] WithValue Int IDT
star' = recCons $ Star +> 5

star :: Lit v t
star = recCons Star

caseTest = __case__ "tc-test" "test/Main.hs" 0
{-# INLINE caseTest #-}

data Test a b = Test !a !b  deriving (Show)

main = do
    let v  = star :: Lit Int IDT
        v' = v +> (5 :: Int)
        l  = recCons v' :: Static Thunk Int IDT
        --l2 = recCons l  :: Dynamic Val Int IDT
        l2 = recCons l  :: Dynamic Thunk Int IDT

    print v
    print l
    print l2

    print $ caseTest v $ do
        match (\Star -> (1 :: Int))

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
