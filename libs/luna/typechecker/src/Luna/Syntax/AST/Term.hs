{-# LANGUAGE CPP                    #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FunctionalDependencies #-}

module Luna.Syntax.AST.Term where

import Prologue hiding (Cons, Swapped)

import Data.Base
import Data.Record
import Luna.Syntax.AST.Layout (SubLayouts, SubSemiLayouts, ToStatic, ToDynamic, ByLayout)
import Type.Container
import Type.Cache.TH          (cacheHelper, cacheType)
import Type.Map
import Data.Abstract

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
#define CACHE_AS(n,cn) cacheHelper ''n (Just cn); cacheType ''n (Just cn)
#define CHECK_EQ(s,t)  assertTypesEq (Proxy :: Proxy (s)) (Proxy :: Proxy (t))


-- TODO[WD]: move to issue tracker after releasing Luna to github

--------------------------------------------
-- === Enhancement proposals & issues === --
--------------------------------------------

-- Status: pending | accepted | rejected

-- Reporter  Status   Description
-- wdanilo   pending  ACCESSORS AND FUNCTIONS UNIFICATION
--                    Check if we can throw away accessors in terms. Let's consider the following Luna code:
--                        a  = x.bar
--                        a' = acc x "bar"
--                    These lines should mean exactly the same with the followings rules:
--                        - both forms have to be distinguishable to provide AST <-> Text conversion
--                        - the performance of STATIC Lun should be as fast as in current solution
--                        - accessors should be first class objects, althought we can easily make a workaround like `myacc = a : a.x`



------------------------
-- === Properties === --
------------------------

type family Name   a
type family Source a
type family Target a
type family Args   a

class HasName   a where name   :: Lens' a (Name   a)
class HasSource a where source :: Lens' a (Source a)
class HasTarget a where target :: Lens' a (Target a)
class HasArgs   a where args   :: Lens' a (Args   a)


-----------------------------
-- === Component types === --
-----------------------------

newtype Arg a = Arg a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data    Star   = Star          deriving (Show, Eq, Ord)
newtype Str    = Str    String deriving (Show, Eq, Ord)
newtype Number = Number Int    deriving (Show, Eq, Ord)


-- LEGEND
--   N   - Name
--   S   - Source
--   A/P - Args / Params

-- Layout                  N  S  A/P
newtype Var   n   = Var    n             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype Cons  n   = Cons   n             deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Arrow   t = Arrow    !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Acc   n t = Acc   !n !t          deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    App     t = App      !t ![Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Unify   t = Unify    !t !t       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Blank     = Blank                deriving (Show, Eq, Ord)


----type family AppNT
----newtype NT a n t = NT (a

--newtype Flipped a t1 t2 = Flipped (a t2 t1) deriving (Show, Eq, Ord)

--instance Functor  (Flipped Acc t) where fmap  f   (Flipped (Acc n t)) = Flipped $ Acc (f n) t
--instance Foldable (Flipped Acc t) where foldr f b (Flipped (Acc n t)) = f n b

----data family NT a n t
----newtype instance NT (Var   n  ) n t = NT_Var   (Var   n  ) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
----newtype instance NT (Cons  n  ) n t = NT_Cons  (Cons  n  ) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
----newtype instance NT (Arrow   t) n t = NT_Arrow (Arrow   t) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

----type family NT a

----type instance NT (Var   n  ) = NT_Var   n t
----type instance NT (Cons  n  ) = NT_Cons  n t
----type instance NT (Arrow   t) = NT_Arrow n t
----type instance NT (Acc   n t) = NT_Acc   n t
----type instance NT (App     t) = NT_App   n t
----type instance NT (Unify   t) = NT_Unify n t
----type instance NT Blank       = NT_Blank n t

--newtype NT_Var   n t = NT_Var   (Var   n  ) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_Cons  n t = NT_Cons  (Cons  n  ) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_Arrow n t = NT_Arrow (Arrow   t) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_Acc   n t = NT_Acc   (Acc   n t) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_App   n t = NT_App   (App     t) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_Unify n t = NT_Unify (Unify   t) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
--newtype NT_Blank n t = NT_Blank Blank       deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


--instance Rewrapped (NT_Var   n t) (NT_Var   n' t')
--instance Wrapped   (NT_Var   n t) where
--    type Unwrapped (NT_Var   n t) = Var n
--    _Wrapped' = iso (\(NT_Var a) -> a) NT_Var

--instance Rewrapped (NT_Cons  n t) (NT_Cons  n' t')
--instance Wrapped   (NT_Cons  n t) where
--    type Unwrapped (NT_Cons  n t) = Cons n
--    _Wrapped' = iso (\(NT_Cons a) -> a) NT_Cons

--instance Rewrapped (NT_Arrow n t) (NT_Arrow n' t')
--instance Wrapped   (NT_Arrow n t) where
--    type Unwrapped (NT_Arrow n t) = Arrow t
--    _Wrapped' = iso (\(NT_Arrow a) -> a) NT_Arrow

--instance Rewrapped (NT_Acc   n t) (NT_Acc   n' t')
--instance Wrapped   (NT_Acc   n t) where
--    type Unwrapped (NT_Acc   n t) = Acc n t
--    _Wrapped' = iso (\(NT_Acc a) -> a) NT_Acc

--instance Rewrapped (NT_App   n t) (NT_App   n' t')
--instance Wrapped   (NT_App   n t) where
--    type Unwrapped (NT_App   n t) = App t
--    _Wrapped' = iso (\(NT_App a) -> a) NT_App

--instance Rewrapped (NT_Unify n t) (NT_Unify n' t')
--instance Wrapped   (NT_Unify n t) where
--    type Unwrapped (NT_Unify n t) = Unify t
--    _Wrapped' = iso (\(NT_Unify a) -> a) NT_Unify

--instance Rewrapped (NT_Blank n t) (NT_Blank n' t')
--instance Wrapped   (NT_Blank n t) where
--    type Unwrapped (NT_Blank n t) = Blank
--    _Wrapped' = iso (\(NT_Blank a) -> a) NT_Blank


--class IsNT a b n t | a -> b, b n t -> a where nt :: Iso' a (b n t)

--instance IsNT (Var   n  ) NT_Var   n t where nt = from wrapped'
--instance IsNT (Cons  n  ) NT_Cons  n t where nt = from wrapped'
--instance IsNT (Arrow   t) NT_Arrow n t where nt = from wrapped'
--instance IsNT (Acc   n t) NT_Acc   n t where nt = from wrapped'
--instance IsNT (App     t) NT_App   n t where nt = from wrapped'
--instance IsNT (Unify   t) NT_Unify n t where nt = from wrapped'
--instance IsNT Blank       NT_Blank n t where nt = from wrapped'

----testme :: _ => _
----testme f a = view (from nt) $ f <$> a ^. nt

----foldr :: (a -> b -> b) -> b -> t a -> b

----newtype NT_Var n t = NT_Var (Var n) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
----newtype TN_Acc t n = TN_Acc (Acc n t) deriving (Show, Eq, Ord, Functor)



-- === Helpers === --

-- FIXME[WD]: poprawic typ oraz `WithElement_` (!)
inputs :: forall r (t :: * -> *) a. WithElement_ (TFoldable (t (r t))) (r t) => r t -> [t (r t)]
inputs r = withElement_ (p :: P (TFoldable (t (r t)))) (foldrT ((:) :: t (r t) -> [t (r t)] -> [t (r t)]) []) r


-- NFunctor and TFunctor allow mapping components over the `n` and `t` param type respectively.
class NFunctor n m a a' | n m a -> a' where fmapN :: (n -> m) -> a -> a'
class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'


class NFoldable a t where foldrN :: (a -> b -> b) -> b -> t -> b
class TFoldable a t where foldrT :: (a -> b -> b) -> b -> t -> b

--instance n ~ n' => NFoldable n (Var n') where foldrN = foldr

instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Acc n t') where foldrT = foldr
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (App   t') where foldrT = foldr
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Unify t') where foldrT = foldr
instance {-# OVERLAPPABLE #-} t ~ t' => TFoldable t (Arrow t') where foldrT = foldr
instance {-# OVERLAPPABLE #-} TFoldable t (Var  n) where foldrT _ = const
instance {-# OVERLAPPABLE #-} TFoldable t (Cons n) where foldrT _ = const
instance {-# OVERLAPPABLE #-} TFoldable t Blank    where foldrT _ = const




-- === Instances ===

-- Wrappers

instance Rewrapped (Var n) (Var n')
instance Wrapped   (Var n) where
  type   Unwrapped (Var n) = n
  _Wrapped' = iso (\(Var n) -> n) Var

instance Rewrapped (Cons n) (Cons n')
instance Wrapped   (Cons n) where
  type   Unwrapped (Cons n) = n
  _Wrapped' = iso (\(Cons n) -> n) Cons

-- Properties

type instance Name   (Var   n  ) = n
type instance Name   (Cons  n  ) = n
type instance Name   (Acc   n t) = n
type instance Source (Arrow   t) = t
type instance Source (Acc   n t) = t
type instance Source (App     t) = t
type instance Source (Unify   t) = t
type instance Target (Arrow   t) = t
type instance Target (Unify   t) = t
type instance Args   (App     t) = [Arg t]

instance HasName   (Var   n  ) where name   = wrapped'                                               ; {-# INLINE name   #-}
instance HasName   (Cons  n  ) where name   = wrapped'                                               ; {-# INLINE name   #-}
instance HasName   (Acc   n t) where name   = lens (\(Acc   n _) -> n) (\(Acc   _ t) n -> Acc   n t) ; {-# INLINE name   #-}
instance HasSource (Arrow   t) where source = lens (\(Arrow s _) -> s) (\(Arrow _ t) s -> Arrow s t) ; {-# INLINE source #-}
instance HasSource (Acc   n t) where source = lens (\(Acc   _ s) -> s) (\(Acc   n _) s -> Acc   n s) ; {-# INLINE source #-}
instance HasSource (App     t) where source = lens (\(App   s _) -> s) (\(App   _ a) s -> App   s a) ; {-# INLINE source #-}
instance HasSource (Unify   t) where source = lens (\(Unify s _) -> s) (\(Unify _ t) s -> Unify s t) ; {-# INLINE source #-}
instance HasTarget (Arrow   t) where target = lens (\(Arrow _ t) -> t) (\(Arrow s _) t -> Arrow s t) ; {-# INLINE target #-}
instance HasTarget (Unify   t) where target = lens (\(Unify _ t) -> t) (\(Unify s _) t -> Unify s t) ; {-# INLINE target #-}
instance HasArgs   (App     t) where args   = lens (\(App   _ a) -> a) (\(App   s _) a -> App   s a) ; {-# INLINE args   #-}

-- Mapping

instance n ~ n' => NFunctor n m (Var   n'  ) (Var   m  ) where fmapN = (wrapped %~)            ; {-# INLINE fmapN #-}
instance n ~ n' => NFunctor n m (Cons  n'  ) (Cons  m  ) where fmapN = (wrapped %~)            ; {-# INLINE fmapN #-}
instance n ~ n' => NFunctor n m (Acc   n' t) (Acc   m t) where fmapN f (Acc n t) = Acc (f n) t ; {-# INLINE fmapN #-}
instance           NFunctor n m (Arrow    t) (Arrow   t) where fmapN = flip const              ; {-# INLINE fmapN #-}
instance           NFunctor n m (App      t) (App     t) where fmapN = flip const              ; {-# INLINE fmapN #-}
instance           NFunctor n m (Unify    t) (Unify   t) where fmapN = flip const              ; {-# INLINE fmapN #-}
instance           NFunctor n m Blank        Blank       where fmapN = flip const              ; {-# INLINE fmapN #-}

instance t ~ t' => TFunctor t r (Arrow   t') (Arrow   r) where fmapT = fmap                    ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Acc   n t') (Acc   n r) where fmapT = fmap                    ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (App     t') (App     r) where fmapT = fmap                    ; {-# INLINE fmapT #-}
instance t ~ t' => TFunctor t r (Unify   t') (Unify   r) where fmapT = fmap                    ; {-# INLINE fmapT #-}
instance           TFunctor t r (Var   n   ) (Var   n  ) where fmapT = flip const              ; {-# INLINE fmapT #-}
instance           TFunctor t r (Cons  n   ) (Cons  n  ) where fmapT = flip const              ; {-# INLINE fmapT #-}
instance           TFunctor t r Blank        Blank       where fmapT = flip const              ; {-# INLINE fmapT #-}


-----------------------------
-- === Term groups v 2 === --
-----------------------------

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- TODO: Rethink the following AST definition. It allows for custom data layouts,
--       that could be of any form, not only something like `t (ast t)`, which would
--       help create much more readable and user-friendly type system on top of AST.
--       Additional it owuld be much more feasible for flat AST layouts as well as
--       breadcrumbs definitions.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

--data family AST t term runtime -- `t` could be something like `Graph` or `Breadcrumb`

--data Lit'   = Lit'   deriving (Show)
--data Val'   = Val'   deriving (Show)
--data Thunk' = Thunk' deriving (Show)
--data Term'  = Term'  deriving (Show)
--data Draft' = Draft' deriving (Show)

--newtype instance AST t Lit'   rt = AST_Lit   (Unlayered (AST t Lit'   rt))
--newtype instance AST t Val'   rt = AST_Val   (Unlayered (AST t Val'   rt))
--newtype instance AST t Thunk' rt = AST_Thunk (Unlayered (AST t Thunk' rt))
--newtype instance AST t Term'  rt = AST_Term  (Unlayered (AST t Term'  rt))
--newtype instance AST t Draft' rt = AST_Draft (Unlayered (AST t Draft' rt))

---- ...

---- | Data layout for a particular `t` like `Graph` or `Breadcrumb`
--type family Layout' t ast


--type LitVariants2       = LitElems
--type ValVariants2   l t = ValElems   (NameByRuntime rt (Layout t (AST )) (ValRepr t l))



-------------------------
-- === Term groups === --
-------------------------

newtype Lit                 (t :: * -> *) = Lit   (Unlayered (Lit          t))
newtype Val   (layout :: *) (t :: * -> *) = Val   (Unlayered (Val   layout t))
newtype Thunk (layout :: *) (t :: * -> *) = Thunk (Unlayered (Thunk layout t))
newtype Term  (layout :: *) (t :: * -> *) = Term  (Unlayered (Term  layout t))
newtype Draft (layout :: *) (t :: * -> *) = Draft (Unlayered (Draft layout t))

type LitElems       = Star
                   ': Str
                   ': Number
                   ': '[]

type ValElems   n t = Cons       n
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
type ValVariants   l t = ValElems   (NameByLayout l (t (Val   l t))) (t (Val   l t))
type ThunkVariants l t = ThunkElems (NameByLayout l (t (Thunk l t))) (t (Thunk l t))
type TermVariants  l t = TermElems  (NameByLayout l (t (Term  l t))) (t (Term  l t))
type DraftVariants l t = DraftElems (NameByLayout l (t (Draft l t))) (t (Draft l t))

type instance Unlayered (Lit     t) = ASTRecord '[]                                                        LitVariants        t Data
type instance Unlayered (Val   l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val]                    ) (ValVariants   l t) t Data
type instance Unlayered (Thunk l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk]             ) (ThunkVariants l t) t Data
type instance Unlayered (Term  l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk, Term]       ) (TermVariants  l t) t Data
type instance Unlayered (Draft l t) = ASTRecord (Lit t ': SubLayoutGroups l t '[Val, Thunk, Term, Draft]) (DraftVariants l t) t Data


-- === Syntax Layouts === --

type family Static  (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Static  a = a Layout.Static
type family Dynamic (a :: * -> (* -> *) -> *) :: ((* -> *) -> *) where Dynamic a = a Layout.Dynamic

type ApplySubLayouts     l  a (t :: * -> *) = ApplyLayouts (SubLayouts     l) a t
type ApplySubSemiLayouts l  a (t :: * -> *) = ApplyLayouts (SubSemiLayouts l) a t
type family ApplyLayouts ls a (t :: * -> *) where ApplyLayouts '[]       a t = '[]
                                                  ApplyLayouts (l ': ls) a t = a l t ': ApplyLayouts ls a t

type family SubLayoutGroups layout (t :: * -> *) gs where
  SubLayoutGroups l t '[]       = '[]
  SubLayoutGroups l t '[g]      = ApplySubLayouts     l g t
  SubLayoutGroups l t (g ': gs) = ApplySubSemiLayouts l g t <> SubLayoutGroups l t gs

type NameByLayout l d = ByLayout l Str d

-- === Instances === --

-- Show

deriving instance Show (Lit     t)
deriving instance Show (Val   l t)
deriving instance Show (Thunk l t)
deriving instance Show (Term  l t)
deriving instance Show (Draft l t)

-- Eq

deriving instance Eq (Lit     t)
deriving instance Eq (Val   l t)
deriving instance Eq (Thunk l t)
deriving instance Eq (Term  l t)
deriving instance Eq (Draft l t)

-- Bases

type instance Base Star        = Proxy Star
type instance Base Str         = Proxy Str
type instance Base Number      = Proxy Number

type instance Base (Arrow   t) = Proxy Arrow
type instance Base (Cons  n  ) = Proxy Cons
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

instance Layered   (Lit t)
instance Rewrapped (Lit t) (Lit t')
instance Wrapped   (Lit t) where
    type Unwrapped (Lit t) = Unlayered (Lit t)
    _Wrapped' = iso (\(Lit a) -> a) Lit ; {-# INLINE _Wrapped' #-}

instance Layered   (Val l t)
instance Rewrapped (Val l t) (Val l' t')
instance Wrapped   (Val l t) where
    type Unwrapped (Val l t) = Unlayered (Val l t)
    _Wrapped' = iso (\(Val a) -> a) Val ; {-# INLINE _Wrapped' #-}

instance Layered   (Thunk l t)
instance Rewrapped (Thunk l t) (Thunk l' t')
instance Wrapped   (Thunk l t) where
    type Unwrapped (Thunk l t) = Unlayered (Thunk l t)
    _Wrapped' = iso (\(Thunk a) -> a) Thunk ; {-# INLINE _Wrapped' #-}

instance Layered   (Term l t)
instance Rewrapped (Term l t) (Term l' t')
instance Wrapped   (Term l t) where
    type Unwrapped (Term l t) = Unlayered (Term l t)
    _Wrapped' = iso (\(Term a) -> a) Term ; {-# INLINE _Wrapped' #-}

instance Layered   (Draft l t)
instance Rewrapped (Draft l t) (Draft l' t')
instance Wrapped   (Draft l t) where
    type Unwrapped (Draft l t) = Unlayered (Draft l t)
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

-- Conversions

instance (ASTRecord gs vs rt d ~ Unwrapped (Lit     t)) => Convertible (Lit     t) (ASTRecord gs vs rt d) where convert = unwrap'
instance (ASTRecord gs vs rt d ~ Unwrapped (Val   l t)) => Convertible (Val   l t) (ASTRecord gs vs rt d) where convert = unwrap'
instance (ASTRecord gs vs rt d ~ Unwrapped (Thunk l t)) => Convertible (Thunk l t) (ASTRecord gs vs rt d) where convert = unwrap'
instance (ASTRecord gs vs rt d ~ Unwrapped (Term  l t)) => Convertible (Term  l t) (ASTRecord gs vs rt d) where convert = unwrap'
instance (ASTRecord gs vs rt d ~ Unwrapped (Draft l t)) => Convertible (Draft l t) (ASTRecord gs vs rt d) where convert = unwrap'

instance Castable (Lit     t) Data
instance Castable (Val   l t) Data
instance Castable (Thunk l t) Data
instance Castable (Term  l t) Data
instance Castable (Draft l t) Data

instance Convertible (Lit     t) Data where convert = convert ∘ unwrap'
instance Convertible (Val   l t) Data where convert = convert ∘ unwrap'
instance Convertible (Thunk l t) Data where convert = convert ∘ unwrap'
instance Convertible (Term  l t) Data where convert = convert ∘ unwrap'
instance Convertible (Draft l t) Data where convert = convert ∘ unwrap'

instance Castable Data (Lit     t) where cast = wrap' ∘ cast
instance Castable Data (Val   l t) where cast = wrap' ∘ cast
instance Castable Data (Thunk l t) where cast = wrap' ∘ cast
instance Castable Data (Term  l t) where cast = wrap' ∘ cast
instance Castable Data (Draft l t) where cast = wrap' ∘ cast

-- Abstractions

type instance Abstract (Lit     t) = Data
type instance Abstract (Val   l t) = Data
type instance Abstract (Thunk l t) = Data
type instance Abstract (Term  l t) = Data
type instance Abstract (Draft l t) = Data

instance HasAbstract (Lit     t)
instance HasAbstract (Val   l t)
instance HasAbstract (Thunk l t)
instance HasAbstract (Term  l t)
instance HasAbstract (Draft l t)

instance IsAbstract (Lit     t) where abstracted = iso cast cast
instance IsAbstract (Val   l t) where abstracted = iso cast cast
instance IsAbstract (Thunk l t) where abstracted = iso cast cast
instance IsAbstract (Term  l t) where abstracted = iso cast cast
instance IsAbstract (Draft l t) where abstracted = iso cast cast


------------------------------------
-- === AST Layout type caches === --
------------------------------------

-- The following code is result of type-families expressions and is cached in order to speed-up the compilation process.
-- related GHC bug:        https://ghc.haskell.org/trac/ghc/ticket/8095#no1
-- related IRC discussion: http://pastebin.com/9PH7TPB9

-- | All possible groups and variants stored as single 64-bit mask:
-- |   - 9  bits for groups
-- |   - 36 bits for variants
-- |   - 19 bits free for further extensions

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
                                  , {- 12 -} Cons  Str
                                  , {- 13 -} Arrow     (t (Static  Val   t))
                                  , {- 14 -} Cons      (t (Dynamic Val   t))
                                  , {- 15 -} Arrow     (t (Dynamic Val   t))
                                  , {- 16 -} Acc   Str (t (Static  Thunk t))
                                  , {- 17 -} App       (t (Static  Thunk t))
                                  , {- 18 -} Arrow     (t (Static  Thunk t))
                                  , {- 19 -} Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t))
                                  , {- 20 -} App       (t (Dynamic Thunk t))
                                  , {- 21 -} Cons      (t (Dynamic Thunk t))
                                  , {- 22 -} Arrow     (t (Dynamic Thunk t))
                                  , {- 23 -} Var   Str
                                  , {- 24 -} Unify     (t (Static  Term t))
                                  , {- 25 -} Acc   Str (t (Static  Term t))
                                  , {- 26 -} App       (t (Static  Term t))
                                  , {- 27 -} Arrow     (t (Static  Term t))
                                  , {- 28 -} Var       (t (Dynamic Term t))
                                  , {- 29 -} Unify     (t (Dynamic Term t))
                                  , {- 30 -} Acc       (t (Dynamic Term t)) (t (Dynamic Term t))
                                  , {- 31 -} App       (t (Dynamic Term t))
                                  , {- 32 -} Cons      (t (Dynamic Term t))
                                  , {- 33 -} Arrow     (t (Dynamic Term t))
                                  , {- 34 -} Blank
                                  , {- 35 -} Unify     (t (Static  Draft t))
                                  , {- 36 -} Acc   Str (t (Static  Draft t))
                                  , {- 37 -} App       (t (Static  Draft t))
                                  , {- 38 -} Arrow     (t (Static  Draft t))
                                  , {- 39 -} Var       (t (Dynamic Draft t))
                                  , {- 40 -} Unify     (t (Dynamic Draft t))
                                  , {- 41 -} Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t))
                                  , {- 42 -} App       (t (Dynamic Draft t))
                                  , {- 43 -} Cons      (t (Dynamic Draft t))
                                  , {- 44 -} Arrow     (t (Dynamic Draft t))
                                  ]

#ifndef CachedTypeFamilies

type VariantList_RULE t = Unique (GatherProps Variant (GroupList t))
CACHE_AS(VariantList_RULE, "VariantList_GEN_CACHE")
CHECK_EQ(VariantList_GEN_CACHE IM, VariantList_MANUAL_CACHE IM)
type VariantList_CACHE t = VariantList_GEN_CACHE t

#else

type VariantList_CACHE t = VariantList_MANUAL_CACHE t

#endif

type VariantList t = VariantList_CACHE t

-- Layout

type Layout_RULE t = GroupList t <> VariantList t
CACHE_AS(Layout_RULE, "Layout_CACHE")

type instance Layout (ASTRecord gs vs t d) = Layout_CACHE t

type instance Layout2 Variant (ASTRecord gs vs t d) = VariantList t

-- === DecodeMap === --

type DecodeMap_MANUAL_CACHE t =
    'Map [ {-  0 -} '( Lit           t                                       ,  0 )
         , {-  1 -} '( Static  Val   t                                       ,  1 )
         , {-  2 -} '( Dynamic Val   t                                       ,  2 )
         , {-  3 -} '( Static  Thunk t                                       ,  3 )
         , {-  4 -} '( Dynamic Thunk t                                       ,  4 )
         , {-  5 -} '( Static  Term  t                                       ,  5 )
         , {-  6 -} '( Dynamic Term  t                                       ,  6 )
         , {-  7 -} '( Static  Draft t                                       ,  7 )
         , {-  8 -} '( Dynamic Draft t                                       ,  8 )
         , {-  9 -} '( Star                                                  ,  9 )
         , {- 10 -} '( Str                                                   , 10 )
         , {- 11 -} '( Number                                                , 11 )
         , {- 12 -} '( Cons  Str                                             , 12 )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                       , 13 )
         , {- 14 -} '( Cons      (t (Dynamic Val   t))                       , 14 )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                       , 15 )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                       , 16 )
         , {- 17 -} '( App       (t (Static  Thunk t))                       , 17 )
         , {- 18 -} '( Arrow     (t (Static  Thunk t))                       , 18 )
         , {- 19 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , 19 )
         , {- 20 -} '( App       (t (Dynamic Thunk t))                       , 20 )
         , {- 21 -} '( Cons      (t (Dynamic Thunk t))                       , 21 )
         , {- 22 -} '( Arrow     (t (Dynamic Thunk t))                       , 22 )
         , {- 23 -} '( Var   Str                                             , 23 )
         , {- 24 -} '( Unify     (t (Static  Term t))                        , 24 )
         , {- 25 -} '( Acc   Str (t (Static  Term t))                        , 25 )
         , {- 26 -} '( App       (t (Static  Term t))                        , 26 )
         , {- 27 -} '( Arrow     (t (Static  Term t))                        , 27 )
         , {- 28 -} '( Var       (t (Dynamic Term t))                        , 28 )
         , {- 29 -} '( Unify     (t (Dynamic Term t))                        , 29 )
         , {- 30 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , 30 )
         , {- 31 -} '( App       (t (Dynamic Term t))                        , 31 )
         , {- 32 -} '( Cons      (t (Dynamic Term t))                        , 32 )
         , {- 33 -} '( Arrow     (t (Dynamic Term t))                        , 33 )
         , {- 34 -} '( Blank                                                 , 34 )
         , {- 35 -} '( Unify     (t (Static  Draft t))                       , 35 )
         , {- 36 -} '( Acc   Str (t (Static  Draft t))                       , 36 )
         , {- 37 -} '( App       (t (Static  Draft t))                       , 37 )
         , {- 38 -} '( Arrow     (t (Static  Draft t))                       , 38 )
         , {- 39 -} '( Var       (t (Dynamic Draft t))                       , 39 )
         , {- 40 -} '( Unify     (t (Dynamic Draft t))                       , 40 )
         , {- 41 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , 41 )
         , {- 42 -} '( App       (t (Dynamic Draft t))                       , 42 )
         , {- 43 -} '( Cons      (t (Dynamic Draft t))                       , 43 )
         , {- 44 -} '( Arrow     (t (Dynamic Draft t))                       , 44 )
         ]

#ifndef CachedTypeFamilies

type DecodeMap_RULE t = 'Map $ Zip (Layout_CACHE t) (Enumerate (Size (Layout_CACHE t)))
CACHE_AS(DecodeMap_RULE, "DecodeMap_GEN_CACHE")
CHECK_EQ(DecodeMap_GEN_CACHE IM, DecodeMap_MANUAL_CACHE IM)
type DecodeMap_CACHE t = DecodeMap_GEN_CACHE t

#else

type DecodeMap_CACHE t = DecodeMap_MANUAL_CACHE t

#endif

type instance DecodeMap (ASTRecord gs vs t d) = DecodeMap_CACHE t


-- === EncodeMap === --

type EncodeMap_MANUAL_CACHE t =
    'Map [ {-  9 -} '( Star                                                  , '[  9 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 10 -} '( Str                                                   , '[ 10 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 11 -} '( Number                                                , '[ 11 , 0,1,2,3,4,5,6,7,8 ] )
         , {- 12 -} '( Cons  Str                                             , '[ 12 , 1,2,3,4,5,6,7,8   ] )
         , {- 13 -} '( Arrow     (t (Static  Val   t))                       , '[ 13 , 1,2,3,4,5,6,7,8   ] )
         , {- 14 -} '( Cons      (t (Dynamic Val   t))                       , '[ 14 , 2,4,6,8           ] )
         , {- 15 -} '( Arrow     (t (Dynamic Val   t))                       , '[ 15 , 2,4,6,8           ] )
         , {- 16 -} '( Acc   Str (t (Static  Thunk t))                       , '[ 16 , 3,4,5,6,7,8       ] )
         , {- 17 -} '( App       (t (Static  Thunk t))                       , '[ 17 , 3,4,5,6,7,8       ] )
         , {- 18 -} '( Arrow     (t (Static  Thunk t))                       , '[ 18 , 3,4,5,6,7,8       ] )
         , {- 19 -} '( Acc       (t (Dynamic Thunk t)) (t (Dynamic Thunk t)) , '[ 19 , 4,6,8             ] )
         , {- 20 -} '( App       (t (Dynamic Thunk t))                       , '[ 20 , 4,6,8             ] )
         , {- 21 -} '( Cons      (t (Dynamic Thunk t))                       , '[ 21 , 4,6,8             ] )
         , {- 22 -} '( Arrow     (t (Dynamic Thunk t))                       , '[ 22 , 4,6,8             ] )
         , {- 23 -} '( Var   Str                                             , '[ 23 , 5,6,7,8           ] )
         , {- 24 -} '( Unify     (t (Static  Term t))                        , '[ 24 , 5,6,7,8           ] )
         , {- 25 -} '( Acc   Str (t (Static  Term t))                        , '[ 25 , 5,6,7,8           ] )
         , {- 26 -} '( App       (t (Static  Term t))                        , '[ 26 , 5,6,7,8           ] )
         , {- 27 -} '( Arrow     (t (Static  Term t))                        , '[ 27 , 5,6,7,8           ] )
         , {- 28 -} '( Var       (t (Dynamic Term t))                        , '[ 28 , 6,8               ] )
         , {- 29 -} '( Unify     (t (Dynamic Term t))                        , '[ 29 , 6,8               ] )
         , {- 30 -} '( Acc       (t (Dynamic Term t)) (t (Dynamic Term t))   , '[ 30 , 6,8               ] )
         , {- 31 -} '( App       (t (Dynamic Term t))                        , '[ 31 , 6,8               ] )
         , {- 32 -} '( Cons      (t (Dynamic Term t))                        , '[ 32 , 6,8               ] )
         , {- 33 -} '( Arrow     (t (Dynamic Term t))                        , '[ 33 , 6,8               ] )
         , {- 34 -} '( Blank                                                 , '[ 34 , 7,8               ] )
         , {- 35 -} '( Unify     (t (Static  Draft t))                       , '[ 35 , 7,8               ] )
         , {- 36 -} '( Acc   Str (t (Static  Draft t))                       , '[ 36 , 7,8               ] )
         , {- 37 -} '( App       (t (Static  Draft t))                       , '[ 37 , 7,8               ] )
         , {- 38 -} '( Arrow     (t (Static  Draft t))                       , '[ 38 , 7,8               ] )
         , {- 39 -} '( Var       (t (Dynamic Draft t))                       , '[ 39 , 8                 ] )
         , {- 40 -} '( Unify     (t (Dynamic Draft t))                       , '[ 40 , 8                 ] )
         , {- 41 -} '( Acc       (t (Dynamic Draft t)) (t (Dynamic Draft t)) , '[ 41 , 8                 ] )
         , {- 42 -} '( App       (t (Dynamic Draft t))                       , '[ 42 , 8                 ] )
         , {- 43 -} '( Cons      (t (Dynamic Draft t))                       , '[ 43 , 8                 ] )
         , {- 44 -} '( Arrow     (t (Dynamic Draft t))                       , '[ 44 , 8                 ] )
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
CACHE_AS(SubGroupRelations_RULE, "SubGroupRelations")

-- SubGroupInvRelations

type family InverseRel  arg rels where InverseRel arg rels = '(arg, InverseRel' arg rels)
type family InverseRel' (arg :: Nat) (rels :: [(Nat, [Nat])]) where
    InverseRel' a '[]                = '[]
    InverseRel' a ( '(s, ts) ': rs ) = If (a `In` ts) '[s] '[] <> InverseRel' a rs

type family MapInverseRel args rels where
    MapInverseRel '[]       rels = '[]
    MapInverseRel (a ': as) rels = InverseRel a rels ': MapInverseRel as rels

type SubGroupInvRelations_RULE = (MapInverseRel (Enumerate (Size (GroupList IM))) SubGroupRelations :: [(Nat, [Nat])])
CACHE_AS(SubGroupInvRelations_RULE, "SubGroupInvRelations")

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
CACHE_AS(EncodeMap_RULE, "EncodeMap_GEN_CACHE")
CHECK_EQ(EncodeMap_GEN_CACHE IM, EncodeMap_MANUAL_CACHE IM)
type EncodeMap_CACHE t = EncodeMap_GEN_CACHE t

#else

type EncodeMap_CACHE t = EncodeMap_MANUAL_CACHE t

#endif

type instance EncodeMap (ASTRecord gs vs t d) = EncodeMap_CACHE t




