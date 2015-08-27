{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Syntax.AST.Term where

import Flowbox.Prelude hiding (Cons, cons)
import Data.Variants   hiding (Cons)

import Luna.Syntax.AST.Lit
import Luna.Syntax.AST.Arg
import Luna.Syntax.Name

import Data.Cata
import Data.Containers.Hetero

import Data.Typeable
import Luna.Repr.Styles (HeaderOnly)

-- === Terms ===

-- Component types

-- LEGEND
-- N   - Name
-- S   - Source
-- A/P - Args / Params

-- Layout                     N S A/P
data    Star       = Star                 deriving (Show, Eq, Ord)
newtype Var      t = Var      t           deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Cons     t = Cons     t   [t]     deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    Accessor t = Accessor t t         deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
data    App      t = App        t [Arg t] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- Type sets

type TermElems  t = Var        t
                 ': ThunkElems t

type ThunkElems t = Accessor t
                 ': App      t
                 ': ValElems t

type ValElems   t = Lit
                 ': Cons t
                 ': Star
                 ': '[]

-- Record types

newtype Val   t = Val   (VariantRec (Val   t)) deriving (Show)
newtype Thunk t = Thunk (VariantRec (Thunk t)) deriving (Show)
newtype Term  t = Term  (VariantRec (Term  t)) deriving (Show)

type instance Variants (Val   t) =                         ValElems   t
type instance Variants (Thunk t) = (Val t) ':              ThunkElems t
type instance Variants (Term  t) = (Val t) ': (Thunk t) ': TermElems  t


-- Record & variant instances

instance Record (Val   h) where mkRecord = Val
instance Record (Thunk h) where mkRecord = Thunk
instance Record (Term  h) where mkRecord = Term

instance HasRecord (Val   t) (Val   t') where record = lens (\(Val   a) -> a) (const Val  )
instance HasRecord (Thunk t) (Thunk t') where record = lens (\(Thunk a) -> a) (const Thunk)
instance HasRecord (Term  t) (Term  t') where record = lens (\(Term  a) -> a) (const Term )

-- Name instances

class                          m ~ Maybe            => MaybeNamedVariant v     m     a where checkVariantName :: v -> m a
instance {-# OVERLAPPABLE #-} (a ~ t, MaybeNamed v) => MaybeNamedVariant (v t) Maybe a where checkVariantName = checkName
instance {-# OVERLAPPABLE #-}                          MaybeNamedVariant v     Maybe a where checkVariantName = const Nothing

instance MaybeNamed Val   where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record
instance MaybeNamed Thunk where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record
instance MaybeNamed Term  where checkName = withVariantsM (Proxy :: Proxy MaybeNamedVariant) checkVariantName . view record


--TODO[wd]: makeClassyInstances ''Cons
instance MaybeNamed Var
instance MaybeNamed Cons
instance MaybeNamed Accessor
instance HasName    Var      where name = lens (\(Var n)        -> n) (\(Var _) n         -> Var n)
instance HasName    Cons     where name = lens (\(Cons n _)     -> n) (\(Cons _ t1) n     -> Cons n t1)
instance HasName    Accessor where name = lens (\(Accessor n _) -> n) (\(Accessor _ t1) n -> Accessor n t1)

-- Utils intances

instance Functor     Val   where fmap = recordMap
instance Functor     Thunk where fmap = recordMap
instance Functor     Term  where fmap = recordMap

instance Foldable    Val   where foldr = recordFoldr
instance Foldable    Thunk where foldr = recordFoldr
instance Foldable    Term  where foldr = recordFoldr

instance Traversable Val   where traverse = recordTraverse
instance Traversable Thunk where traverse = recordTraverse
instance Traversable Term  where traverse = recordTraverse

-- === Representations ===

instance             Repr s Star         where repr = fromString . show
instance Repr s t => Repr s (Var      t) where repr (Var      n  ) = "Var"      <+> repr n
instance Repr s t => Repr s (Cons     t) where repr (Cons     n t) = "Cons"     <+> repr n <+> repr t
instance Repr s t => Repr s (Accessor t) where repr (Accessor n t) = "Accessor" <+> repr n <+> repr t
instance Repr s t => Repr s (App      t) where repr (App      n t) = "App"      <+> repr n <+> repr t

instance VariantReprs s (Val   t) => Repr s (Val   t) where repr (Val   t) = "Val"   <+> repr t
instance VariantReprs s (Thunk t) => Repr s (Thunk t) where repr (Thunk t) = "Thunk" <+> repr t
instance VariantReprs s (Term  t) => Repr s (Term  t) where repr (Term  t) = "Term"  <+> repr t

-- HeaderOnly

instance {-# OVERLAPPING #-} Repr HeaderOnly Star         where repr _ = "Star"
instance {-# OVERLAPPING #-} Repr HeaderOnly (App      t) where repr _ = "App"
instance {-# OVERLAPPING #-} Repr HeaderOnly (Var      t) where repr a = "Var"
instance {-# OVERLAPPING #-} Repr HeaderOnly (Cons     t) where repr a = "Cons"
instance {-# OVERLAPPING #-} Repr HeaderOnly (Accessor t) where repr a = "Accessor"


-- === Inputs ===

inputs :: Foldable t => t a -> [a]
inputs = foldr (:) []


-- === Ideas ===

-- Termy takie jak Val pobieraja Stringa, ale powinny pobierac Term, otypowany implicite jako String lub Interface

-- === Breadcrumbs ===

--instance Inputs Star where inputs
        --data CTop      = CTop
        --data CTarget t = CTarget t
        --data CName     = CName

        ----data CVar    = CVar  (Record '[CName])
        --data CAccessor t = CAccessor (Record '[CName, CTarget t])
        --data CTerm     t = CTerm     (Record '[CAccessor t])


        --type family PathsOf a :: [*]
        --type instance PathsOf (Accessor t) = '[CTarget (Record (PathsOf t))]


        --data Crumb t a = Crumb a


        --type CX = Mu CTerm

        ----c1 = Mu (CTerm $ cons $ CAccessor $ undefined) :: CX

        --cx = CAccessor $ cons CName :: CAccessor (Mu CTerm)

        --cy = CTerm $ cons cx :: CTerm (Mu CTerm)

        --cz = Mu (CTerm $ cons cx) :: CX
--data CTerm t = CTerm (Record '[Accessor t])

--c1 = cons CName :: CAccessor


    --type instance Variants (CAccessor t) = '[CName, CTarget (Record (PathsOf t))]

    --instance IsVariant (CAccessor t) where
    --    variant = CAccessor
    --    record  = undefined

    ----class IsVariant a where
    ----    variant :: Record (Variants a) -> a
    ----    record  :: Lens' a (Record (Variants a))

    --type family CrumbOf a
    --type instance CrumbOf (Mu a) = CrumbOf (a (Mu a))
    --type instance CrumbOf (Accessor t) = CAccessor t
    --type instance CrumbOf (Term t) = CTerm t


    --type family PathsOf a :: [*]

    --type instance PathsOf (Accessor t) = '[CTarget (Record (PathsOf t))]

    --class HasTarget t a | a -> t where
    --    target :: Lens' a t

    --instance HasTarget t (Accessor t) where
    --    target = lens (\(Accessor _ t) -> t) (\(Accessor n _) t -> Accessor n t)

    --instance HasTarget (CAccessor t) (CTop (Accessor t)) where
    --    target = lens (const $ cons (CTarget :: CTarget (Record (PathsOf t)))) undefined

    --instance c ~ CrumbOf a => HasTarget c (CTop a) where

    --v = Mu (Term $ cons $ Accessor "foo" (Mu $ Term $ cons $ Var "foo")) :: Mu Term

    --vc = CTop v :: CTop (Mu Term)

    --vc' :: CTerm (Mu Term)
    --vc' = view target vc

--instance HasTarget a t => HasTarget (Crumb a) where
--    target = lens (const $ Crumb CTarget) const - - - tu jest zle
    -- trzebaby dorobic MapVariants lub WithVariants - przy dodawaniu nowego breadcrumba

--------------------

--v = Crumb CTop :: Crumb CTop (Accessor Int)
--v' = view target v :: _


-- breadcrumby moga byc po prostu lensami Lens' a t
-- lub czyms podobnym pozwalajacym na rozlaczanie (!)

--data CTop    = CTop
--data CTarget = CTarget

--data Crumb a b = Crumb a b

-- ...

--instance HasTarget CTarget (Crumb a (Accessor t)) where
--    target = lens (const CTarget) const

-- najlepsze jest chyba reczne odwzorowanie struktury z lensami, przy cyzm nawet stale Lensy takie jak HasName przerobic na takie jak wyzej,
-- by dzialaly z breadcrumbami










--type Term h = h (Term h)


--data Arg h = Arg { _label :: Maybe Name, _term :: Term h }

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



