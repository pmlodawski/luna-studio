{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term where

import Flowbox.Prelude hiding (Cons)
import Data.Variant
import Luna.Syntax.Lit
import Luna.Syntax.Arg
import Luna.Syntax.Name


-- === Terms ===

-- Component types

newtype Var        = Var      Name      deriving (Show, Eq)
data    Cons     t = Cons     Name [t]  deriving (Show)
data    Accessor t = Accessor Name t    deriving (Show)
data    App      t = App      t [Arg t] deriving (Show)

-- Type sets

type TermElems  t = Var
                 ': ThunkElems t

type ThunkElems t = Accessor t
                 ': App      t
                 ': ValElems t

type ValElems   t = Lit
                 ': Cons t
                 ': '[]

-- Record types

type H a h = h (a h)
type HRecord a (h :: * -> *) = VariantRecord (a h)

newtype Val   h = Val   { __valRec   :: HRecord Val   h }
newtype Thunk h = Thunk { __thunkRec :: HRecord Thunk h }
newtype Term  h = Term  { __termRec  :: HRecord Term  h }

--data TT a (h :: * -> *) = TT (a h)

--deriving instance Show (a h) => Show (TT a h)

type instance Variants (Val   h) =                         ValElems   (H Val   h)
type instance Variants (Thunk h) = (Val h) ':              ThunkElems (H Thunk h)
type instance Variants (Term  h) = (Val h) ': (Thunk h) ': TermElems  (H Term  h)

makeLenses ''Val
makeLenses ''Thunk
makeLenses ''Term
--TODO[wd]: makeClassyInstances (dla HasRecord etc)

-- instances

deriving instance (Show (H Val h))                                    => Show (Val   h)
deriving instance (Show (H Val h), Show (H Thunk h))                  => Show (Thunk h)
deriving instance (Show (H Val h), Show (H Thunk h), Show (H Term h)) => Show (Term  h)

instance IsVariant (Val h)   where record  = _valRec
                                   variant = Val

instance IsVariant (Thunk h) where record  = _thunkRec
                                   variant = Thunk

instance IsVariant (Term h)  where record  = _termRec
                                   variant = Term

-- Name instances

--TODO[wd]: makeClassyInstances ''Cons
instance HasName Var          where name = lens (\(Var n)        -> n) (\(Var _) n         -> Var n)
instance HasName (Cons a)     where name = lens (\(Cons n _)     -> n) (\(Cons _ t1) n     -> Cons n t1)
instance HasName (Accessor a) where name = lens (\(Accessor n _) -> n) (\(Accessor _ t1) n -> Accessor n t1)

-- Repr instances

instance Repr a => Repr (App a) where
    repr (App a args) = "App (" <> repr a <> ") " <> repr args

instance Repr a => Repr (Arg a) where
    repr (Arg n a) = "Arg (" <> repr n <> ") (" <> repr a <> ")"







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



