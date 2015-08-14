{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Term where

import Flowbox.Prelude hiding (Cons)
import Data.Variant
import Luna.Syntax.Lit
import Luna.Syntax.Arg
import Luna.Syntax.Name

-- === Terms ===




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




-- Component types

newtype Var        = Var      Name      deriving (Show, Eq)
data    Cons     a = Cons     Name [a]  deriving (Show)
data    Accessor a = Accessor Name a    deriving (Show)
data    App      a = App      a [Arg a] deriving (Show)



-- Type sets

type TermElems      a = Var
                     ': ConstTermElems a

type ConstTermElems a = Accessor a
                     ': App      a
                     ': ValElems a

type ValElems       a = Lit
                     ': Cons a
                     ': '[]


-- Record types

type ValTypes       h = ValElems (h (Val h))
type ConstTermTypes h = (Val h) ': ConstTermElems (h (ConstTerm h))
type TermTypes      h = (Val h) ': (ConstTerm h) ': TermElems (h (Term h))

newtype Val       h = Val       { __vrecH :: Record (ValTypes h) }
newtype ConstTerm h = ConstTerm { __crecH :: Record (ConstTermTypes h) }
newtype Term      h = Term      { __trecH :: Record (TermTypes h) }

makeLenses ''Val
makeLenses ''ConstTerm
makeLenses ''Term

-- instances

deriving instance (Show (h (Val h)))                                            => Show (Val h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)))                    => Show (ConstTerm h)
deriving instance (Show (h (Val h)), Show (h (ConstTerm h)), Show (h (Term h))) => Show (Term h)

type instance Variants (Val h)       = ValTypes       h
type instance Variants (ConstTerm h) = ConstTermTypes h
type instance Variants (Term h)      = TermTypes      h

instance IsVariant (Val h)       where record  = _vrecH
                                       variant = Val

instance IsVariant (ConstTerm h) where record  = _crecH
                                       variant = ConstTerm

instance IsVariant (Term h)      where record  = _trecH
                                       variant = Term

-- Repr instances

instance Repr a => Repr (App a) where
    repr (App a args) = "App (" <> repr a <> ") " <> repr args

instance Repr a => Repr (Arg a) where
    repr (Arg n a) = "Arg (" <> repr n <> ") (" <> repr a <> ")"