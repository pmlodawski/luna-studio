{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


module Luna.Syntax.Term where

import Flowbox.Prelude hiding (Cons)
import Data.Variant
import Luna.Syntax.Lit
import Luna.Syntax.Arg
import Luna.Syntax.Name

import Data.Cata

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

-----------------------------------------------------------------------------------------------

newtype Val'   t = Val'   (Record (ValElems   t))
newtype Thunk' t = Thunk' (Record (ThunkElems t))
newtype Term'  t = Term'  (Record (TermElems  t))





data Cat a = Cat { _ci :: Int, _ca :: a } deriving (Functor)

makeLenses ''Cat

mkRec c i = Mu (c i $ mkRec c (i + 1))
c = mkRec Cat 0 :: Mu Cat

c' = cata (\a -> if (view ci a < 10) then view ca a else (view ci a)) c





data Type a = Star
            | Type a

data Typed a t = Typed t (a t)

data Labeled' a t = Labeled' (a t)

type F1 h = MuH h Val'

type F2 h = MuH h (Labeled' (Typed Val'))
type F3   = Mu  (Labeled' (Typed Val'))
type F4 h = MuH h (Typed Val')
type F5   = Mu (Typed Val')

data Han a = Han

xt1 = toMu (Typed Han (Val' undefined)) :: F4 Han
xt2 = toMu (Typed (toMu (Typed undefined undefined)) undefined) :: F5

newtype ValT  h = ValT  { __valTRec  :: Record (ValElems (h (T h (Val (T h))))) }


-- mozna type sets zamienic na Typed, np:
-- ValElems t = Typed t Lit
--            : Typed t (Cons t)
--            : ...


newtype Val2 h = Val2 { __valRec2   :: Record (ValElems (h (Val2 h))) }

data Typed2 a h = Typed2 (a (Typed2 a)) -- (h (a (Typed2 a)))

--Typed2 Val

    --newtype Val2 h = Val2 { __valRec2   :: Record (ValElems (h (Val2 h))) }

    --data Typed2   h a = Typed2   (h a) a
    --data Labeled2 h a = Labeled2 (h a)

    --type TVal h = Val2 (Typed2 h)


    --Record (ValElems (h (Val2 h)))


--type TVal h = Val2 (Labeled2 (Typed2 h))

data T h a = T a (h a)
newtype TT (h :: * -> *) a = TT (h (T h a))


--h (Val h)

type Foo1 h = TT h (Val (TT h))

--deriving instance Show (a h) => Show (TT a h)
-----------------------------------------------------------------------------------------------


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



