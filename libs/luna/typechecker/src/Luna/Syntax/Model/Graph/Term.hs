{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo          #-}

module Luna.Syntax.Model.Graph.Term where

import Prologue hiding (cons, Num)

import           Control.Monad.Event
import           Data.Layer.Cover
import           Data.Record                    (cons, RecordOf, IsRecord, asRecord, SmartCons)
import           Data.Reprx                     (Repr, repr)
import qualified Luna.Syntax.AST.Term           as Term
import           Luna.Syntax.AST.Term           hiding (Val, Lit, Thunk, Expr, Draft)
import           Luna.Syntax.Model.Builder.Self
import           Luna.Syntax.Model.Graph.Class
import           Luna.Syntax.Model.Layer


---------------------------------------
-- === Network layout definition === --
---------------------------------------

type instance Layout (Network ls) term rt = Ref $ Link (ls :< TermWrapper term rt)


---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

type family TermWrapper (a :: *) :: * -> [*] -> *

data    Raw      (ls :: [*]) = Raw Data                                deriving (Show, Eq)
newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Term.Lit   rt) deriving (Show, Eq)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Term.Val   rt) deriving (Show, Eq)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Term.Thunk rt) deriving (Show, Eq)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Term.Expr  rt) deriving (Show, Eq)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Term.Draft rt) deriving (Show, Eq)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw
makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

-- Layout types

type instance LayoutType (Raw      ls) = Network ls
type instance LayoutType (Lit   rt ls) = Network ls
type instance LayoutType (Val   rt ls) = Network ls
type instance LayoutType (Thunk rt ls) = Network ls
type instance LayoutType (Expr  rt ls) = Network ls
type instance LayoutType (Draft rt ls) = Network ls

-- Term bindings

type instance TermWrapper Term.Lit   = Lit
type instance TermWrapper Term.Val   = Val
type instance TermWrapper Term.Thunk = Thunk
type instance TermWrapper Term.Expr  = Expr
type instance TermWrapper Term.Draft = Draft

-- Records

type instance RecordOf (Lit   rt ls) = RecordOf (Unwrapped (Lit   rt ls))
type instance RecordOf (Val   rt ls) = RecordOf (Unwrapped (Val   rt ls))
type instance RecordOf (Thunk rt ls) = RecordOf (Unwrapped (Thunk rt ls))
type instance RecordOf (Expr  rt ls) = RecordOf (Unwrapped (Expr  rt ls))
type instance RecordOf (Draft rt ls) = RecordOf (Unwrapped (Draft rt ls))

instance IsRecord (Lit   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Val   rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Thunk rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Expr  rt ls) where asRecord = wrapped' ∘ asRecord
instance IsRecord (Draft rt ls) where asRecord = wrapped' ∘ asRecord

-- Conversions

instance Castable (Raw      ls) (Raw      ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Lit   rt ls) (Lit   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Val   rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Thunk rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Expr  rt ls) where cast = id ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Draft rt ls) where cast = id ; {-# INLINE cast #-}

instance Castable (Lit   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Val   rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Thunk rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Expr  rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Draft rt ls) (Raw ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

instance Castable (Raw ls) (Lit   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Val   rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Thunk rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Expr  rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}
instance Castable (Raw ls) (Draft rt ls) where cast = wrap' ∘ cast ∘ unwrap' ; {-# INLINE cast #-}

-- Representations

instance {-# OVERLAPPABLE #-}                                     Repr s (Raw      ls) where repr = const "Raw"
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Lit   rt ls)) => Repr s (Lit   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Val   rt ls)) => Repr s (Val   rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Thunk rt ls)) => Repr s (Thunk rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Expr  rt ls)) => Repr s (Expr  rt ls) where repr = repr ∘ unwrap'
instance {-# OVERLAPPABLE #-} Repr s (Unwrapped (Draft rt ls)) => Repr s (Draft rt ls) where repr = repr ∘ unwrap'


-------------------------------------
-- === Term building utilities === --
-------------------------------------

class    ElemBuilder el m  a where buildElem :: el -> m a
instance ElemBuilder el IM a where buildElem = impossible


-- === Instances === --

instance ( SmartCons el (Uncovered a)
         , CoverConstructor m a
         , Register ELEMENT a m
         , MonadSelfBuilder s m
         , Castable a s
         ) => ElemBuilder el m a where
    -- TODO[WD]: change buildAbsMe to buildMe2
    --           and fire monad every time we construct an element, not once for the graph
    buildElem el = register ELEMENT =<< buildAbsMe (constructCover $ cons el) where
    {-# INLINE buildElem #-}



-------------------------------
-- === Term constructors === --
-------------------------------

arg :: a -> Arg a
arg = Arg Nothing

fromArg :: Arg a -> a
fromArg (Arg _ a) = a

star :: ElemBuilder Star m a => m a
star = buildElem Star

string :: ElemBuilder Str m a => String -> m a
string = buildElem . Str

int :: ElemBuilder Num m a => Int -> m a
int = buildElem . Num

acc :: ( MonadFix m
       , ElemBuilder (Acc n (Connection a u)) m u
       , Connectible a u m
       ) => n -> a -> m u
acc n a = mdo
    out <- buildElem $ Acc n ca
    ca  <- connection a out
    return out

app :: ( MonadFix m
       , ElemBuilder (App (Connection a u)) m u
       , Connectible a u m
       ) => a -> [Arg a] -> m u
app f args = mdo
    out <- buildElem $ App cf cargs
    cf  <- connection f out
    cargs <- mapM (\(Arg n a) -> (Arg n) <$> (connection a out)) args
    return out

unify :: ( MonadFix m
         , ElemBuilder (Unify (Connection b u)) m u
         , Connectible a u m
         , Connectible b u m
         , Connection b u ~ Connection a u
         ) => a -> b -> m u
unify a b = mdo
    out <- buildElem $ Unify ca cb
    ca  <- connection a out
    cb  <- connection b out
    return out



