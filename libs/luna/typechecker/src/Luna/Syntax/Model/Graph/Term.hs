{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Term where

import Prologue

import           Data.Record                   (RecordOf, IsRecord, asRecord)
import           Data.Reprx                    (Repr, repr)
import           Luna.Syntax.AST.Term          (Term, Data)
import           Luna.Syntax.Model.Graph.Class (Network)
import qualified Luna.Syntax.AST.Term          as Term


---------------------------
-- === Network Terms === --
---------------------------

-- === Definitions === --

type family TermWrapper (a :: *) :: * -> [*] -> *

data    Raw      (ls :: [*]) = Raw Data                                deriving (Show)

newtype Lit   rt (ls :: [*]) = Lit   (Term (Network ls) Term.Lit   rt) deriving (Show)
newtype Val   rt (ls :: [*]) = Val   (Term (Network ls) Term.Val   rt) deriving (Show)
newtype Thunk rt (ls :: [*]) = Thunk (Term (Network ls) Term.Thunk rt) deriving (Show)
newtype Expr  rt (ls :: [*]) = Expr  (Term (Network ls) Term.Expr  rt) deriving (Show)
newtype Draft rt (ls :: [*]) = Draft (Term (Network ls) Term.Draft rt) deriving (Show)


-- === Instances === --

-- Wrappers

makeWrapped ''Raw

makeWrapped ''Lit
makeWrapped ''Val
makeWrapped ''Thunk
makeWrapped ''Expr
makeWrapped ''Draft

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