{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Luna.Syntax.AST.Typed where

import Prologue

import Luna.Syntax.AST
import Luna.Syntax.AST.Term
--import Luna.Syntax.Builder
import Data.Cata
import Data.Layer

import           Luna.Syntax.Builder.Star (MonadStarBuilder)
import qualified Luna.Syntax.Builder.Star as Star

import           Luna.Syntax.Builder       (connect, getStar2)
import           Luna.Syntax.Builder.Node  (MonadNodeBuilder)
import           Luna.Syntax.Builder.Class (modify2, BuilderMonad)
import           Luna.Syntax.Repr.Graph    (Graph, TracksSuccs, Ref, Node, Edge, DoubleArc)

import           Data.Layer.Coat
import           Data.Variants             (SpecificCons)

-- === Typed ===

data Typed t a = Typed t a deriving (Show, Functor, Traversable, Foldable)


class HasType a t | a -> t where tp :: Lens' a t
instance {-# OVERLAPPABLE #-} (HasType (Unlayered a) t, Layered a) 
                           => HasType a           t where tp = layered . tp
instance {-# OVERLAPPABLE #-} HasType (Typed t a) t where tp = lens (\(Typed t _) -> t) (\(Typed _ a) t -> Typed t a)


-- === Typed layer ===

type instance Unlayered (Typed t a) = a
instance      Layered   (Typed t a) where layered = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)


instance ( TracksSuccs (Unlayered n), Layered n
         , Uncoated n ~ Uncoated (Unlayered n), LayerGen m n
         , MonadFix m, CoatGen m (Unlayered n), MonadStarBuilder (Maybe (Ref Node)) m, BuilderMonad (Graph n DoubleArc) m, SpecificCons Star (Uncoated n), MonadNodeBuilder (Ref Node) m
         ) => LayerGen m (Typed (Ref Edge) a) where
    genLayer a = do
        s <- getStar2
        c <- connect s
        return $ Typed c a



