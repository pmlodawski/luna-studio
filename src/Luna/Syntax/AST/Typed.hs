{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Luna.Syntax.Repr.Graph    (Graph, TracksSuccs, Ref)

import           Data.Layer.Coat
import           Data.Variants             (SpecificCons)

-- === Typed ===

data Typed t a = Typed t a deriving (Show, Functor, Traversable, Foldable)



-- === Typed layer ===

type instance Unlayered (Typed t a) = a
instance      Layered   (Typed t a) where layered = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)


instance ( TracksSuccs (Unlayered t), Layered t
         , Uncoated t ~ Uncoated (Unlayered t), LayerGen m t
         , MonadFix m, CoatGen m (Unlayered t), MonadStarBuilder (Maybe (Ref Int)) m, BuilderMonad (Graph t) m, SpecificCons Star (Uncoated t), MonadNodeBuilder (Ref Int) m
         ) => LayerGen m (Typed (Ref Int) a) where
    genLayer a = do
        s <- getStar2
        c <- connect s
        return $ Typed c a
