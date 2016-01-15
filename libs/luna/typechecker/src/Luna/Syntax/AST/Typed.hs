{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Luna.Syntax.AST.Typed where

import Prologue

import Luna.Syntax.AST
import Luna.Syntax.AST.Term
import Luna.Syntax.Builder
import Data.Cata
import Data.Layer

import           Luna.Syntax.Builder.Star (MonadStarBuilder)
import qualified Luna.Syntax.Builder.Star as Star

--import           Luna.Syntax.Builder       (connect, getStar2)
import           Luna.Syntax.Builder.Node  (MonadNodeBuilder)
import           Luna.Syntax.Builder.Class (modify2, BuilderMonad)
import           Luna.Syntax.Repr.Graph    (Graph, TracksSuccs, Ref, Node, Edge, DoubleArc)

--import           Data.Layer.Coat
--import           Data.Variants             (SpecificCons)
import           Data.Construction

---- === Typed ===

--data Typed t a = Typed t a deriving (Show, Functor, Traversable, Foldable)


--class HasType a t | a -> t where tp :: Lens' a t
--instance {-# OVERLAPPABLE #-} (HasType (Unlayered a) t, Layered a) 
--                           => HasType a           t where tp = layered . tp
--instance {-# OVERLAPPABLE #-} HasType (Typed t a) t where tp = lens (\(Typed t _) -> t) (\(Typed _ a) t -> Typed t a)


---- === Typed layer ===

--type instance Unlayered  (Typed t a) = a
--type instance Destructed (Typed t a) = a
--instance      Layered    (Typed t a) where layered = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)

--instance ( TracksSuccs (Unlayered n), Layered n
--         , Uncoated n ~ Uncoated (Destructed n), Constructor m n
--         , MonadFix m, CoatConstructor m (Destructed n), MonadStarBuilder (Maybe (Ref Node)) m, BuilderMonad (Graph n DoubleArc) m, SpecificCons Star (Uncoated n), MonadNodeBuilder (Ref Node) m
--         ) => Constructor m (Typed (Ref Edge) a) where
--    construct a = do
--        s <- getStar2
--        c <- connect s
--        return $ Typed c a

--instance (Monad m, BuilderMonad (Graph n DoubleArc) m, Layered n, TracksSuccs (Unlayered n)) => Destructor m (Typed (Ref Edge) a) where
--	destruct (Typed t a) = do
--		unregisterEdge t
--		return a

----class Destructor  m a where destruct  :: a -> m (Destructed a)




--type family Param a where Param (a t) = t
----data Typed2 l a = Typed2 (l a)
--data Typed2 l a = Typed2 (Param a) (l a)


--type instance Unlayered  (Typed2 l a) = l a
--type instance Destructed (Typed2 l a) = l a

--instance Monad m => Constructor m (Typed2 l a) where construct = return . Typed2 undefined