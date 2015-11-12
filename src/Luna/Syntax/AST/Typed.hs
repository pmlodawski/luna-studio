{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Luna.Syntax.AST.Typed where

import Flowbox.Prelude

import Luna.Syntax.AST
import Luna.Syntax.AST.Term
--import Luna.Syntax.Builder
import Data.Cata
import Data.Layer

import           Luna.Syntax.Builder.Star (MonadStarBuilder)
import qualified Luna.Syntax.Builder.Star as Star

-- === Typed ===

data Typed t a = Typed t a deriving (Show, Functor, Traversable, Foldable)
----type instance ASTOf (Typed a) = ASTOf a

type instance Unlayered (Typed t a) = a

instance Layered (Typed t a) where layered = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)

--instance MonadStarBuilder t m => LayerGen m (Typed t a) where
--  genLayer a = Typed <$> Star.get <*> pure a



--deriving instance (Show t, Show (a t)) => Show (Typed a t)


----instance HasAST a ast => HasAST (Typed a) ast where ast = inner . ast

----instance Layer Typed where
----    inner = lens (\(Typed _ a) -> a) (\(Typed t _) a -> Typed t a)

----instance (LayeredStarBuilder m t, LayerGen (Mu t) m a) => LayerGen (Mu t) m (Typed a) where
----    genLayers a = Typed <$> getStar <*> genLayers a


----instance (TypeGenerator m (Mu t), LayerGen (Mu t) m a) => LayerGen (Mu t) m (Typed a) where
----    genLayers a = Typed <$> genType <*> genLayers a


--class TypeGenerator m t where
--    genType :: m t

--instance (LayeredStarBuilder m t, LayeredASTMuCons Var m t) => TypeGenerator m (Mu t) where
--    genType = getStar