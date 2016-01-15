module Empire.Data.WithMeta where

import Prologue

import Data.Layer.Coat
import Data.Construction
import Data.Variants

import Luna.Diagnostic.AST (GenInEdges, genInEdges)

data WithMeta b a = WithMeta { _meta :: b
                             , _node :: a
                             } deriving (Show, Functor, Traversable, Foldable)

makeLenses ''WithMeta

type instance Unlayered  (WithMeta b a) = a
type instance Destructed (WithMeta b a) = a
instance Layered (WithMeta b a) where layered = lens (\(WithMeta _ a) -> a) (\(WithMeta l _) a -> WithMeta l a)
instance (Monad m, Maker m b) => Constructor m (WithMeta b a) where construct a = WithMeta <$> make <*> pure a
instance (Monad m, Destroyer m b) => Destructor m (WithMeta b a) where destruct (WithMeta b a) = a <$ destroy b

instance Monad m => Destroyer m (Maybe a) where destroy _ = return ()

-- TODO: Remove once Wojtek resolves the crap in Diagnostic
instance GenInEdges n e a => GenInEdges n e (WithMeta b a) where
    genInEdges g = genInEdges g . view node
