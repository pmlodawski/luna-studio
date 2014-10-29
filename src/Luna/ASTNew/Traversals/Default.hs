---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
 {-# LANGUAGE UndecidableInstances #-}
 {-# LANGUAGE OverlappingInstances #-}

module Luna.ASTNew.Traversals.Default where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           Luna.ASTNew.Traversals.Class
import           Luna.ASTNew.Label      (Label(Label))


----- Basic -----
instance DefaultTraversal base m a => Traversal base m a where traverse = defaultTraverse
--instance DefaultTraversal base m a where defaultTraverse _ = pure


----- Label -----
instance Traversal base m a => Traversal base m (Label l a) where
    traverse base (Label l a) = fmap (Label l) $ traverse base a
