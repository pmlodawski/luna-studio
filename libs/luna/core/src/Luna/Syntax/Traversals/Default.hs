---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Please think twice if you have strong reason to touch this file. 
-- Fragile, can explode in your face and you will not know about it!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 {-# LANGUAGE UndecidableInstances #-}
 {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

module Luna.Syntax.Traversals.Default where

import           Flowbox.Prelude        hiding (Cons, Traversal, traverse)
import           Luna.Syntax.Traversals.Class
import           Luna.Syntax.Label      (Label(Label))


----- Basic -----
instance DefaultTraversal base m a b => Traversal base m a b where traverseM = defaultTraverseM


----- Label -----
instance Traversal base m a b => Traversal base m (Label l a) (Label l b) where
    traverseM base (Label l a) = fmap (Label l) $ traverseM base a


instance Traversal base m a b => DefaultTraversal base m (Label l a) (Label l b) where
    defaultTraverseM b (Label l a) = Label l <$> traverseM b a