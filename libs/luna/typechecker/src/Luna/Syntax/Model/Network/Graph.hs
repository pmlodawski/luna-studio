{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Graph (module Luna.Syntax.Model.Network.Graph, module X) where

import Prelude.Luna

import           Luna.Evaluation.Runtime        as Runtime
import           Luna.Syntax.Model.Graph        as X hiding (nodes, edges)
import           Luna.Syntax.AST.Term           (Input)
import qualified Luna.Syntax.Model.Graph        as Graph
import           Luna.Syntax.Model.Layer        ((:<))


-- === Instances === --
-- All th instances below implement Network relations on general Graph type.
-- They should not be included in the Graph-like modules, because Graphs should not know
-- about these functionalities. Although these are orphans, they are imported by the most basic
-- modules of Network implementation.

type instance Input (Ref a) = Ref (Input (Unlayered (Ref a)))
type instance Input (Node a) = Node a

type instance Runtime.Model (Ref a) = Runtime.Model (Unlayered (Ref a))
type instance Runtime.Model (Node a) = Runtime.Model (Unlayered (Node a))
type instance Runtime.Model (ls :< a) = Runtime.Model (Uncovered (ls :< a))
