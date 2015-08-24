module Luna.Syntax.AST.Arg where

import Flowbox.Prelude
import Luna.Syntax.Name


data Arg a = Arg { __aname :: Maybe Name , __arec :: a } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
