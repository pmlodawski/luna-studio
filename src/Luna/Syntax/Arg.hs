module Luna.Syntax.Arg where

import Flowbox.Prelude
import Luna.Syntax.Name


data    Arg      a = Arg { __aname :: Maybe Name , __arec :: a } deriving (Show)
