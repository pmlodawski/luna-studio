{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Decl.Function where

import Flowbox.Prelude
import Data.Containers

data Function body = Function { _body :: body } deriving (Show)
makeLenses ''Function

-- instances

instance HasContainer body c => HasContainer (Function body) c where
    container = body . container