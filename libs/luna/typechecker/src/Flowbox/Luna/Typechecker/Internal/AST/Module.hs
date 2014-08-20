module Flowbox.Luna.Typechecker.Internal.AST.Module (Module(..)) where

import Flowbox.Luna.Typechecker.Internal.AST.Common (ID)
import Flowbox.Luna.Typechecker.Internal.AST.Type   (Type)
import Flowbox.Luna.Typechecker.Internal.AST.Expr   (Expr)

-- # data Module = Module {
-- #                   _id          :: ID
-- #                 , _cls         :: Type
-- #                 , _imports     :: [Expr]
-- #                 , _classes     :: [Expr]
-- #                 , _typeAliases :: [Expr]
-- #                 , _typeDefs    :: [Expr]
-- #                 , _fields      :: [Expr]
-- #                 , _methods     :: [Expr]
-- #                 , _modules     :: [Module]
-- #             }
-- #             deriving (Show)