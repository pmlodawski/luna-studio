---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- TODO [wd]: when typechecker will be ready

module Luna.Core.Expr where


--data Var = Var Name Type

---- multiarg functions defined as: (Lam ... (Lam ... ...))

--data Expr = Var  Var
--          | Lit  Literal
--          | App  Expr Arg
--          | Lam  Patt Expr
--          | Let  Patt Expr
--          | Case ...
--          ... ?
--          deriving (Show, Eq, Generic, Read)

--type Arg = Expr