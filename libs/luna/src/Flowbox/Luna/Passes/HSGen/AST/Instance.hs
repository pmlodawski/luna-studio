---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.AST.Instance where

import           Flowbox.Prelude
--import           Debug.Trace                            

--import qualified Flowbox.Luna.Passes.HSGen.AST.Expr     as Expr
--import           Flowbox.Luna.Passes.HSGen.AST.Expr       (Expr)
--import           Data.String.Utils                      (join)
--import qualified Flowbox.Luna.Passes.HSGen.Path         as Path
--import qualified Flowbox.Luna.Passes.HSGen.AST.Function as Function
--import           Flowbox.Luna.Passes.HSGen.AST.Function   (Function)


--data Instance = Instance { name   :: String,
--                           params :: [Expr],
--                           funcs  :: [Function]
--                         } deriving (Show)


--empty :: Instance
--empty = Instance "" [] [] 


--genCode :: Instance -> String
--genCode inst = header ++ signature ++ " where\n" ++ body where
--    header    = "instance " ++ (name inst) ++ " "
--    signature = join " " $ map Expr.genCode (params inst)
--    body      = join "\n" $ map Function.genCode (funcs inst)

