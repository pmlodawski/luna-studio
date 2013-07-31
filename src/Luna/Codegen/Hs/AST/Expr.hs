---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Expr (
    Expr(..),
    genCode
)where

import           Data.String.Utils                 (join)


data Expr = Assignment { src   :: Expr    , dst  :: Expr   }
          | Var        { name  :: String                   }
          | VarRef     { vid   :: Int                      } 
          | Tuple      { elems :: [Expr]                   }
          | Call       { name  :: String  , args :: [Expr] }
          | Default    { val   :: String                   }
          deriving (Show)


genCode :: Expr -> String
genCode expr = case expr of
    Assignment src dst   -> genCode src ++ " = " ++ genCode dst
    Var        name      -> name
    Default    val       -> val
    VarRef     vid       -> "v'" ++ show vid
    Call       name args -> name ++ " " ++ join " " (map genCode args)
    Tuple      elems     -> if length elems == 1
                               then "OneTuple " ++ body
                               else "(" ++ body ++ ")"
                            where body = join ", " (map genCode elems)