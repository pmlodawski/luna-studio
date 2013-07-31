---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Expr (
    Expr(..),
    Context(..),
    genCode
)where

import           Data.String.Utils                 (join)

data Context = Pure | IO deriving (Show, Eq)

data Expr = Assignment { src   :: Expr    , dst  :: Expr   , ctx :: Context }
          | Var        { name  :: String                                    }
          | VarRef     { vid   :: Int                                       } 
          | Tuple      { elems :: [Expr]                                    }
          | Call       { name  :: String  , args :: [Expr] , ctx :: Context }
          | Default    { val   :: String                                    }
          deriving (Show)


mpostfix :: String
mpostfix = "''M"


genCode :: Expr -> String
genCode expr = case expr of
    Assignment src dst ctx   -> genCode src ++ " " ++ operator ++ " " ++ genCode dst where
                                    operator = case ctx of
                                        Pure -> "="
                                        IO   -> "<-"
    Var        name          -> name
    Default    val           -> val
    VarRef     vid           -> "v'" ++ show vid
    Call       name args ctx -> name ++ suffix ++ " " ++ join " " (map (genCode) args) where
                                    suffix = case ctx of
                                        Pure -> ""
                                        IO   -> mpostfix
    Tuple      elems         -> if length elems == 1
                                   then "OneTuple " ++ body
                                   else "(" ++ body ++ ")"
                                where body = join ", " (map (genCode) elems)