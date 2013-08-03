---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Expr (
    Expr(..),
    Context(..),
    genCode,
    mkAlias,
    mkCall,
    mkPure
)where

import           Data.String.Utils                 (join)
--import qualified Luna.Codegen.Hs.GenState         as GenState
--import           Luna.Codegen.Hs.GenState           (GenState)

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
    Call       name args ctx -> fname ++ " " ++ join " " (map (genCode) args) where
                                    fname = case ctx of
                                        Pure -> name
                                        IO   -> name ++ mpostfix
    Tuple      elems         -> if length elems == 1
                                   then "OneTuple " ++ body
                                   else "(" ++ body ++ ")"
                                where body = join ", " (map (genCode) elems)


mkPure expr = case expr of
    Assignment src dst ctx   -> Assignment (mkPure src) (mkPure dst) Pure
    Tuple      elems         -> Tuple $ map mkPure elems
    Call       name args ctx -> Call name (map mkPure args) Pure
    other                    -> other

mkAlias :: (String, String) -> Expr
mkAlias (n1, n2) = Assignment (Var n1) (Var n2) Pure


mkCall :: String -> [String] -> Expr
mkCall name args = Call name (map Var args) Pure