---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.AST.Expr where

import           Debug.Trace                    

import           Data.String.Utils              (join)
import qualified Flowbox.Luna.Codegen.Hs.Path as Path
--import qualified Flowbox.Luna.Codegen.Hs.GenState         as GenState
--import           Flowbox.Luna.Codegen.Hs.GenState           (GenState)
import qualified Flowbox.Luna.Codegen.Hs.AST.Constant as Constant

data Context = Pure | IO deriving (Show, Eq)

data Expr = Assignment { src   :: Expr    , dst :: Expr    , ctx :: Context }
          | Var        { name  :: String                                    }
          | VarRef     { vid   :: Int                                       } 
          | Tuple      { elems :: [Expr]                                    }
          | NTuple     { elems :: [Expr]                                    }
          | Type       { name  :: String  , params :: [String]              }
          | Call       { name  :: String  , args :: [Expr] , ctx :: Context }
          | StringLit  { val   :: String                                    }
          | Default    { val   :: String                                    }
          | THExprCtx  { name  :: String                                    }
          | THTypeCtx  { name  :: String                                    }
          | Cons       { name  :: String  , fields :: [Expr]                }
          | Typed      { src   :: Expr    , t :: Expr                       }
          | At         { name  :: String  , dst :: Expr                     }
          | Any        {                                                    }
          | Block      { body  :: [Expr]  , ctx :: Context                  }
          | BlockRet   { name  :: String  , ctx :: Context                  }
          | FuncType   { elems :: [Expr]                                    }
          | NOP        {                                                    }
          | Operator   { name  :: String  , src :: Expr    , dst :: Expr    }
          | Constant   { cval  :: Constant.Constant                         }
          deriving (Show)


mkExprCtx :: String -> String
mkExprCtx name' = "'"  ++ name'


mkTypeCtx :: String -> String
mkTypeCtx name' = "''"  ++ name'


empty :: Expr
empty = NOP

mpostfix :: String
mpostfix = "''M"


mkBlock :: String -> Expr
mkBlock retname = Block [BlockRet retname IO] Pure


genCode :: Expr -> String
genCode expr = case expr of
    Assignment src' dst' ctx'   -> genCode src' ++ " " ++ operator ++ " " ++ genCode dst' where
                                   operator = case ctx' of
                                       Pure -> "="
                                       IO   -> "<-"
    Var        name'            -> name'
    Default    val'             -> val'
    StringLit  val'             -> show val'
    VarRef     vid'             -> "v'" ++ show vid'
    Call       name' args' ctx' -> fname' ++ " " ++ join " " (map (("("++) . (++")") . genCode) args') where
                                   fname' = case ctx' of
                                       Pure -> name'
                                       IO   -> name' ++ mpostfix
    Tuple      elems'           -> if length elems' == 1
                                     then "OneTuple " ++ body
                                     else "(" ++ body ++ ")"
                                         where body = join ", " (map (genCode) elems')
    NTuple     elems'           -> "(" ++ join ", (" (map (genCode) elems') ++ ", ()" ++ replicate (length elems') ')'
    Type       name' params'    -> name' ++ (if null params' then "" else " " ++ join " " params')
    THExprCtx  name'            -> "'"  ++ name'
    THTypeCtx  name'            -> "''" ++ name'
    Cons       name' fields'    -> name' ++ " {" ++ join ", " (map genCode fields') ++ "}"
    Typed      src' t'          -> genCode src' ++ " :: " ++ genCode t'
    At         name' dst'       -> name' ++ "@" ++ genCode dst'
    Any                         -> "_"
    Block      body' ctx'       -> prefix ++ genBlockCode body' IO where
                                       prefix = case ctx' of
                                           Pure -> "\n"
                                           IO   -> "do\n"
    BlockRet   name' ctx'       -> case ctx' of
                                       Pure -> "in " ++ name'
                                       IO   -> "return " ++ name'
    FuncType   elems'           -> join " -> " (map genCode elems')
    NOP                         -> ""


genBlockCode :: [Expr] -> Context -> String
genBlockCode exprs' ctx' = case exprs' of
    []     -> ""
    x : xs -> prefix ++ indent ++ genCode x ++ "\n" ++ genBlockCode xs ectx where
        ectx = ctx x
        indent = case x of
            BlockRet{} -> Path.mkIndent 1
            _          -> case ectx of
                              Pure -> Path.mkIndent 2
                              _    -> Path.mkIndent 1
        prefix = if ctx' == IO && ectx == Pure
            then Path.mkIndent 1 ++ "let\n"
            else ""


addExpr :: Expr -> Expr -> Expr
addExpr nexpr base = base { body = nexpr : body base,
                            ctx  = chooseCtx (ctx base) (ctx nexpr)
                          }



chooseCtx :: Context -> Context -> Context
chooseCtx ctx1 ctx2 = case ctx2 of
    IO   -> IO
    Pure -> ctx1


mkAssignment :: Expr -> Expr -> Expr
mkAssignment src' dst' = Assignment src' dst' ctx' where
    ctx' = chooseCtx Pure (ctx dst')


mkPure :: Expr -> Expr
mkPure expr = case expr of
    Assignment src' dst' _   -> Assignment (mkPure src') (mkPure dst') Pure
    Tuple      elems'        -> Tuple $ map mkPure elems'
    Typed      src' t'       -> Typed (mkPure src') (mkPure t')
    Call       name' args' _ -> Call name' (map mkPure args') Pure
    Block      body' _       -> Block (map mkPure body') Pure
    BlockRet   name' ctx'    -> BlockRet name' Pure
    FuncType   elems'        -> FuncType $ map mkPure elems'
    other                    -> other


mkAlias :: (String, String) -> Expr
mkAlias (n1, n2) = Assignment (Var n1) (Var n2) Pure


mkCall :: String -> [String] -> Expr
mkCall name' args' = Call name' (map Var args') Pure