{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Typechecker.Internal.AST.Lit (Lit(..),tiLit) where

import qualified Luna.Typechecker.Internal.AST.Type         as Ty

import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl

import           Luna.Typechecker.Internal.AST.Common       (ID)
import           Text.Printf                                (printf)

data Lit = Char    { _id :: ID, _char :: Prelude.Char   }
         | Float   { _id :: ID, _str  :: Prelude.String }
         | Integer { _id :: ID, _str  :: Prelude.String }
         | String  { _id :: ID, _str  :: Prelude.String }
         deriving (Eq)


tiLit :: Lit -> TIM.TI ([Tcl.Pred], Ty.Type)
tiLit (Char    _ _) = return ( [], Ty.tChar )
tiLit (Float   _ _) = return ( [], Ty.tFloat ) -- 'tFloat' of 'Fractional'?
tiLit (Integer _ _) = return ( [], Ty.tInteger )
--tiLit (Integer _ _) = do v <- newTVar Knd.Star
--                         return ([IsIn "Num" v], v)
tiLit (String  _ _) = return ( [], Ty.tString )


instance Show Lit where
  show (Char    _ c) = printf "'%c'" c
  show (Float   _ f) = case reads f of
                         (fl::Float, _):_ -> printf "%.3f" fl
                         _                -> "{badparse}"
  show (Integer _ i) = case reads i of
                         (ii::Int, _):_ -> printf "%d" ii
                         _                -> "{badparse}"
  show (String  _ s) = printf "\"%s\"" s