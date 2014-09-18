module Luna.Typechecker.Assumptions (
    Assump(..), find
  ) where

import           Luna.Typechecker.AST.Scheme       (Scheme(..))

import           Luna.Typechecker.Substitutions    (Types(..))

import           Luna.Typechecker.AST.TID          (TID)

import           Text.Printf                                (printf)
import           Data.List                                  (intercalate)

import Control.DeepSeq

data Assump = TID :>: Scheme
            deriving (Eq)

instance NFData Assump where
  rnf (tid :>: sch) = rnf tid `seq` rnf sch

instance Show Assump where
  show (t :>: sch) = printf "%s :: %s" (show t) (show sch)
  showList as s = printf "%s%s" s (intercalate "\n" $ map show as)

instance Types Assump where
  apply s (i :>: sc) = i :>: apply s sc
  tv (_ :>: sc) = tv sc

find :: Monad m => TID -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as) = if i==i'
                              then return sc
                              else find i as
